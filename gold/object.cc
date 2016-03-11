// object.cc -- support for an object file for linking in gold

// Copyright 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
// Written by Ian Lance Taylor <iant@google.com>.

// This file is part of gold.

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
// MA 02110-1301, USA.

#include "gold.h"

#include <cerrno>
#include <cstring>
#include <cstdarg>
#include "demangle.h"
#include "libiberty.h"

#include "gc.h"
#include "target-select.h"
#include "dwarf_reader.h"
#include "layout.h"
#include "output.h"
#include "symtab.h"
#include "cref.h"
#include "reloc.h"
#include "object.h"
#include "dynobj.h"
#include "plugin.h"

namespace gold
{

// Class Xindex.

// Initialize the symtab_xindex_ array.  Find the SHT_SYMTAB_SHNDX
// section and read it in.  SYMTAB_SHNDX is the index of the symbol
// table we care about.

template<int size, bool big_endian>
void
Xindex::initialize_symtab_xindex(Object* object, unsigned int symtab_shndx)
{
  if (!this->symtab_xindex_.empty())
    return;

  gold_assert(symtab_shndx != 0);

  // Look through the sections in reverse order, on the theory that it
  // is more likely to be near the end than the beginning.
  unsigned int i = object->shnum();
  while (i > 0)
    {
      --i;
      if (object->section_type(i) == elfcpp::SHT_SYMTAB_SHNDX
	  && this->adjust_shndx(object->section_link(i)) == symtab_shndx)
	{
	  this->read_symtab_xindex<size, big_endian>(object, i, NULL);
	  return;
	}
    }

  object->error(_("missing SHT_SYMTAB_SHNDX section"));
}

// Read in the symtab_xindex_ array, given the section index of the
// SHT_SYMTAB_SHNDX section.  If PSHDRS is not NULL, it points at the
// section headers.

template<int size, bool big_endian>
void
Xindex::read_symtab_xindex(Object* object, unsigned int xindex_shndx,
			   const unsigned char* pshdrs)
{
  section_size_type bytecount;
  const unsigned char* contents;
  if (pshdrs == NULL)
    contents = object->section_contents(xindex_shndx, &bytecount, false);
  else
    {
      const unsigned char* p = (pshdrs
				+ (xindex_shndx
				   * elfcpp::Elf_sizes<size>::shdr_size));
      typename elfcpp::Shdr<size, big_endian> shdr(p);
      bytecount = convert_to_section_size_type(shdr.get_sh_size());
      contents = object->get_view(shdr.get_sh_offset(), bytecount, true, false);
    }

  gold_assert(this->symtab_xindex_.empty());
  this->symtab_xindex_.reserve(bytecount / 4);
  for (section_size_type i = 0; i < bytecount; i += 4)
    {
      unsigned int shndx = elfcpp::Swap<32, big_endian>::readval(contents + i);
      // We preadjust the section indexes we save.
      this->symtab_xindex_.push_back(this->adjust_shndx(shndx));
    }
}

// Symbol symndx has a section of SHN_XINDEX; return the real section
// index.

unsigned int
Xindex::sym_xindex_to_shndx(Object* object, unsigned int symndx)
{
  if (symndx >= this->symtab_xindex_.size())
    {
      object->error(_("symbol %u out of range for SHT_SYMTAB_SHNDX section"),
		    symndx);
      return elfcpp::SHN_UNDEF;
    }
  unsigned int shndx = this->symtab_xindex_[symndx];
  if (shndx < elfcpp::SHN_LORESERVE || shndx >= object->shnum())
    {
      object->error(_("extended index for symbol %u out of range: %u"),
		    symndx, shndx);
      return elfcpp::SHN_UNDEF;
    }
  return shndx;
}

// Class Object.

// Report an error for this object file.  This is used by the
// elfcpp::Elf_file interface, and also called by the Object code
// itself.

void
Object::error(const char* format, ...) const
{
  va_list args;
  va_start(args, format);
  char* buf = NULL;
  if (vasprintf(&buf, format, args) < 0)
    gold_nomem();
  va_end(args);
  gold_error(_("%s: %s"), this->name().c_str(), buf);
  free(buf);
}

// Return a view of the contents of a section.

const unsigned char*
Object::section_contents(unsigned int shndx, section_size_type* plen,
			 bool cache)
{
  Location loc(this->do_section_contents(shndx));
  *plen = convert_to_section_size_type(loc.data_size);
  if (*plen == 0)
    {
      static const unsigned char empty[1] = { '\0' };
      return empty;
    }
  return this->get_view(loc.file_offset, *plen, true, cache);
}

// Read the section data into SD.  This is code common to Sized_relobj
// and Sized_dynobj, so we put it into Object.

template<int size, bool big_endian>
void
Object::read_section_data(elfcpp::Elf_file<size, big_endian, Object>* elf_file,
			  Read_symbols_data* sd)
{
  const int shdr_size = elfcpp::Elf_sizes<size>::shdr_size;

  // Read the section headers.
  const off_t shoff = elf_file->shoff();
  const unsigned int shnum = this->shnum();
  sd->section_headers = this->get_lasting_view(shoff, shnum * shdr_size,
					       true, true);

  // Read the section names.
  const unsigned char* pshdrs = sd->section_headers->data();
  const unsigned char* pshdrnames = pshdrs + elf_file->shstrndx() * shdr_size;
  typename elfcpp::Shdr<size, big_endian> shdrnames(pshdrnames);

  if (shdrnames.get_sh_type() != elfcpp::SHT_STRTAB)
    this->error(_("section name section has wrong type: %u"),
		static_cast<unsigned int>(shdrnames.get_sh_type()));

  sd->section_names_size =
    convert_to_section_size_type(shdrnames.get_sh_size());
  sd->section_names = this->get_lasting_view(shdrnames.get_sh_offset(),
					     sd->section_names_size, false,
					     false);
}

// If NAME is the name of a special .gnu.warning section, arrange for
// the warning to be issued.  SHNDX is the section index.  Return
// whether it is a warning section.

bool
Object::handle_gnu_warning_section(const char* name, unsigned int shndx,
				   Symbol_table* symtab)
{
  const char warn_prefix[] = ".gnu.warning.";
  const int warn_prefix_len = sizeof warn_prefix - 1;
  if (strncmp(name, warn_prefix, warn_prefix_len) == 0)
    {
      // Read the section contents to get the warning text.  It would
      // be nicer if we only did this if we have to actually issue a
      // warning.  Unfortunately, warnings are issued as we relocate
      // sections.  That means that we can not lock the object then,
      // as we might try to issue the same warning multiple times
      // simultaneously.
      section_size_type len;
      const unsigned char* contents = this->section_contents(shndx, &len,
							     false);
      if (len == 0)
	{
	  const char* warning = name + warn_prefix_len;
	  contents = reinterpret_cast<const unsigned char*>(warning);
	  len = strlen(warning);
	}
      std::string warning(reinterpret_cast<const char*>(contents), len);
      symtab->add_warning(name + warn_prefix_len, this, warning);
      return true;
    }
  return false;
}

// If NAME is the name of the special section which indicates that
// this object was compiled with -fstack-split, mark it accordingly.

bool
Object::handle_split_stack_section(const char* name)
{
  if (strcmp(name, ".note.GNU-split-stack") == 0)
    {
      this->uses_split_stack_ = true;
      return true;
    }
  if (strcmp(name, ".note.GNU-no-split-stack") == 0)
    {
      this->has_no_split_stack_ = true;
      return true;
    }
  return false;
}

// Class Relobj

// To copy the symbols data read from the file to a local data structure.
// This function is called from do_layout only while doing garbage 
// collection.

void
Relobj::copy_symbols_data(Symbols_data* gc_sd, Read_symbols_data* sd, 
                          unsigned int section_header_size)
{
  gc_sd->section_headers_data = 
         new unsigned char[(section_header_size)];
  memcpy(gc_sd->section_headers_data, sd->section_headers->data(),
         section_header_size);
  gc_sd->section_names_data = 
         new unsigned char[sd->section_names_size];
  memcpy(gc_sd->section_names_data, sd->section_names->data(),
         sd->section_names_size);
  gc_sd->section_names_size = sd->section_names_size;
  if (sd->symbols != NULL)
    {
      gc_sd->symbols_data = 
             new unsigned char[sd->symbols_size];
      memcpy(gc_sd->symbols_data, sd->symbols->data(),
            sd->symbols_size);
    }
  else
    {
      gc_sd->symbols_data = NULL;
    }
  gc_sd->symbols_size = sd->symbols_size;
  gc_sd->external_symbols_offset = sd->external_symbols_offset;
  if (sd->symbol_names != NULL)
    {
      gc_sd->symbol_names_data =
             new unsigned char[sd->symbol_names_size];
      memcpy(gc_sd->symbol_names_data, sd->symbol_names->data(),
            sd->symbol_names_size);
    }
  else
    {
      gc_sd->symbol_names_data = NULL;
    }
  gc_sd->symbol_names_size = sd->symbol_names_size;
}

// This function determines if a particular section name must be included
// in the link.  This is used during garbage collection to determine the
// roots of the worklist.

bool
Relobj::is_section_name_included(const char* name)
{
  if (is_prefix_of(".ctors", name) 
      || is_prefix_of(".dtors", name) 
      || is_prefix_of(".note", name) 
      || is_prefix_of(".init", name) 
      || is_prefix_of(".fini", name) 
      || is_prefix_of(".gcc_except_table", name) 
      || is_prefix_of(".jcr", name) 
      || is_prefix_of(".preinit_array", name) 
      || (is_prefix_of(".text", name) 
          && strstr(name, "personality")) 
      || (is_prefix_of(".data", name) 
          &&  strstr(name, "personality")) 
      || (is_prefix_of(".gnu.linkonce.d", name) && 
            strstr(name, "personality")))
    {
      return true; 
    }
  return false;
}

// Class Sized_relobj.

template<int size, bool big_endian>
Sized_relobj<size, big_endian>::Sized_relobj(
    const std::string& name,
    Input_file* input_file,
    off_t offset,
    const elfcpp::Ehdr<size, big_endian>& ehdr)
  : Relobj(name, input_file, offset),
    elf_file_(this, ehdr),
    symtab_shndx_(-1U),
    local_symbol_count_(0),
    output_local_symbol_count_(0),
    output_local_dynsym_count_(0),
    symbols_(),
    defined_count_(0),
    local_symbol_offset_(0),
    local_dynsym_offset_(0),
    local_values_(),
    local_got_offsets_(),
    kept_comdat_sections_(),
    has_eh_frame_(false),
    discarded_eh_frame_shndx_(-1U)
{
}

template<int size, bool big_endian>
Sized_relobj<size, big_endian>::~Sized_relobj()
{
}

// Set up an object file based on the file header.  This sets up the
// section information.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_setup()
{
  const unsigned int shnum = this->elf_file_.shnum();
  this->set_shnum(shnum);
}

// Find the SHT_SYMTAB section, given the section headers.  The ELF
// standard says that maybe in the future there can be more than one
// SHT_SYMTAB section.  Until somebody figures out how that could
// work, we assume there is only one.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::find_symtab(const unsigned char* pshdrs)
{
  const unsigned int shnum = this->shnum();
  this->symtab_shndx_ = 0;
  if (shnum > 0)
    {
      // Look through the sections in reverse order, since gas tends
      // to put the symbol table at the end.
      const unsigned char* p = pshdrs + shnum * This::shdr_size;
      unsigned int i = shnum;
      unsigned int xindex_shndx = 0;
      unsigned int xindex_link = 0;
      while (i > 0)
	{
	  --i;
	  p -= This::shdr_size;
	  typename This::Shdr shdr(p);
	  if (shdr.get_sh_type() == elfcpp::SHT_SYMTAB)
	    {
	      this->symtab_shndx_ = i;
	      if (xindex_shndx > 0 && xindex_link == i)
		{
		  Xindex* xindex =
		    new Xindex(this->elf_file_.large_shndx_offset());
		  xindex->read_symtab_xindex<size, big_endian>(this,
							       xindex_shndx,
							       pshdrs);
		  this->set_xindex(xindex);
		}
	      break;
	    }

	  // Try to pick up the SHT_SYMTAB_SHNDX section, if there is
	  // one.  This will work if it follows the SHT_SYMTAB
	  // section.
	  if (shdr.get_sh_type() == elfcpp::SHT_SYMTAB_SHNDX)
	    {
	      xindex_shndx = i;
	      xindex_link = this->adjust_shndx(shdr.get_sh_link());
	    }
	}
    }
}

// Return the Xindex structure to use for object with lots of
// sections.

template<int size, bool big_endian>
Xindex*
Sized_relobj<size, big_endian>::do_initialize_xindex()
{
  gold_assert(this->symtab_shndx_ != -1U);
  Xindex* xindex = new Xindex(this->elf_file_.large_shndx_offset());
  xindex->initialize_symtab_xindex<size, big_endian>(this, this->symtab_shndx_);
  return xindex;
}

// Return whether SHDR has the right type and flags to be a GNU
// .eh_frame section.

template<int size, bool big_endian>
bool
Sized_relobj<size, big_endian>::check_eh_frame_flags(
    const elfcpp::Shdr<size, big_endian>* shdr) const
{
  return (shdr->get_sh_type() == elfcpp::SHT_PROGBITS
	  && (shdr->get_sh_flags() & elfcpp::SHF_ALLOC) != 0);
}

// Return whether there is a GNU .eh_frame section, given the section
// headers and the section names.

template<int size, bool big_endian>
bool
Sized_relobj<size, big_endian>::find_eh_frame(
    const unsigned char* pshdrs,
    const char* names,
    section_size_type names_size) const
{
  const unsigned int shnum = this->shnum();
  const unsigned char* p = pshdrs + This::shdr_size;
  for (unsigned int i = 1; i < shnum; ++i, p += This::shdr_size)
    {
      typename This::Shdr shdr(p);
      if (this->check_eh_frame_flags(&shdr))
	{
	  if (shdr.get_sh_name() >= names_size)
	    {
	      this->error(_("bad section name offset for section %u: %lu"),
			  i, static_cast<unsigned long>(shdr.get_sh_name()));
	      continue;
	    }

	  const char* name = names + shdr.get_sh_name();
	  if (strcmp(name, ".eh_frame") == 0)
	    return true;
	}
    }
  return false;
}

// Read the sections and symbols from an object file.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_read_symbols(Read_symbols_data* sd)
{
  this->read_section_data(&this->elf_file_, sd);

  const unsigned char* const pshdrs = sd->section_headers->data();

  this->find_symtab(pshdrs);

  const unsigned char* namesu = sd->section_names->data();
  const char* names = reinterpret_cast<const char*>(namesu);
  if (memmem(names, sd->section_names_size, ".eh_frame", 10) != NULL)
    {
      if (this->find_eh_frame(pshdrs, names, sd->section_names_size))
        this->has_eh_frame_ = true;
    }

  sd->symbols = NULL;
  sd->symbols_size = 0;
  sd->external_symbols_offset = 0;
  sd->symbol_names = NULL;
  sd->symbol_names_size = 0;

  if (this->symtab_shndx_ == 0)
    {
      // No symbol table.  Weird but legal.
      return;
    }

  // Get the symbol table section header.
  typename This::Shdr symtabshdr(pshdrs
				 + this->symtab_shndx_ * This::shdr_size);
  gold_assert(symtabshdr.get_sh_type() == elfcpp::SHT_SYMTAB);

  // If this object has a .eh_frame section, we need all the symbols.
  // Otherwise we only need the external symbols.  While it would be
  // simpler to just always read all the symbols, I've seen object
  // files with well over 2000 local symbols, which for a 64-bit
  // object file format is over 5 pages that we don't need to read
  // now.

  const int sym_size = This::sym_size;
  const unsigned int loccount = symtabshdr.get_sh_info();
  this->local_symbol_count_ = loccount;
  this->local_values_.resize(loccount);
  section_offset_type locsize = loccount * sym_size;
  off_t dataoff = symtabshdr.get_sh_offset();
  section_size_type datasize =
    convert_to_section_size_type(symtabshdr.get_sh_size());
  off_t extoff = dataoff + locsize;
  section_size_type extsize = datasize - locsize;

  off_t readoff = this->has_eh_frame_ ? dataoff : extoff;
  section_size_type readsize = this->has_eh_frame_ ? datasize : extsize;

  if (readsize == 0)
    {
      // No external symbols.  Also weird but also legal.
      return;
    }

  File_view* fvsymtab = this->get_lasting_view(readoff, readsize, true, false);

  // Read the section header for the symbol names.
  unsigned int strtab_shndx = this->adjust_shndx(symtabshdr.get_sh_link());
  if (strtab_shndx >= this->shnum())
    {
      this->error(_("invalid symbol table name index: %u"), strtab_shndx);
      return;
    }
  typename This::Shdr strtabshdr(pshdrs + strtab_shndx * This::shdr_size);
  if (strtabshdr.get_sh_type() != elfcpp::SHT_STRTAB)
    {
      this->error(_("symbol table name section has wrong type: %u"),
		  static_cast<unsigned int>(strtabshdr.get_sh_type()));
      return;
    }

  // Read the symbol names.
  File_view* fvstrtab = this->get_lasting_view(strtabshdr.get_sh_offset(),
					       strtabshdr.get_sh_size(),
					       false, true);

  sd->symbols = fvsymtab;
  sd->symbols_size = readsize;
  sd->external_symbols_offset = this->has_eh_frame_ ? locsize : 0;
  sd->symbol_names = fvstrtab;
  sd->symbol_names_size =
    convert_to_section_size_type(strtabshdr.get_sh_size());
}

// Return the section index of symbol SYM.  Set *VALUE to its value in
// the object file.  Set *IS_ORDINARY if this is an ordinary section
// index.  not a special cod between SHN_LORESERVE and SHN_HIRESERVE.
// Note that for a symbol which is not defined in this object file,
// this will set *VALUE to 0 and return SHN_UNDEF; it will not return
// the final value of the symbol in the link.

template<int size, bool big_endian>
unsigned int
Sized_relobj<size, big_endian>::symbol_section_and_value(unsigned int sym,
							 Address* value,
							 bool* is_ordinary)
{
  section_size_type symbols_size;
  const unsigned char* symbols = this->section_contents(this->symtab_shndx_,
							&symbols_size,
							false);

  const size_t count = symbols_size / This::sym_size;
  gold_assert(sym < count);

  elfcpp::Sym<size, big_endian> elfsym(symbols + sym * This::sym_size);
  *value = elfsym.get_st_value();

  return this->adjust_sym_shndx(sym, elfsym.get_st_shndx(), is_ordinary);
}

// Return whether to include a section group in the link.  LAYOUT is
// used to keep track of which section groups we have already seen.
// INDEX is the index of the section group and SHDR is the section
// header.  If we do not want to include this group, we set bits in
// OMIT for each section which should be discarded.

template<int size, bool big_endian>
bool
Sized_relobj<size, big_endian>::include_section_group(
    Symbol_table* symtab,
    Layout* layout,
    unsigned int index,
    const char* name,
    const unsigned char* shdrs,
    const char* section_names,
    section_size_type section_names_size,
    std::vector<bool>* omit)
{
  // Read the section contents.
  typename This::Shdr shdr(shdrs + index * This::shdr_size);
  const unsigned char* pcon = this->get_view(shdr.get_sh_offset(),
					     shdr.get_sh_size(), true, false);
  const elfcpp::Elf_Word* pword =
    reinterpret_cast<const elfcpp::Elf_Word*>(pcon);

  // The first word contains flags.  We only care about COMDAT section
  // groups.  Other section groups are always included in the link
  // just like ordinary sections.
  elfcpp::Elf_Word flags = elfcpp::Swap<32, big_endian>::readval(pword);

  // Look up the group signature, which is the name of a symbol.  This
  // is a lot of effort to go to to read a string.  Why didn't they
  // just have the group signature point into the string table, rather
  // than indirect through a symbol?

  // Get the appropriate symbol table header (this will normally be
  // the single SHT_SYMTAB section, but in principle it need not be).
  const unsigned int link = this->adjust_shndx(shdr.get_sh_link());
  typename This::Shdr symshdr(this, this->elf_file_.section_header(link));

  // Read the symbol table entry.
  unsigned int symndx = shdr.get_sh_info();
  if (symndx >= symshdr.get_sh_size() / This::sym_size)
    {
      this->error(_("section group %u info %u out of range"),
		  index, symndx);
      return false;
    }
  off_t symoff = symshdr.get_sh_offset() + symndx * This::sym_size;
  const unsigned char* psym = this->get_view(symoff, This::sym_size, true,
					     false);
  elfcpp::Sym<size, big_endian> sym(psym);

  // Read the symbol table names.
  section_size_type symnamelen;
  const unsigned char* psymnamesu;
  psymnamesu = this->section_contents(this->adjust_shndx(symshdr.get_sh_link()),
				      &symnamelen, true);
  const char* psymnames = reinterpret_cast<const char*>(psymnamesu);

  // Get the section group signature.
  if (sym.get_st_name() >= symnamelen)
    {
      this->error(_("symbol %u name offset %u out of range"),
		  symndx, sym.get_st_name());
      return false;
    }

  std::string signature(psymnames + sym.get_st_name());

  // It seems that some versions of gas will create a section group
  // associated with a section symbol, and then fail to give a name to
  // the section symbol.  In such a case, use the name of the section.
  if (signature[0] == '\0' && sym.get_st_type() == elfcpp::STT_SECTION)
    {
      bool is_ordinary;
      unsigned int sym_shndx = this->adjust_sym_shndx(symndx,
						      sym.get_st_shndx(),
						      &is_ordinary);
      if (!is_ordinary || sym_shndx >= this->shnum())
	{
	  this->error(_("symbol %u invalid section index %u"),
		      symndx, sym_shndx);
	  return false;
	}
      typename This::Shdr member_shdr(shdrs + sym_shndx * This::shdr_size);
      if (member_shdr.get_sh_name() < section_names_size)
        signature = section_names + member_shdr.get_sh_name();
    }

  // Record this section group in the layout, and see whether we've already
  // seen one with the same signature.
  bool include_group;
  bool is_comdat;
  Kept_section* kept_section = NULL;

  if ((flags & elfcpp::GRP_COMDAT) == 0)
    {
      include_group = true;
      is_comdat = false;
    }
  else
    {
      include_group = layout->find_or_add_kept_section(signature,
						       this, index, true,
						       true, &kept_section);
      is_comdat = true;
    }

  size_t count = shdr.get_sh_size() / sizeof(elfcpp::Elf_Word);

  std::vector<unsigned int> shndxes;
  bool relocate_group = include_group && parameters->options().relocatable();
  if (relocate_group)
    shndxes.reserve(count - 1);

  for (size_t i = 1; i < count; ++i)
    {
      elfcpp::Elf_Word shndx =
	this->adjust_shndx(elfcpp::Swap<32, big_endian>::readval(pword + i));

      if (relocate_group)
	shndxes.push_back(shndx);

      if (shndx >= this->shnum())
	{
	  this->error(_("section %u in section group %u out of range"),
		      shndx, index);
	  continue;
	}

      // Check for an earlier section number, since we're going to get
      // it wrong--we may have already decided to include the section.
      if (shndx < index)
        this->error(_("invalid section group %u refers to earlier section %u"),
                    index, shndx);

      // Get the name of the member section.
      typename This::Shdr member_shdr(shdrs + shndx * This::shdr_size);
      if (member_shdr.get_sh_name() >= section_names_size)
        {
          // This is an error, but it will be diagnosed eventually
          // in do_layout, so we don't need to do anything here but
          // ignore it.
          continue;
        }
      std::string mname(section_names + member_shdr.get_sh_name());

      if (include_group)
	{
	  if (is_comdat)
	    kept_section->add_comdat_section(mname, shndx,
					     member_shdr.get_sh_size());
	}
      else
        {
          (*omit)[shndx] = true;

	  if (is_comdat)
            {
	      Relobj* kept_object = kept_section->object();
	      if (kept_section->is_comdat())
		{
		  // Find the corresponding kept section, and store
		  // that info in the discarded section table.
		  unsigned int kept_shndx;
		  uint64_t kept_size;
		  if (kept_section->find_comdat_section(mname, &kept_shndx,
							&kept_size))
		    {
		      // We don't keep a mapping for this section if
		      // it has a different size.  The mapping is only
		      // used for relocation processing, and we don't
		      // want to treat the sections as similar if the
		      // sizes are different.  Checking the section
		      // size is the approach used by the GNU linker.
		      if (kept_size == member_shdr.get_sh_size())
			this->set_kept_comdat_section(shndx, kept_object,
						      kept_shndx);
		    }
		}
	      else
		{
		  // The existing section is a linkonce section.  Add
		  // a mapping if there is exactly one section in the
		  // group (which is true when COUNT == 2) and if it
		  // is the same size.
		  if (count == 2
		      && (kept_section->linkonce_size()
			  == member_shdr.get_sh_size()))
		    this->set_kept_comdat_section(shndx, kept_object,
						  kept_section->shndx());
		}
            }
        }
    }

  if (relocate_group)
    layout->layout_group(symtab, this, index, name, signature.c_str(),
			 shdr, flags, &shndxes);

  return include_group;
}

// Whether to include a linkonce section in the link.  NAME is the
// name of the section and SHDR is the section header.

// Linkonce sections are a GNU extension implemented in the original
// GNU linker before section groups were defined.  The semantics are
// that we only include one linkonce section with a given name.  The
// name of a linkonce section is normally .gnu.linkonce.T.SYMNAME,
// where T is the type of section and SYMNAME is the name of a symbol.
// In an attempt to make linkonce sections interact well with section
// groups, we try to identify SYMNAME and use it like a section group
// signature.  We want to block section groups with that signature,
// but not other linkonce sections with that signature.  We also use
// the full name of the linkonce section as a normal section group
// signature.

template<int size, bool big_endian>
bool
Sized_relobj<size, big_endian>::include_linkonce_section(
    Layout* layout,
    unsigned int index,
    const char* name,
    const elfcpp::Shdr<size, big_endian>& shdr)
{
  typename elfcpp::Elf_types<size>::Elf_WXword sh_size = shdr.get_sh_size();
  // In general the symbol name we want will be the string following
  // the last '.'.  However, we have to handle the case of
  // .gnu.linkonce.t.__i686.get_pc_thunk.bx, which was generated by
  // some versions of gcc.  So we use a heuristic: if the name starts
  // with ".gnu.linkonce.t.", we use everything after that.  Otherwise
  // we look for the last '.'.  We can't always simply skip
  // ".gnu.linkonce.X", because we have to deal with cases like
  // ".gnu.linkonce.d.rel.ro.local".
  const char* const linkonce_t = ".gnu.linkonce.t.";
  const char* symname;
  if (strncmp(name, linkonce_t, strlen(linkonce_t)) == 0)
    symname = name + strlen(linkonce_t);
  else
    symname = strrchr(name, '.') + 1;
  std::string sig1(symname);
  std::string sig2(name);
  Kept_section* kept1;
  Kept_section* kept2;
  bool include1 = layout->find_or_add_kept_section(sig1, this, index, false,
						   false, &kept1);
  bool include2 = layout->find_or_add_kept_section(sig2, this, index, false,
						   true, &kept2);

  if (!include2)
    {
      // We are not including this section because we already saw the
      // name of the section as a signature.  This normally implies
      // that the kept section is another linkonce section.  If it is
      // the same size, record it as the section which corresponds to
      // this one.
      if (kept2->object() != NULL
	  && !kept2->is_comdat()
	  && kept2->linkonce_size() == sh_size)
	this->set_kept_comdat_section(index, kept2->object(), kept2->shndx());
    }
  else if (!include1)
    {
      // The section is being discarded on the basis of its symbol
      // name.  This means that the corresponding kept section was
      // part of a comdat group, and it will be difficult to identify
      // the specific section within that group that corresponds to
      // this linkonce section.  We'll handle the simple case where
      // the group has only one member section.  Otherwise, it's not
      // worth the effort.
      unsigned int kept_shndx;
      uint64_t kept_size;
      if (kept1->object() != NULL
	  && kept1->is_comdat()
	  && kept1->find_single_comdat_section(&kept_shndx, &kept_size)
	  && kept_size == sh_size)
	this->set_kept_comdat_section(index, kept1->object(), kept_shndx);
    }
  else
    {
      kept1->set_linkonce_size(sh_size);
      kept2->set_linkonce_size(sh_size);
    }

  return include1 && include2;
}

// Layout an input section.

template<int size, bool big_endian>
inline void
Sized_relobj<size, big_endian>::layout_section(Layout* layout,
                                               unsigned int shndx,
                                               const char* name,
                                               typename This::Shdr& shdr,
                                               unsigned int reloc_shndx,
                                               unsigned int reloc_type)
{
  off_t offset;
  Output_section* os = layout->layout(this, shndx, name, shdr,
					  reloc_shndx, reloc_type, &offset);

  this->output_sections()[shndx] = os;
  if (offset == -1)
    this->section_offsets_[shndx] = invalid_address;
  else
    this->section_offsets_[shndx] = convert_types<Address, off_t>(offset);

  // If this section requires special handling, and if there are
  // relocs that apply to it, then we must do the special handling
  // before we apply the relocs.
  if (offset == -1 && reloc_shndx != 0)
    this->set_relocs_must_follow_section_writes();
}

// Lay out the input sections.  We walk through the sections and check
// whether they should be included in the link.  If they should, we
// pass them to the Layout object, which will return an output section
// and an offset.  
// During garbage collection (--gc-sections) and identical code folding 
// (--icf), this function is called twice.  When it is called the first 
// time, it is for setting up some sections as roots to a work-list for
// --gc-sections and to do comdat processing.  Actual layout happens the 
// second time around after all the relevant sections have been determined.  
// The first time, is_worklist_ready or is_icf_ready is false. It is then 
// set to true after the garbage collection worklist or identical code 
// folding is processed and the relevant sections to be kept are 
// determined.  Then, this function is called again to layout the sections.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_layout(Symbol_table* symtab,
					  Layout* layout,
					  Read_symbols_data* sd)
{
  const unsigned int shnum = this->shnum();
  bool is_gc_pass_one = ((parameters->options().gc_sections() 
                          && !symtab->gc()->is_worklist_ready())
                         || (parameters->options().icf_enabled()
                             && !symtab->icf()->is_icf_ready()));
 
  bool is_gc_pass_two = ((parameters->options().gc_sections() 
                          && symtab->gc()->is_worklist_ready())
                         || (parameters->options().icf_enabled()
                             && symtab->icf()->is_icf_ready()));

  bool is_gc_or_icf = (parameters->options().gc_sections()
                       || parameters->options().icf_enabled()); 

  // Both is_gc_pass_one and is_gc_pass_two should not be true.
  gold_assert(!(is_gc_pass_one  && is_gc_pass_two));

  if (shnum == 0)
    return;
  Symbols_data* gc_sd = NULL;
  if (is_gc_pass_one)
    {
      // During garbage collection save the symbols data to use it when 
      // re-entering this function.   
      gc_sd = new Symbols_data;
      this->copy_symbols_data(gc_sd, sd, This::shdr_size * shnum);
      this->set_symbols_data(gc_sd);
    }
  else if (is_gc_pass_two)
    {
      gc_sd = this->get_symbols_data();
    }

  const unsigned char* section_headers_data = NULL;
  section_size_type section_names_size;
  const unsigned char* symbols_data = NULL;
  section_size_type symbols_size;
  section_offset_type external_symbols_offset;
  const unsigned char* symbol_names_data = NULL;
  section_size_type symbol_names_size;
 
  if (is_gc_or_icf)
    {
      section_headers_data = gc_sd->section_headers_data;
      section_names_size = gc_sd->section_names_size;
      symbols_data = gc_sd->symbols_data;
      symbols_size = gc_sd->symbols_size;
      external_symbols_offset = gc_sd->external_symbols_offset;
      symbol_names_data = gc_sd->symbol_names_data;
      symbol_names_size = gc_sd->symbol_names_size;
    }
  else
    {
      section_headers_data = sd->section_headers->data();
      section_names_size = sd->section_names_size;
      if (sd->symbols != NULL)
        symbols_data = sd->symbols->data();
      symbols_size = sd->symbols_size;
      external_symbols_offset = sd->external_symbols_offset;
      if (sd->symbol_names != NULL)
        symbol_names_data = sd->symbol_names->data();
      symbol_names_size = sd->symbol_names_size;
    }

  // Get the section headers.
  const unsigned char* shdrs = section_headers_data;
  const unsigned char* pshdrs;

  // Get the section names.
  const unsigned char* pnamesu = (is_gc_or_icf) 
                                 ? gc_sd->section_names_data
                                 : sd->section_names->data();

  const char* pnames = reinterpret_cast<const char*>(pnamesu);

  // If any input files have been claimed by plugins, we need to defer
  // actual layout until the replacement files have arrived.
  const bool should_defer_layout =
      (parameters->options().has_plugins()
       && parameters->options().plugins()->should_defer_layout());
  unsigned int num_sections_to_defer = 0;

  // For each section, record the index of the reloc section if any.
  // Use 0 to mean that there is no reloc section, -1U to mean that
  // there is more than one.
  std::vector<unsigned int> reloc_shndx(shnum, 0);
  std::vector<unsigned int> reloc_type(shnum, elfcpp::SHT_NULL);
  // Skip the first, dummy, section.
  pshdrs = shdrs + This::shdr_size;
  for (unsigned int i = 1; i < shnum; ++i, pshdrs += This::shdr_size)
    {
      typename This::Shdr shdr(pshdrs);

      // Count the number of sections whose layout will be deferred.
      if (should_defer_layout && (shdr.get_sh_flags() & elfcpp::SHF_ALLOC))
        ++num_sections_to_defer;

      unsigned int sh_type = shdr.get_sh_type();
      if (sh_type == elfcpp::SHT_REL || sh_type == elfcpp::SHT_RELA)
	{
	  unsigned int target_shndx = this->adjust_shndx(shdr.get_sh_info());
	  if (target_shndx == 0 || target_shndx >= shnum)
	    {
	      this->error(_("relocation section %u has bad info %u"),
			  i, target_shndx);
	      continue;
	    }

	  if (reloc_shndx[target_shndx] != 0)
	    reloc_shndx[target_shndx] = -1U;
	  else
	    {
	      reloc_shndx[target_shndx] = i;
	      reloc_type[target_shndx] = sh_type;
	    }
	}
    }

  Output_sections& out_sections(this->output_sections());
  std::vector<Address>& out_section_offsets(this->section_offsets_);

  if (!is_gc_pass_two)
    {
      out_sections.resize(shnum);
      out_section_offsets.resize(shnum);
    }

  // If we are only linking for symbols, then there is nothing else to
  // do here.
  if (this->input_file()->just_symbols())
    {
      if (!is_gc_pass_two)
        {
          delete sd->section_headers;
          sd->section_headers = NULL;
          delete sd->section_names;
          sd->section_names = NULL;
        }
      return;
    }

  if (num_sections_to_defer > 0)
    {
      parameters->options().plugins()->add_deferred_layout_object(this);
      this->deferred_layout_.reserve(num_sections_to_defer);
    }

  // Whether we've seen a .note.GNU-stack section.
  bool seen_gnu_stack = false;
  // The flags of a .note.GNU-stack section.
  uint64_t gnu_stack_flags = 0;

  // Keep track of which sections to omit.
  std::vector<bool> omit(shnum, false);

  // Keep track of reloc sections when emitting relocations.
  const bool relocatable = parameters->options().relocatable();
  const bool emit_relocs = (relocatable
			    || parameters->options().emit_relocs());
  std::vector<unsigned int> reloc_sections;

  // Keep track of .eh_frame sections.
  std::vector<unsigned int> eh_frame_sections;

  // Skip the first, dummy, section.
  pshdrs = shdrs + This::shdr_size;
  for (unsigned int i = 1; i < shnum; ++i, pshdrs += This::shdr_size)
    {
      typename This::Shdr shdr(pshdrs);

      if (shdr.get_sh_name() >= section_names_size)
	{
	  this->error(_("bad section name offset for section %u: %lu"),
		      i, static_cast<unsigned long>(shdr.get_sh_name()));
	  return;
	}

      const char* name = pnames + shdr.get_sh_name();

      if (!is_gc_pass_two)
        { 
          if (this->handle_gnu_warning_section(name, i, symtab))
            { 
    	      if (!relocatable)
	        omit[i] = true;
	    }

          // The .note.GNU-stack section is special.  It gives the
          // protection flags that this object file requires for the stack
          // in memory.
          if (strcmp(name, ".note.GNU-stack") == 0)
            {
	      seen_gnu_stack = true;
	      gnu_stack_flags |= shdr.get_sh_flags();
	      omit[i] = true;
            }

	  // The .note.GNU-split-stack section is also special.  It
	  // indicates that the object was compiled with
	  // -fsplit-stack.
	  if (this->handle_split_stack_section(name))
	    {
	      if (!parameters->options().relocatable()
		  && !parameters->options().shared())
		omit[i] = true;
	    }

          bool discard = omit[i];
          if (!discard)
            {
	      if (shdr.get_sh_type() == elfcpp::SHT_GROUP)
	        {
	          if (!this->include_section_group(symtab, layout, i, name, 
                                                   shdrs, pnames, 
                                                   section_names_size,
					           &omit))
		    discard = true;
	        }
              else if ((shdr.get_sh_flags() & elfcpp::SHF_GROUP) == 0
                       && Layout::is_linkonce(name))
	        {
	          if (!this->include_linkonce_section(layout, i, name, shdr))
   		    discard = true;
	        }
	    }

          if (discard)
            {
	      // Do not include this section in the link.
	      out_sections[i] = NULL;
              out_section_offsets[i] = invalid_address;
	      continue;
            }
        }
 
      if (is_gc_pass_one && parameters->options().gc_sections())
        {
          if (is_section_name_included(name)
              || shdr.get_sh_type() == elfcpp::SHT_INIT_ARRAY 
              || shdr.get_sh_type() == elfcpp::SHT_FINI_ARRAY)
            {
              symtab->gc()->worklist().push(Section_id(this, i)); 
            }
        }

      // When doing a relocatable link we are going to copy input
      // reloc sections into the output.  We only want to copy the
      // ones associated with sections which are not being discarded.
      // However, we don't know that yet for all sections.  So save
      // reloc sections and process them later. Garbage collection is
      // not triggered when relocatable code is desired.
      if (emit_relocs
	  && (shdr.get_sh_type() == elfcpp::SHT_REL
	      || shdr.get_sh_type() == elfcpp::SHT_RELA))
	{
	  reloc_sections.push_back(i);
	  continue;
	}

      if (relocatable && shdr.get_sh_type() == elfcpp::SHT_GROUP)
	continue;

      // The .eh_frame section is special.  It holds exception frame
      // information that we need to read in order to generate the
      // exception frame header.  We process these after all the other
      // sections so that the exception frame reader can reliably
      // determine which sections are being discarded, and discard the
      // corresponding information.
      if (!relocatable
          && strcmp(name, ".eh_frame") == 0
          && this->check_eh_frame_flags(&shdr))
        {
          if (is_gc_pass_one)
            {
              out_sections[i] = reinterpret_cast<Output_section*>(1);
              out_section_offsets[i] = invalid_address;
            }
          else
            eh_frame_sections.push_back(i);
          continue;
        }

      if (is_gc_pass_two && parameters->options().gc_sections())
        {
          // This is executed during the second pass of garbage 
          // collection. do_layout has been called before and some 
          // sections have been already discarded. Simply ignore 
          // such sections this time around.
          if (out_sections[i] == NULL)
            {
              gold_assert(out_section_offsets[i] == invalid_address);
              continue; 
            }
          if (((shdr.get_sh_flags() & elfcpp::SHF_ALLOC) != 0)
              && symtab->gc()->is_section_garbage(this, i))
              {
                if (parameters->options().print_gc_sections())
                  gold_info(_("%s: removing unused section from '%s'" 
                              " in file '%s'"),
                            program_name, this->section_name(i).c_str(), 
                            this->name().c_str());
                out_sections[i] = NULL;
                out_section_offsets[i] = invalid_address;
                continue;
              }
        }

      if (is_gc_pass_two && parameters->options().icf_enabled())
        {
          if (out_sections[i] == NULL)
            {
              gold_assert(out_section_offsets[i] == invalid_address);
              continue;
            }
          if (((shdr.get_sh_flags() & elfcpp::SHF_ALLOC) != 0)
              && symtab->icf()->is_section_folded(this, i))
              {
                if (parameters->options().print_icf_sections())
                  {
                    Section_id folded =
                                symtab->icf()->get_folded_section(this, i);
                    Relobj* folded_obj =
                                reinterpret_cast<Relobj*>(folded.first);
                    gold_info(_("%s: ICF folding section '%s' in file '%s'"
                                "into '%s' in file '%s'"),
                              program_name, this->section_name(i).c_str(),
                              this->name().c_str(),
                              folded_obj->section_name(folded.second).c_str(),
                              folded_obj->name().c_str());
                  }
                out_sections[i] = NULL;
                out_section_offsets[i] = invalid_address;
                continue;
              }
        }

      // Defer layout here if input files are claimed by plugins.  When gc
      // is turned on this function is called twice.  For the second call
      // should_defer_layout should be false.
      if (should_defer_layout && (shdr.get_sh_flags() & elfcpp::SHF_ALLOC))
        {
          gold_assert(!is_gc_pass_two);
          this->deferred_layout_.push_back(Deferred_layout(i, name, 
                                                           pshdrs,
                                                           reloc_shndx[i],
                                                           reloc_type[i]));
          // Put dummy values here; real values will be supplied by
          // do_layout_deferred_sections.
          out_sections[i] = reinterpret_cast<Output_section*>(2);
          out_section_offsets[i] = invalid_address;
          continue;
        }

      // During gc_pass_two if a section that was previously deferred is
      // found, do not layout the section as layout_deferred_sections will
      // do it later from gold.cc.
      if (is_gc_pass_two 
          && (out_sections[i] == reinterpret_cast<Output_section*>(2)))
        continue;

      if (is_gc_pass_one)
        {
          // This is during garbage collection. The out_sections are 
          // assigned in the second call to this function. 
          out_sections[i] = reinterpret_cast<Output_section*>(1);
          out_section_offsets[i] = invalid_address;
        }
      else
        {
          // When garbage collection is switched on the actual layout
          // only happens in the second call.
          this->layout_section(layout, i, name, shdr, reloc_shndx[i],
                               reloc_type[i]);
        }
    }

  if (!is_gc_pass_one)
    layout->layout_gnu_stack(seen_gnu_stack, gnu_stack_flags);

  // When doing a relocatable link handle the reloc sections at the
  // end.  Garbage collection  and Identical Code Folding is not 
  // turned on for relocatable code. 
  if (emit_relocs)
    this->size_relocatable_relocs();

  gold_assert(!(is_gc_or_icf) || reloc_sections.empty());

  for (std::vector<unsigned int>::const_iterator p = reloc_sections.begin();
       p != reloc_sections.end();
       ++p)
    {
      unsigned int i = *p;
      const unsigned char* pshdr;
      pshdr = section_headers_data + i * This::shdr_size;
      typename This::Shdr shdr(pshdr);

      unsigned int data_shndx = this->adjust_shndx(shdr.get_sh_info());
      if (data_shndx >= shnum)
	{
	  // We already warned about this above.
	  continue;
	}

      Output_section* data_section = out_sections[data_shndx];
      if (data_section == NULL)
	{
	  out_sections[i] = NULL;
          out_section_offsets[i] = invalid_address;
	  continue;
	}

      Relocatable_relocs* rr = new Relocatable_relocs();
      this->set_relocatable_relocs(i, rr);

      Output_section* os = layout->layout_reloc(this, i, shdr, data_section,
						rr);
      out_sections[i] = os;
      out_section_offsets[i] = invalid_address;
    }

  // Handle the .eh_frame sections at the end.
  gold_assert(!is_gc_pass_one || eh_frame_sections.empty());
  for (std::vector<unsigned int>::const_iterator p = eh_frame_sections.begin();
       p != eh_frame_sections.end();
       ++p)
    {
      gold_assert(this->has_eh_frame_);
      gold_assert(external_symbols_offset != 0);

      unsigned int i = *p;
      const unsigned char *pshdr;
      pshdr = section_headers_data + i * This::shdr_size;
      typename This::Shdr shdr(pshdr);

      off_t offset;
      Output_section* os = layout->layout_eh_frame(this,
						   symbols_data,
						   symbols_size,
						   symbol_names_data,
						   symbol_names_size,
						   i, shdr,
						   reloc_shndx[i],
						   reloc_type[i],
						   &offset);
      out_sections[i] = os;
      if (offset == -1)
	{
	  // An object can contain at most one section holding exception
	  // frame information.
	  gold_assert(this->discarded_eh_frame_shndx_ == -1U);
	  this->discarded_eh_frame_shndx_ = i;
	  out_section_offsets[i] = invalid_address;
	}
      else
        out_section_offsets[i] = convert_types<Address, off_t>(offset);

      // If this section requires special handling, and if there are
      // relocs that apply to it, then we must do the special handling
      // before we apply the relocs.
      if (offset == -1 && reloc_shndx[i] != 0)
	this->set_relocs_must_follow_section_writes();
    }

  if (is_gc_pass_two)
    {
      delete[] gc_sd->section_headers_data;
      delete[] gc_sd->section_names_data;
      delete[] gc_sd->symbols_data;
      delete[] gc_sd->symbol_names_data;
      this->set_symbols_data(NULL);
    }
  else
    {
      delete sd->section_headers;
      sd->section_headers = NULL;
      delete sd->section_names;
      sd->section_names = NULL;
    }
}

// Layout sections whose layout was deferred while waiting for
// input files from a plugin.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_layout_deferred_sections(Layout* layout)
{
  typename std::vector<Deferred_layout>::iterator deferred;

  for (deferred = this->deferred_layout_.begin();
       deferred != this->deferred_layout_.end();
       ++deferred)
    {
      typename This::Shdr shdr(deferred->shdr_data_);
      this->layout_section(layout, deferred->shndx_, deferred->name_.c_str(),
                           shdr, deferred->reloc_shndx_, deferred->reloc_type_);
    }

  this->deferred_layout_.clear();
}

// Add the symbols to the symbol table.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_add_symbols(Symbol_table* symtab,
					       Read_symbols_data* sd,
					       Layout*)
{
  if (sd->symbols == NULL)
    {
      gold_assert(sd->symbol_names == NULL);
      return;
    }

  const int sym_size = This::sym_size;
  size_t symcount = ((sd->symbols_size - sd->external_symbols_offset)
		     / sym_size);
  if (symcount * sym_size != sd->symbols_size - sd->external_symbols_offset)
    {
      this->error(_("size of symbols is not multiple of symbol size"));
      return;
    }

  this->symbols_.resize(symcount);

  const char* sym_names =
    reinterpret_cast<const char*>(sd->symbol_names->data());
  symtab->add_from_relobj(this,
			  sd->symbols->data() + sd->external_symbols_offset,
			  symcount, this->local_symbol_count_,
			  sym_names, sd->symbol_names_size,
			  &this->symbols_,
			  &this->defined_count_);

  delete sd->symbols;
  sd->symbols = NULL;
  delete sd->symbol_names;
  sd->symbol_names = NULL;
}

// First pass over the local symbols.  Here we add their names to
// *POOL and *DYNPOOL, and we store the symbol value in
// THIS->LOCAL_VALUES_.  This function is always called from a
// singleton thread.  This is followed by a call to
// finalize_local_symbols.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_count_local_symbols(Stringpool* pool,
						       Stringpool* dynpool)
{
  gold_assert(this->symtab_shndx_ != -1U);
  if (this->symtab_shndx_ == 0)
    {
      // This object has no symbols.  Weird but legal.
      return;
    }

  // Read the symbol table section header.
  const unsigned int symtab_shndx = this->symtab_shndx_;
  typename This::Shdr symtabshdr(this,
				 this->elf_file_.section_header(symtab_shndx));
  gold_assert(symtabshdr.get_sh_type() == elfcpp::SHT_SYMTAB);

  // Read the local symbols.
  const int sym_size = This::sym_size;
  const unsigned int loccount = this->local_symbol_count_;
  gold_assert(loccount == symtabshdr.get_sh_info());
  off_t locsize = loccount * sym_size;
  const unsigned char* psyms = this->get_view(symtabshdr.get_sh_offset(),
					      locsize, true, true);

  // Read the symbol names.
  const unsigned int strtab_shndx =
    this->adjust_shndx(symtabshdr.get_sh_link());
  section_size_type strtab_size;
  const unsigned char* pnamesu = this->section_contents(strtab_shndx,
							&strtab_size,
							true);
  const char* pnames = reinterpret_cast<const char*>(pnamesu);

  // Loop over the local symbols.

  const Output_sections& out_sections(this->output_sections());
  unsigned int shnum = this->shnum();
  unsigned int count = 0;
  unsigned int dyncount = 0;
  // Skip the first, dummy, symbol.
  psyms += sym_size;
  bool discard_locals = parameters->options().discard_locals();
  for (unsigned int i = 1; i < loccount; ++i, psyms += sym_size)
    {
      elfcpp::Sym<size, big_endian> sym(psyms);

      Symbol_value<size>& lv(this->local_values_[i]);

      bool is_ordinary;
      unsigned int shndx = this->adjust_sym_shndx(i, sym.get_st_shndx(),
						  &is_ordinary);
      lv.set_input_shndx(shndx, is_ordinary);

      if (sym.get_st_type() == elfcpp::STT_SECTION)
	lv.set_is_section_symbol();
      else if (sym.get_st_type() == elfcpp::STT_TLS)
	lv.set_is_tls_symbol();

      // Save the input symbol value for use in do_finalize_local_symbols().
      lv.set_input_value(sym.get_st_value());

      // Decide whether this symbol should go into the output file.

      if ((shndx < shnum && out_sections[shndx] == NULL)
	  || (shndx == this->discarded_eh_frame_shndx_))
        {
	  lv.set_no_output_symtab_entry();
          gold_assert(!lv.needs_output_dynsym_entry());
          continue;
        }

      if (sym.get_st_type() == elfcpp::STT_SECTION)
	{
	  lv.set_no_output_symtab_entry();
          gold_assert(!lv.needs_output_dynsym_entry());
	  continue;
	}

      if (sym.get_st_name() >= strtab_size)
	{
	  this->error(_("local symbol %u section name out of range: %u >= %u"),
		      i, sym.get_st_name(),
		      static_cast<unsigned int>(strtab_size));
	  lv.set_no_output_symtab_entry();
	  continue;
	}

      // If --discard-locals option is used, discard all temporary local
      // symbols.  These symbols start with system-specific local label
      // prefixes, typically .L for ELF system.  We want to be compatible
      // with GNU ld so here we essentially use the same check in
      // bfd_is_local_label().  The code is different because we already
      // know that:
      //
      //   - the symbol is local and thus cannot have global or weak binding.
      //   - the symbol is not a section symbol.
      //   - the symbol has a name.
      //
      // We do not discard a symbol if it needs a dynamic symbol entry.
      const char* name = pnames + sym.get_st_name();
      if (discard_locals
	  && sym.get_st_type() != elfcpp::STT_FILE
	  && !lv.needs_output_dynsym_entry()
	  && parameters->target().is_local_label_name(name))
	{
	  lv.set_no_output_symtab_entry();
	  continue;
	}

      // Discard the local symbol if -retain_symbols_file is specified
      // and the local symbol is not in that file.
      if (!parameters->options().should_retain_symbol(name))
        {
          lv.set_no_output_symtab_entry();
          continue;
        }

      // Add the symbol to the symbol table string pool.
      pool->add(name, true, NULL);
      ++count;

      // If needed, add the symbol to the dynamic symbol table string pool.
      if (lv.needs_output_dynsym_entry())
        {
          dynpool->add(name, true, NULL);
          ++dyncount;
        }
    }

  this->output_local_symbol_count_ = count;
  this->output_local_dynsym_count_ = dyncount;
}

// Finalize the local symbols.  Here we set the final value in
// THIS->LOCAL_VALUES_ and set their output symbol table indexes.
// This function is always called from a singleton thread.  The actual
// output of the local symbols will occur in a separate task.

template<int size, bool big_endian>
unsigned int
Sized_relobj<size, big_endian>::do_finalize_local_symbols(unsigned int index,
							  off_t off,
                                                          Symbol_table* symtab)
{
  gold_assert(off == static_cast<off_t>(align_address(off, size >> 3)));

  const unsigned int loccount = this->local_symbol_count_;
  this->local_symbol_offset_ = off;

  const bool relocatable = parameters->options().relocatable();
  const Output_sections& out_sections(this->output_sections());
  const std::vector<Address>& out_offsets(this->section_offsets_);
  unsigned int shnum = this->shnum();

  for (unsigned int i = 1; i < loccount; ++i)
    {
      Symbol_value<size>& lv(this->local_values_[i]);

      bool is_ordinary;
      unsigned int shndx = lv.input_shndx(&is_ordinary);

      // Set the output symbol value.

      if (!is_ordinary)
	{
	  if (shndx == elfcpp::SHN_ABS || Symbol::is_common_shndx(shndx))
	    lv.set_output_value(lv.input_value());
	  else
	    {
	      this->error(_("unknown section index %u for local symbol %u"),
			  shndx, i);
	      lv.set_output_value(0);
	    }
	}
      else
	{
	  if (shndx >= shnum)
	    {
	      this->error(_("local symbol %u section index %u out of range"),
			  i, shndx);
	      shndx = 0;
	    }

	  Output_section* os = out_sections[shndx];
          Address secoffset = out_offsets[shndx];
          if (symtab->is_section_folded(this, shndx))
            {
              gold_assert (os == NULL && secoffset == invalid_address);
              // Get the os of the section it is folded onto.
              Section_id folded = symtab->icf()->get_folded_section(this,
                                                                    shndx);
              gold_assert(folded.first != NULL);
              Sized_relobj<size, big_endian>* folded_obj = reinterpret_cast
                <Sized_relobj<size, big_endian>*>(folded.first);
              os = folded_obj->output_section(folded.second);
              gold_assert(os != NULL);
              secoffset = folded_obj->get_output_section_offset(folded.second);
              gold_assert(secoffset != invalid_address);
            }

	  if (os == NULL)
	    {
              // This local symbol belongs to a section we are discarding.
              // In some cases when applying relocations later, we will
              // attempt to match it to the corresponding kept section,
              // so we leave the input value unchanged here.
	      continue;
	    }
	  else if (secoffset == invalid_address)
	    {
	      uint64_t start;

	      // This is a SHF_MERGE section or one which otherwise
	      // requires special handling.
	      if (shndx == this->discarded_eh_frame_shndx_)
		{
		  // This local symbol belongs to a discarded .eh_frame
		  // section.  Just treat it like the case in which
		  // os == NULL above.
		  gold_assert(this->has_eh_frame_);
		  continue;
		}
	      else if (!lv.is_section_symbol())
		{
		  // This is not a section symbol.  We can determine
		  // the final value now.
		  lv.set_output_value(os->output_address(this, shndx,
							 lv.input_value()));
		}
	      else if (!os->find_starting_output_address(this, shndx, &start))
		{
		  // This is a section symbol, but apparently not one
		  // in a merged section.  Just use the start of the
		  // output section.  This happens with relocatable
		  // links when the input object has section symbols
		  // for arbitrary non-merge sections.
		  lv.set_output_value(os->address());
		}
	      else
		{
		  // We have to consider the addend to determine the
		  // value to use in a relocation.  START is the start
		  // of this input section.
		  Merged_symbol_value<size>* msv =
		    new Merged_symbol_value<size>(lv.input_value(), start);
		  lv.set_merged_symbol_value(msv);
		}
	    }
          else if (lv.is_tls_symbol())
	    lv.set_output_value(os->tls_offset()
				+ secoffset
				+ lv.input_value());
	  else
	    lv.set_output_value((relocatable ? 0 : os->address())
				+ secoffset
				+ lv.input_value());
	}

      if (lv.needs_output_symtab_entry())
        {
          lv.set_output_symtab_index(index);
          ++index;
        }
    }
  return index;
}

// Set the output dynamic symbol table indexes for the local variables.

template<int size, bool big_endian>
unsigned int
Sized_relobj<size, big_endian>::do_set_local_dynsym_indexes(unsigned int index)
{
  const unsigned int loccount = this->local_symbol_count_;
  for (unsigned int i = 1; i < loccount; ++i)
    {
      Symbol_value<size>& lv(this->local_values_[i]);
      if (lv.needs_output_dynsym_entry())
        {
          lv.set_output_dynsym_index(index);
          ++index;
        }
    }
  return index;
}

// Set the offset where local dynamic symbol information will be stored.
// Returns the count of local symbols contributed to the symbol table by
// this object.

template<int size, bool big_endian>
unsigned int
Sized_relobj<size, big_endian>::do_set_local_dynsym_offset(off_t off)
{
  gold_assert(off == static_cast<off_t>(align_address(off, size >> 3)));
  this->local_dynsym_offset_ = off;
  return this->output_local_dynsym_count_;
}

// If Symbols_data is not NULL get the section flags from here otherwise
// get it from the file.

template<int size, bool big_endian>
uint64_t
Sized_relobj<size, big_endian>::do_section_flags(unsigned int shndx)
{
  Symbols_data* sd = this->get_symbols_data();
  if (sd != NULL)
    {
      const unsigned char* pshdrs = sd->section_headers_data
                                    + This::shdr_size * shndx;
      typename This::Shdr shdr(pshdrs);
      return shdr.get_sh_flags(); 
    }
  // If sd is NULL, read the section header from the file.
  return this->elf_file_.section_flags(shndx); 
}

// Get the section's ent size from Symbols_data.  Called by get_section_contents
// in icf.cc

template<int size, bool big_endian>
uint64_t
Sized_relobj<size, big_endian>::do_section_entsize(unsigned int shndx)
{
  Symbols_data* sd = this->get_symbols_data();
  gold_assert (sd != NULL);

  const unsigned char* pshdrs = sd->section_headers_data
                                + This::shdr_size * shndx;
  typename This::Shdr shdr(pshdrs);
  return shdr.get_sh_entsize(); 
}


// Write out the local symbols.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::write_local_symbols(
    Output_file* of,
    const Stringpool* sympool,
    const Stringpool* dynpool,
    Output_symtab_xindex* symtab_xindex,
    Output_symtab_xindex* dynsym_xindex)
{
  const bool strip_all = parameters->options().strip_all();
  if (strip_all)
    {
      if (this->output_local_dynsym_count_ == 0)
	return;
      this->output_local_symbol_count_ = 0;
    }

  gold_assert(this->symtab_shndx_ != -1U);
  if (this->symtab_shndx_ == 0)
    {
      // This object has no symbols.  Weird but legal.
      return;
    }

  // Read the symbol table section header.
  const unsigned int symtab_shndx = this->symtab_shndx_;
  typename This::Shdr symtabshdr(this,
				 this->elf_file_.section_header(symtab_shndx));
  gold_assert(symtabshdr.get_sh_type() == elfcpp::SHT_SYMTAB);
  const unsigned int loccount = this->local_symbol_count_;
  gold_assert(loccount == symtabshdr.get_sh_info());

  // Read the local symbols.
  const int sym_size = This::sym_size;
  off_t locsize = loccount * sym_size;
  const unsigned char* psyms = this->get_view(symtabshdr.get_sh_offset(),
					      locsize, true, false);

  // Read the symbol names.
  const unsigned int strtab_shndx =
    this->adjust_shndx(symtabshdr.get_sh_link());
  section_size_type strtab_size;
  const unsigned char* pnamesu = this->section_contents(strtab_shndx,
							&strtab_size,
							false);
  const char* pnames = reinterpret_cast<const char*>(pnamesu);

  // Get views into the output file for the portions of the symbol table
  // and the dynamic symbol table that we will be writing.
  off_t output_size = this->output_local_symbol_count_ * sym_size;
  unsigned char* oview = NULL;
  if (output_size > 0)
    oview = of->get_output_view(this->local_symbol_offset_, output_size);

  off_t dyn_output_size = this->output_local_dynsym_count_ * sym_size;
  unsigned char* dyn_oview = NULL;
  if (dyn_output_size > 0)
    dyn_oview = of->get_output_view(this->local_dynsym_offset_,
                                    dyn_output_size);

  const Output_sections out_sections(this->output_sections());

  gold_assert(this->local_values_.size() == loccount);

  unsigned char* ov = oview;
  unsigned char* dyn_ov = dyn_oview;
  psyms += sym_size;
  for (unsigned int i = 1; i < loccount; ++i, psyms += sym_size)
    {
      elfcpp::Sym<size, big_endian> isym(psyms);

      Symbol_value<size>& lv(this->local_values_[i]);

      bool is_ordinary;
      unsigned int st_shndx = this->adjust_sym_shndx(i, isym.get_st_shndx(),
						     &is_ordinary);
      if (is_ordinary)
	{
	  gold_assert(st_shndx < out_sections.size());
	  if (out_sections[st_shndx] == NULL)
	    continue;
	  st_shndx = out_sections[st_shndx]->out_shndx();
	  if (st_shndx >= elfcpp::SHN_LORESERVE)
	    {
	      if (lv.needs_output_symtab_entry() && !strip_all)
		symtab_xindex->add(lv.output_symtab_index(), st_shndx);
	      if (lv.needs_output_dynsym_entry())
		dynsym_xindex->add(lv.output_dynsym_index(), st_shndx);
	      st_shndx = elfcpp::SHN_XINDEX;
	    }
	}

      // Write the symbol to the output symbol table.
      if (!strip_all && lv.needs_output_symtab_entry())
        {
          elfcpp::Sym_write<size, big_endian> osym(ov);

          gold_assert(isym.get_st_name() < strtab_size);
          const char* name = pnames + isym.get_st_name();
          osym.put_st_name(sympool->get_offset(name));
          osym.put_st_value(this->local_values_[i].value(this, 0));
          osym.put_st_size(isym.get_st_size());
          osym.put_st_info(isym.get_st_info());
          osym.put_st_other(isym.get_st_other());
          osym.put_st_shndx(st_shndx);

          ov += sym_size;
        }

      // Write the symbol to the output dynamic symbol table.
      if (lv.needs_output_dynsym_entry())
        {
          gold_assert(dyn_ov < dyn_oview + dyn_output_size);
          elfcpp::Sym_write<size, big_endian> osym(dyn_ov);

          gold_assert(isym.get_st_name() < strtab_size);
          const char* name = pnames + isym.get_st_name();
          osym.put_st_name(dynpool->get_offset(name));
          osym.put_st_value(this->local_values_[i].value(this, 0));
          osym.put_st_size(isym.get_st_size());
          osym.put_st_info(isym.get_st_info());
          osym.put_st_other(isym.get_st_other());
          osym.put_st_shndx(st_shndx);

          dyn_ov += sym_size;
        }
    }


  if (output_size > 0)
    {
      gold_assert(ov - oview == output_size);
      of->write_output_view(this->local_symbol_offset_, output_size, oview);
    }

  if (dyn_output_size > 0)
    {
      gold_assert(dyn_ov - dyn_oview == dyn_output_size);
      of->write_output_view(this->local_dynsym_offset_, dyn_output_size,
                            dyn_oview);
    }
}

// Set *INFO to symbolic information about the offset OFFSET in the
// section SHNDX.  Return true if we found something, false if we
// found nothing.

template<int size, bool big_endian>
bool
Sized_relobj<size, big_endian>::get_symbol_location_info(
    unsigned int shndx,
    off_t offset,
    Symbol_location_info* info)
{
  if (this->symtab_shndx_ == 0)
    return false;

  section_size_type symbols_size;
  const unsigned char* symbols = this->section_contents(this->symtab_shndx_,
							&symbols_size,
							false);

  unsigned int symbol_names_shndx =
    this->adjust_shndx(this->section_link(this->symtab_shndx_));
  section_size_type names_size;
  const unsigned char* symbol_names_u =
    this->section_contents(symbol_names_shndx, &names_size, false);
  const char* symbol_names = reinterpret_cast<const char*>(symbol_names_u);

  const int sym_size = This::sym_size;
  const size_t count = symbols_size / sym_size;

  const unsigned char* p = symbols;
  for (size_t i = 0; i < count; ++i, p += sym_size)
    {
      elfcpp::Sym<size, big_endian> sym(p);

      if (sym.get_st_type() == elfcpp::STT_FILE)
	{
	  if (sym.get_st_name() >= names_size)
	    info->source_file = "(invalid)";
	  else
	    info->source_file = symbol_names + sym.get_st_name();
	  continue;
	}

      bool is_ordinary;
      unsigned int st_shndx = this->adjust_sym_shndx(i, sym.get_st_shndx(),
						     &is_ordinary);
      if (is_ordinary
	  && st_shndx == shndx
	  && static_cast<off_t>(sym.get_st_value()) <= offset
	  && (static_cast<off_t>(sym.get_st_value() + sym.get_st_size())
	      > offset))
        {
          if (sym.get_st_name() > names_size)
	    info->enclosing_symbol_name = "(invalid)";
	  else
            {
              info->enclosing_symbol_name = symbol_names + sym.get_st_name();
              if (parameters->options().do_demangle())
                {
                  char* demangled_name = cplus_demangle(
                      info->enclosing_symbol_name.c_str(),
                      DMGL_ANSI | DMGL_PARAMS);
                  if (demangled_name != NULL)
                    {
                      info->enclosing_symbol_name.assign(demangled_name);
                      free(demangled_name);
                    }
                }
            }
	  return true;
        }
    }

  return false;
}

// Look for a kept section corresponding to the given discarded section,
// and return its output address.  This is used only for relocations in
// debugging sections.  If we can't find the kept section, return 0.

template<int size, bool big_endian>
typename Sized_relobj<size, big_endian>::Address
Sized_relobj<size, big_endian>::map_to_kept_section(
    unsigned int shndx,
    bool* found) const
{
  Relobj* kept_object;
  unsigned int kept_shndx;
  if (this->get_kept_comdat_section(shndx, &kept_object, &kept_shndx))
    {
      Sized_relobj<size, big_endian>* kept_relobj =
	static_cast<Sized_relobj<size, big_endian>*>(kept_object);
      Output_section* os = kept_relobj->output_section(kept_shndx);
      Address offset = kept_relobj->get_output_section_offset(kept_shndx);
      if (os != NULL && offset != invalid_address)
	{
	  *found = true;
	  return os->address() + offset;
	}
    }
  *found = false;
  return 0;
}

// Get symbol counts.

template<int size, bool big_endian>
void
Sized_relobj<size, big_endian>::do_get_global_symbol_counts(
    const Symbol_table*,
    size_t* defined,
    size_t* used) const
{
  *defined = this->defined_count_;
  size_t count = 0;
  for (Symbols::const_iterator p = this->symbols_.begin();
       p != this->symbols_.end();
       ++p)
    if (*p != NULL
	&& (*p)->source() == Symbol::FROM_OBJECT
	&& (*p)->object() == this
	&& (*p)->is_defined())
      ++count;
  *used = count;
}

// Input_objects methods.

// Add a regular relocatable object to the list.  Return false if this
// object should be ignored.

bool
Input_objects::add_object(Object* obj)
{
  // Print the filename if the -t/--trace option is selected.
  if (parameters->options().trace())
    gold_info("%s", obj->name().c_str());

  if (!obj->is_dynamic())
    this->relobj_list_.push_back(static_cast<Relobj*>(obj));
  else
    {
      // See if this is a duplicate SONAME.
      Dynobj* dynobj = static_cast<Dynobj*>(obj);
      const char* soname = dynobj->soname();

      std::pair<Unordered_set<std::string>::iterator, bool> ins =
	this->sonames_.insert(soname);
      if (!ins.second)
	{
	  // We have already seen a dynamic object with this soname.
	  return false;
	}

      this->dynobj_list_.push_back(dynobj);
    }

  // Add this object to the cross-referencer if requested.
  if (parameters->options().user_set_print_symbol_counts())
    {
      if (this->cref_ == NULL)
	this->cref_ = new Cref();
      this->cref_->add_object(obj);
    }

  return true;
}

// For each dynamic object, record whether we've seen all of its
// explicit dependencies.

void
Input_objects::check_dynamic_dependencies() const
{
  bool issued_copy_dt_needed_error = false;
  for (Dynobj_list::const_iterator p = this->dynobj_list_.begin();
       p != this->dynobj_list_.end();
       ++p)
    {
      const Dynobj::Needed& needed((*p)->needed());
      bool found_all = true;
      Dynobj::Needed::const_iterator pneeded;
      for (pneeded = needed.begin(); pneeded != needed.end(); ++pneeded)
	{
	  if (this->sonames_.find(*pneeded) == this->sonames_.end())
	    {
	      found_all = false;
	      break;
	    }
	}
      (*p)->set_has_unknown_needed_entries(!found_all);

      // --copy-dt-needed-entries aka --add-needed is a GNU ld option
      // --that gold does not support.  However, they cause no trouble
      // --unless there is a DT_NEEDED entry that we don't know about;
      // --warn only in that case.
      if (!found_all
	  && !issued_copy_dt_needed_error
	  && (parameters->options().copy_dt_needed_entries()
	      || parameters->options().add_needed()))
	{
	  const char* optname;
	  if (parameters->options().copy_dt_needed_entries())
	    optname = "--copy-dt-needed-entries";
	  else
	    optname = "--add-needed";
	  gold_error(_("%s is not supported but is required for %s in %s"),
		     optname, (*pneeded).c_str(), (*p)->name().c_str());
	  issued_copy_dt_needed_error = true;
	}
    }
}

// Start processing an archive.

void
Input_objects::archive_start(Archive* archive)
{
  if (parameters->options().user_set_print_symbol_counts())
    {
      if (this->cref_ == NULL)
	this->cref_ = new Cref();
      this->cref_->add_archive_start(archive);
    }
}

// Stop processing an archive.

void
Input_objects::archive_stop(Archive* archive)
{
  if (parameters->options().user_set_print_symbol_counts())
    this->cref_->add_archive_stop(archive);
}

// Print symbol counts

void
Input_objects::print_symbol_counts(const Symbol_table* symtab) const
{
  if (parameters->options().user_set_print_symbol_counts()
      && this->cref_ != NULL)
    this->cref_->print_symbol_counts(symtab);
}

// Relocate_info methods.

// Return a string describing the location of a relocation.  This is
// only used in error messages.

template<int size, bool big_endian>
std::string
Relocate_info<size, big_endian>::location(size_t, off_t offset) const
{
  // See if we can get line-number information from debugging sections.
  std::string filename;
  std::string file_and_lineno;   // Better than filename-only, if available.

  Sized_dwarf_line_info<size, big_endian> line_info(this->object);
  // This will be "" if we failed to parse the debug info for any reason.
  file_and_lineno = line_info.addr2line(this->data_shndx, offset);

  std::string ret(this->object->name());
  ret += ':';
  Symbol_location_info info;
  if (this->object->get_symbol_location_info(this->data_shndx, offset, &info))
    {
      ret += " in function ";
      ret += info.enclosing_symbol_name;
      ret += ":";
      filename = info.source_file;
    }

  if (!file_and_lineno.empty())
    ret += file_and_lineno;
  else
    {
      if (!filename.empty())
        ret += filename;
      ret += "(";
      ret += this->object->section_name(this->data_shndx);
      char buf[100];
      // Offsets into sections have to be positive.
      snprintf(buf, sizeof(buf), "+0x%lx", static_cast<long>(offset));
      ret += buf;
      ret += ")";
    }
  return ret;
}

} // End namespace gold.

namespace
{

using namespace gold;

// Read an ELF file with the header and return the appropriate
// instance of Object.

template<int size, bool big_endian>
Object*
make_elf_sized_object(const std::string& name, Input_file* input_file,
		      off_t offset, const elfcpp::Ehdr<size, big_endian>& ehdr,
		      bool* punconfigured)
{
  Target* target = select_target(ehdr.get_e_machine(), size, big_endian,
				 ehdr.get_e_ident()[elfcpp::EI_OSABI],
				 ehdr.get_e_ident()[elfcpp::EI_ABIVERSION]);
  if (target == NULL)
    gold_fatal(_("%s: unsupported ELF machine number %d"),
	       name.c_str(), ehdr.get_e_machine());

  if (!parameters->target_valid())
    set_parameters_target(target);
  else if (target != &parameters->target())
    {
      if (punconfigured != NULL)
	*punconfigured = true;
      else
	gold_error(_("%s: incompatible target"), name.c_str());
      return NULL;
    }

  return target->make_elf_object<size, big_endian>(name, input_file, offset,
						   ehdr);
}

} // End anonymous namespace.

namespace gold
{

// Return whether INPUT_FILE is an ELF object.

bool
is_elf_object(Input_file* input_file, off_t offset,
	      const unsigned char** start, int *read_size)
{
  off_t filesize = input_file->file().filesize();
  int want = elfcpp::Elf_recognizer::max_header_size;
  if (filesize - offset < want)
    want = filesize - offset;

  const unsigned char* p = input_file->file().get_view(offset, 0, want,
						       true, false);
  *start = p;
  *read_size = want;

  return elfcpp::Elf_recognizer::is_elf_file(p, want);
}

// Read an ELF file and return the appropriate instance of Object.

Object*
make_elf_object(const std::string& name, Input_file* input_file, off_t offset,
		const unsigned char* p, section_offset_type bytes,
		bool* punconfigured)
{
  if (punconfigured != NULL)
    *punconfigured = false;

  std::string error;
  bool big_endian;
  int size;
  if (!elfcpp::Elf_recognizer::is_valid_header(p, bytes, &size,
                                               &big_endian, &error))
    {
      gold_error(_("%s: %s"), name.c_str(), error.c_str());
      return NULL;
    }

  if (size == 32)
    {
      if (big_endian)
	{
#ifdef HAVE_TARGET_32_BIG
	  elfcpp::Ehdr<32, true> ehdr(p);
	  return make_elf_sized_object<32, true>(name, input_file,
						 offset, ehdr, punconfigured);
#else
	  if (punconfigured != NULL)
	    *punconfigured = true;
	  else
	    gold_error(_("%s: not configured to support "
			 "32-bit big-endian object"),
		       name.c_str());
	  return NULL;
#endif
	}
      else
	{
#ifdef HAVE_TARGET_32_LITTLE
	  elfcpp::Ehdr<32, false> ehdr(p);
	  return make_elf_sized_object<32, false>(name, input_file,
						  offset, ehdr, punconfigured);
#else
	  if (punconfigured != NULL)
	    *punconfigured = true;
	  else
	    gold_error(_("%s: not configured to support "
			 "32-bit little-endian object"),
		       name.c_str());
	  return NULL;
#endif
	}
    }
  else if (size == 64)
    {
      if (big_endian)
	{
#ifdef HAVE_TARGET_64_BIG
	  elfcpp::Ehdr<64, true> ehdr(p);
	  return make_elf_sized_object<64, true>(name, input_file,
						 offset, ehdr, punconfigured);
#else
	  if (punconfigured != NULL)
	    *punconfigured = true;
	  else
	    gold_error(_("%s: not configured to support "
			 "64-bit big-endian object"),
		       name.c_str());
	  return NULL;
#endif
	}
      else
	{
#ifdef HAVE_TARGET_64_LITTLE
	  elfcpp::Ehdr<64, false> ehdr(p);
	  return make_elf_sized_object<64, false>(name, input_file,
						  offset, ehdr, punconfigured);
#else
	  if (punconfigured != NULL)
	    *punconfigured = true;
	  else
	    gold_error(_("%s: not configured to support "
			 "64-bit little-endian object"),
		       name.c_str());
	  return NULL;
#endif
	}
    }
  else
    gold_unreachable();
}

// Instantiate the templates we need.

#ifdef HAVE_TARGET_32_LITTLE
template
void
Object::read_section_data<32, false>(elfcpp::Elf_file<32, false, Object>*,
				     Read_symbols_data*);
#endif

#ifdef HAVE_TARGET_32_BIG
template
void
Object::read_section_data<32, true>(elfcpp::Elf_file<32, true, Object>*,
				    Read_symbols_data*);
#endif

#ifdef HAVE_TARGET_64_LITTLE
template
void
Object::read_section_data<64, false>(elfcpp::Elf_file<64, false, Object>*,
				     Read_symbols_data*);
#endif

#ifdef HAVE_TARGET_64_BIG
template
void
Object::read_section_data<64, true>(elfcpp::Elf_file<64, true, Object>*,
				    Read_symbols_data*);
#endif

#ifdef HAVE_TARGET_32_LITTLE
template
class Sized_relobj<32, false>;
#endif

#ifdef HAVE_TARGET_32_BIG
template
class Sized_relobj<32, true>;
#endif

#ifdef HAVE_TARGET_64_LITTLE
template
class Sized_relobj<64, false>;
#endif

#ifdef HAVE_TARGET_64_BIG
template
class Sized_relobj<64, true>;
#endif

#ifdef HAVE_TARGET_32_LITTLE
template
struct Relocate_info<32, false>;
#endif

#ifdef HAVE_TARGET_32_BIG
template
struct Relocate_info<32, true>;
#endif

#ifdef HAVE_TARGET_64_LITTLE
template
struct Relocate_info<64, false>;
#endif

#ifdef HAVE_TARGET_64_BIG
template
struct Relocate_info<64, true>;
#endif

} // End namespace gold.
