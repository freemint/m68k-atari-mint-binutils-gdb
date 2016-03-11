// layout.h -- lay out output file sections for gold  -*- C++ -*-

// Copyright 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

#ifndef GOLD_LAYOUT_H
#define GOLD_LAYOUT_H

#include <cstring>
#include <list>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include "script.h"
#include "workqueue.h"
#include "object.h"
#include "dynobj.h"
#include "stringpool.h"

namespace gold
{

class General_options;
class Incremental_inputs;
class Input_objects;
class Mapfile;
class Symbol_table;
class Output_section_data;
class Output_section;
class Output_section_headers;
class Output_segment_headers;
class Output_file_header;
class Output_segment;
class Output_data;
class Output_data_dynamic;
class Output_symtab_xindex;
class Output_reduced_debug_abbrev_section;
class Output_reduced_debug_info_section;
class Eh_frame;
class Target;

// This task function handles mapping the input sections to output
// sections and laying them out in memory.

class Layout_task_runner : public Task_function_runner
{
 public:
  // OPTIONS is the command line options, INPUT_OBJECTS is the list of
  // input objects, SYMTAB is the symbol table, LAYOUT is the layout
  // object.
  Layout_task_runner(const General_options& options,
		     const Input_objects* input_objects,
		     Symbol_table* symtab,
                     Target* target,
		     Layout* layout,
		     Mapfile* mapfile)
    : options_(options), input_objects_(input_objects), symtab_(symtab),
      target_(target), layout_(layout), mapfile_(mapfile)
  { }

  // Run the operation.
  void
  run(Workqueue*, const Task*);

 private:
  Layout_task_runner(const Layout_task_runner&);
  Layout_task_runner& operator=(const Layout_task_runner&);

  const General_options& options_;
  const Input_objects* input_objects_;
  Symbol_table* symtab_;
  Target* target_;
  Layout* layout_;
  Mapfile* mapfile_;
};

// This class holds information about the comdat group or
// .gnu.linkonce section that will be kept for a given signature.

class Kept_section
{
 private:
  // For a comdat group, we build a mapping from the name of each
  // section in the group to the section index and the size in object.
  // When we discard a group in some other object file, we use this
  // map to figure out which kept section the discarded section is
  // associated with.  We then use that mapping when processing relocs
  // against discarded sections.
  struct Comdat_section_info
  {
    // The section index.
    unsigned int shndx;
    // The section size.
    uint64_t size;

    Comdat_section_info(unsigned int a_shndx, uint64_t a_size)
      : shndx(a_shndx), size(a_size)
    { }
  };

  // Most comdat groups have only one or two sections, so we use a
  // std::map rather than an Unordered_map to optimize for that case
  // without paying too heavily for groups with more sections.
  typedef std::map<std::string, Comdat_section_info> Comdat_group;

 public:
  Kept_section()
    : object_(NULL), shndx_(0), is_comdat_(false), is_group_name_(false)
  { this->u_.linkonce_size = 0; }

  // We need to support copies for the signature map in the Layout
  // object, but we should never copy an object after it has been
  // marked as a comdat section.
  Kept_section(const Kept_section& k)
    : object_(k.object_), shndx_(k.shndx_), is_comdat_(false),
      is_group_name_(k.is_group_name_)
  {
    gold_assert(!k.is_comdat_);
    this->u_.linkonce_size = 0;
  }

  ~Kept_section()
  {
    if (this->is_comdat_)
      delete this->u_.group_sections;
  }

  // The object where this section lives.
  Relobj*
  object() const
  { return this->object_; }

  // Set the object.
  void
  set_object(Relobj* object)
  {
    gold_assert(this->object_ == NULL);
    this->object_ = object;
  }

  // The section index.
  unsigned int
  shndx() const
  { return this->shndx_; }

  // Set the section index.
  void
  set_shndx(unsigned int shndx)
  {
    gold_assert(this->shndx_ == 0);
    this->shndx_ = shndx;
  }

  // Whether this is a comdat group.
  bool
  is_comdat() const
  { return this->is_comdat_; }

  // Set that this is a comdat group.
  void
  set_is_comdat()
  {
    gold_assert(!this->is_comdat_);
    this->is_comdat_ = true;
    this->u_.group_sections = new Comdat_group();
  }

  // Whether this is associated with the name of a group or section
  // rather than the symbol name derived from a linkonce section.
  bool
  is_group_name() const
  { return this->is_group_name_; }

  // Note that this represents a comdat group rather than a single
  // linkonce section.
  void
  set_is_group_name()
  { this->is_group_name_ = true; }

  // Add a section to the group list.
  void
  add_comdat_section(const std::string& name, unsigned int shndx,
		     uint64_t size)
  {
    gold_assert(this->is_comdat_);
    Comdat_section_info sinfo(shndx, size);
    this->u_.group_sections->insert(std::make_pair(name, sinfo));
  }

  // Look for a section name in the group list, and return whether it
  // was found.  If found, returns the section index and size.
  bool
  find_comdat_section(const std::string& name, unsigned int *pshndx,
		      uint64_t *psize) const
  {
    gold_assert(this->is_comdat_);
    Comdat_group::const_iterator p = this->u_.group_sections->find(name);
    if (p == this->u_.group_sections->end())
      return false;
    *pshndx = p->second.shndx;
    *psize = p->second.size;
    return true;
  }

  // If there is only one section in the group list, return true, and
  // return the section index and size.
  bool
  find_single_comdat_section(unsigned int *pshndx, uint64_t *psize) const
  {
    gold_assert(this->is_comdat_);
    if (this->u_.group_sections->size() != 1)
      return false;
    Comdat_group::const_iterator p = this->u_.group_sections->begin();
    *pshndx = p->second.shndx;
    *psize = p->second.size;
    return true;
  }

  // Return the size of a linkonce section.
  uint64_t
  linkonce_size() const
  {
    gold_assert(!this->is_comdat_);
    return this->u_.linkonce_size;
  }

  // Set the size of a linkonce section.
  void
  set_linkonce_size(uint64_t size)
  {
    gold_assert(!this->is_comdat_);
    this->u_.linkonce_size = size;
  }

 private:
  // No assignment.
  Kept_section& operator=(const Kept_section&);

  // The object containing the comdat group or .gnu.linkonce section.
  Relobj* object_;
  // Index of the group section for comdats and the section itself for
  // .gnu.linkonce.
  unsigned int shndx_;
  // True if this is for a comdat group rather than a .gnu.linkonce
  // section.
  bool is_comdat_;
  // The Kept_sections are values of a mapping, that maps names to
  // them.  This field is true if this struct is associated with the
  // name of a comdat or .gnu.linkonce, false if it is associated with
  // the name of a symbol obtained from the .gnu.linkonce.* name
  // through some heuristics.
  bool is_group_name_;
  union
  {
    // If the is_comdat_ field is true, this holds a map from names of
    // the sections in the group to section indexes in object_ and to
    // section sizes.
    Comdat_group* group_sections;
    // If the is_comdat_ field is false, this holds the size of the
    // single section.
    uint64_t linkonce_size;
  } u_;
};

// This class handles the details of laying out input sections.

class Layout
{
 public:
  Layout(int number_of_input_files, Script_options*);

  ~Layout()
  {
    delete this->relaxation_debug_check_;
    delete this->segment_states_;
  }

  // Given an input section SHNDX, named NAME, with data in SHDR, from
  // the object file OBJECT, return the output section where this
  // input section should go.  RELOC_SHNDX is the index of a
  // relocation section which applies to this section, or 0 if none,
  // or -1U if more than one.  RELOC_TYPE is the type of the
  // relocation section if there is one.  Set *OFFSET to the offset
  // within the output section.
  template<int size, bool big_endian>
  Output_section*
  layout(Sized_relobj<size, big_endian> *object, unsigned int shndx,
	 const char* name, const elfcpp::Shdr<size, big_endian>& shdr,
	 unsigned int reloc_shndx, unsigned int reloc_type, off_t* offset);

  // Layout an input reloc section when doing a relocatable link.  The
  // section is RELOC_SHNDX in OBJECT, with data in SHDR.
  // DATA_SECTION is the reloc section to which it refers.  RR is the
  // relocatable information.
  template<int size, bool big_endian>
  Output_section*
  layout_reloc(Sized_relobj<size, big_endian>* object,
	       unsigned int reloc_shndx,
	       const elfcpp::Shdr<size, big_endian>& shdr,
	       Output_section* data_section,
	       Relocatable_relocs* rr);

  // Layout a group section when doing a relocatable link.
  template<int size, bool big_endian>
  void
  layout_group(Symbol_table* symtab,
	       Sized_relobj<size, big_endian>* object,
	       unsigned int group_shndx,
	       const char* group_section_name,
	       const char* signature,
	       const elfcpp::Shdr<size, big_endian>& shdr,
	       elfcpp::Elf_Word flags,
	       std::vector<unsigned int>* shndxes);

  // Like layout, only for exception frame sections.  OBJECT is an
  // object file.  SYMBOLS is the contents of the symbol table
  // section, with size SYMBOLS_SIZE.  SYMBOL_NAMES is the contents of
  // the symbol name section, with size SYMBOL_NAMES_SIZE.  SHNDX is a
  // .eh_frame section in OBJECT.  SHDR is the section header.
  // RELOC_SHNDX is the index of a relocation section which applies to
  // this section, or 0 if none, or -1U if more than one.  RELOC_TYPE
  // is the type of the relocation section if there is one.  This
  // returns the output section, and sets *OFFSET to the offset.
  template<int size, bool big_endian>
  Output_section*
  layout_eh_frame(Sized_relobj<size, big_endian>* object,
		  const unsigned char* symbols,
		  off_t symbols_size,
		  const unsigned char* symbol_names,
		  off_t symbol_names_size,
		  unsigned int shndx,
		  const elfcpp::Shdr<size, big_endian>& shdr,
		  unsigned int reloc_shndx, unsigned int reloc_type,
		  off_t* offset);

  // Handle a GNU stack note.  This is called once per input object
  // file.  SEEN_GNU_STACK is true if the object file has a
  // .note.GNU-stack section.  GNU_STACK_FLAGS is the section flags
  // from that section if there was one.
  void
  layout_gnu_stack(bool seen_gnu_stack, uint64_t gnu_stack_flags);

  // Add an Output_section_data to the layout.  This is used for
  // special sections like the GOT section.  IS_DYNAMIC_LINKER_SECTION
  // is true for sections which are used by the dynamic linker, such
  // as dynamic reloc sections.
  Output_section*
  add_output_section_data(const char* name, elfcpp::Elf_Word type,
			  elfcpp::Elf_Xword flags,
			  Output_section_data*, bool is_dynamic_linker_section);

  // Create dynamic sections if necessary.
  void
  create_initial_dynamic_sections(Symbol_table*);

  // Define __start and __stop symbols for output sections.
  void
  define_section_symbols(Symbol_table*);

  // Create automatic note sections.
  void
  create_notes();

  // Create sections for linker scripts.
  void
  create_script_sections()
  { this->script_options_->create_script_sections(this); }

  // Define symbols from any linker script.
  void
  define_script_symbols(Symbol_table* symtab)
  { this->script_options_->add_symbols_to_table(symtab); }

  // Define symbols for group signatures.
  void
  define_group_signatures(Symbol_table*);

  // Return the Stringpool used for symbol names.
  const Stringpool*
  sympool() const
  { return &this->sympool_; }

  // Return the Stringpool used for dynamic symbol names and dynamic
  // tags.
  const Stringpool*
  dynpool() const
  { return &this->dynpool_; }

  // Return the symtab_xindex section used to hold large section
  // indexes for the normal symbol table.
  Output_symtab_xindex*
  symtab_xindex() const
  { return this->symtab_xindex_; }

  // Return the dynsym_xindex section used to hold large section
  // indexes for the dynamic symbol table.
  Output_symtab_xindex*
  dynsym_xindex() const
  { return this->dynsym_xindex_; }

  // Return whether a section is a .gnu.linkonce section, given the
  // section name.
  static inline bool
  is_linkonce(const char* name)
  { return strncmp(name, ".gnu.linkonce", sizeof(".gnu.linkonce") - 1) == 0; }

  // Return true if a section is a debugging section.
  static inline bool
  is_debug_info_section(const char* name)
  {
    // Debugging sections can only be recognized by name.
    return (strncmp(name, ".debug", sizeof(".debug") - 1) == 0
            || strncmp(name, ".gnu.linkonce.wi.",
                       sizeof(".gnu.linkonce.wi.") - 1) == 0
            || strncmp(name, ".line", sizeof(".line") - 1) == 0
            || strncmp(name, ".stab", sizeof(".stab") - 1) == 0);
  }

  // Check if a comdat group or .gnu.linkonce section with the given
  // NAME is selected for the link.  If there is already a section,
  // *KEPT_SECTION is set to point to the signature and the function
  // returns false.  Otherwise, OBJECT, SHNDX,IS_COMDAT, and
  // IS_GROUP_NAME are recorded for this NAME in the layout object,
  // *KEPT_SECTION is set to the internal copy and the function return
  // false.
  bool
  find_or_add_kept_section(const std::string& name, Relobj* object, 
			   unsigned int shndx, bool is_comdat,
			   bool is_group_name, Kept_section** kept_section);

  // Finalize the layout after all the input sections have been added.
  off_t
  finalize(const Input_objects*, Symbol_table*, Target*, const Task*);

  // Return whether any sections require postprocessing.
  bool
  any_postprocessing_sections() const
  { return this->any_postprocessing_sections_; }

  // Return the size of the output file.
  off_t
  output_file_size() const
  { return this->output_file_size_; }

  // Return the TLS segment.  This will return NULL if there isn't
  // one.
  Output_segment*
  tls_segment() const
  { return this->tls_segment_; }

  // Return the normal symbol table.
  Output_section*
  symtab_section() const
  {
    gold_assert(this->symtab_section_ != NULL);
    return this->symtab_section_;
  }

  // Return the dynamic symbol table.
  Output_section*
  dynsym_section() const
  {
    gold_assert(this->dynsym_section_ != NULL);
    return this->dynsym_section_;
  }

  // Return the dynamic tags.
  Output_data_dynamic*
  dynamic_data() const
  { return this->dynamic_data_; }

  // Write out the output sections.
  void
  write_output_sections(Output_file* of) const;

  // Write out data not associated with an input file or the symbol
  // table.
  void
  write_data(const Symbol_table*, Output_file*) const;

  // Write out output sections which can not be written until all the
  // input sections are complete.
  void
  write_sections_after_input_sections(Output_file* of);

  // Return an output section named NAME, or NULL if there is none.
  Output_section*
  find_output_section(const char* name) const;

  // Return an output segment of type TYPE, with segment flags SET set
  // and segment flags CLEAR clear.  Return NULL if there is none.
  Output_segment*
  find_output_segment(elfcpp::PT type, elfcpp::Elf_Word set,
		      elfcpp::Elf_Word clear) const;

  // Return the number of segments we expect to produce.
  size_t
  expected_segment_count() const;

  // Set a flag to indicate that an object file uses the static TLS model.
  void
  set_has_static_tls()
  { this->has_static_tls_ = true; }

  // Return true if any object file uses the static TLS model.
  bool
  has_static_tls() const
  { return this->has_static_tls_; }

  // Return the options which may be set by a linker script.
  Script_options*
  script_options()
  { return this->script_options_; }

  const Script_options*
  script_options() const
  { return this->script_options_; }

  // Return the object managing inputs in incremental build. NULL in
  // non-incremental builds.
  Incremental_inputs*
  incremental_inputs()
  { return this->incremental_inputs_; }

  // Compute and write out the build ID if needed.
  void
  write_build_id(Output_file*) const;

  // Rewrite output file in binary format.
  void
  write_binary(Output_file* in) const;

  // Print output sections to the map file.
  void
  print_to_mapfile(Mapfile*) const;

  // Dump statistical information to stderr.
  void
  print_stats() const;

  // A list of segments.

  typedef std::vector<Output_segment*> Segment_list;

  // A list of sections.

  typedef std::vector<Output_section*> Section_list;

  // The list of information to write out which is not attached to
  // either a section or a segment.
  typedef std::vector<Output_data*> Data_list;

  // Store the allocated sections into the section list.  This is used
  // by the linker script code.
  void
  get_allocated_sections(Section_list*) const;

  // Make a section for a linker script to hold data.
  Output_section*
  make_output_section_for_script(const char* name);

  // Make a segment.  This is used by the linker script code.
  Output_segment*
  make_output_segment(elfcpp::Elf_Word type, elfcpp::Elf_Word flags);

  // Return the number of segments.
  size_t
  segment_count() const
  { return this->segment_list_.size(); }

  // Map from section flags to segment flags.
  static elfcpp::Elf_Word
  section_flags_to_segment(elfcpp::Elf_Xword flags);

  // Attach sections to segments.
  void
  attach_sections_to_segments();

  // For relaxation clean up, we need to know output section data created
  // from a linker script.
  void
  new_output_section_data_from_script(Output_section_data* posd)
  {
    if (this->record_output_section_data_from_script_)
      this->script_output_section_data_list_.push_back(posd);
  }

  // Return section list.
  const Section_list&
  section_list() const
  { return this->section_list_; }

 private:
  Layout(const Layout&);
  Layout& operator=(const Layout&);

  // Mapping from input section names to output section names.
  struct Section_name_mapping
  {
    const char* from;
    int fromlen;
    const char* to;
    int tolen;
  };
  static const Section_name_mapping section_name_mapping[];
  static const int section_name_mapping_count;

  // During a relocatable link, a list of group sections and
  // signatures.
  struct Group_signature
  {
    // The group section.
    Output_section* section;
    // The signature.
    const char* signature;

    Group_signature()
      : section(NULL), signature(NULL)
    { }

    Group_signature(Output_section* sectiona, const char* signaturea)
      : section(sectiona), signature(signaturea)
    { }
  };
  typedef std::vector<Group_signature> Group_signatures;

  // Create a note section, filling in the header.
  Output_section*
  create_note(const char* name, int note_type, const char *section_name,
	      size_t descsz, bool allocate, size_t* trailing_padding);

  // Create a note section for gold version.
  void
  create_gold_note();

  // Record whether the stack must be executable.
  void
  create_executable_stack_info();

  // Create a build ID note if needed.
  void
  create_build_id();

  // Link .stab and .stabstr sections.
  void
  link_stabs_sections();

  // Create .gnu_incremental_inputs and .gnu_incremental_strtab sections needed
  // for the next run of incremental linking to check what has changed.
  void
  create_incremental_info_sections();

  // Find the first read-only PT_LOAD segment, creating one if
  // necessary.
  Output_segment*
  find_first_load_seg();

  // Count the local symbols in the regular symbol table and the dynamic
  // symbol table, and build the respective string pools.
  void
  count_local_symbols(const Task*, const Input_objects*);

  // Create the output sections for the symbol table.
  void
  create_symtab_sections(const Input_objects*, Symbol_table*,
			 unsigned int, off_t*);

  // Create the .shstrtab section.
  Output_section*
  create_shstrtab();

  // Create the section header table.
  void
  create_shdrs(const Output_section* shstrtab_section, off_t*);

  // Create the dynamic symbol table.
  void
  create_dynamic_symtab(const Input_objects*, Symbol_table*,
			Output_section** pdynstr,
			unsigned int* plocal_dynamic_count,
			std::vector<Symbol*>* pdynamic_symbols,
			Versions* versions);

  // Assign offsets to each local portion of the dynamic symbol table.
  void
  assign_local_dynsym_offsets(const Input_objects*);

  // Finish the .dynamic section and PT_DYNAMIC segment.
  void
  finish_dynamic_section(const Input_objects*, const Symbol_table*);

  // Create the .interp section and PT_INTERP segment.
  void
  create_interp(const Target* target);

  // Create the version sections.
  void
  create_version_sections(const Versions*,
			  const Symbol_table*,
			  unsigned int local_symcount,
			  const std::vector<Symbol*>& dynamic_symbols,
			  const Output_section* dynstr);

  template<int size, bool big_endian>
  void
  sized_create_version_sections(const Versions* versions,
				const Symbol_table*,
				unsigned int local_symcount,
				const std::vector<Symbol*>& dynamic_symbols,
				const Output_section* dynstr);

  // Return whether to include this section in the link.
  template<int size, bool big_endian>
  bool
  include_section(Sized_relobj<size, big_endian>* object, const char* name,
		  const elfcpp::Shdr<size, big_endian>&);

  // Return the output section name to use given an input section
  // name.  Set *PLEN to the length of the name.  *PLEN must be
  // initialized to the length of NAME.
  static const char*
  output_section_name(const char* name, size_t* plen);

  // Return the number of allocated output sections.
  size_t
  allocated_output_section_count() const;

  // Return the output section for NAME, TYPE and FLAGS.
  Output_section*
  get_output_section(const char* name, Stringpool::Key name_key,
		     elfcpp::Elf_Word type, elfcpp::Elf_Xword flags,
		     bool is_interp, bool is_dynamic_linker_section);

  // Choose the output section for NAME in RELOBJ.
  Output_section*
  choose_output_section(const Relobj* relobj, const char* name,
			elfcpp::Elf_Word type, elfcpp::Elf_Xword flags,
			bool is_input_section, bool is_interp,
			bool is_dynamic_linker_section);

  // Create a new Output_section.
  Output_section*
  make_output_section(const char* name, elfcpp::Elf_Word type,
		      elfcpp::Elf_Xword flags, bool is_interp,
		      bool is_dynamic_linker_section);

  // Attach a section to a segment.
  void
  attach_section_to_segment(Output_section*);

  // Attach an allocated section to a segment.
  void
  attach_allocated_section_to_segment(Output_section*);

  // Set the final file offsets of all the segments.
  off_t
  set_segment_offsets(const Target*, Output_segment*, unsigned int* pshndx);

  // Set the file offsets of the sections when doing a relocatable
  // link.
  off_t
  set_relocatable_section_offsets(Output_data*, unsigned int* pshndx);

  // Set the final file offsets of all the sections not associated
  // with a segment.  We set section offsets in three passes: the
  // first handles all allocated sections, the second sections that
  // require postprocessing, and the last the late-bound STRTAB
  // sections (probably only shstrtab, which is the one we care about
  // because it holds section names).
  enum Section_offset_pass
  {
    BEFORE_INPUT_SECTIONS_PASS,
    POSTPROCESSING_SECTIONS_PASS,
    STRTAB_AFTER_POSTPROCESSING_SECTIONS_PASS
  };
  off_t
  set_section_offsets(off_t, Section_offset_pass pass);

  // Set the final section indexes of all the sections not associated
  // with a segment.  Returns the next unused index.
  unsigned int
  set_section_indexes(unsigned int pshndx);

  // Set the section addresses when using a script.
  Output_segment*
  set_section_addresses_from_script(Symbol_table*);

  // Find appropriate places or orphan sections in a script.
  void
  place_orphan_sections_in_script();

  // Return whether SEG1 comes before SEG2 in the output file.
  static bool
  segment_precedes(const Output_segment* seg1, const Output_segment* seg2);

  // Use to save and restore segments during relaxation. 
  typedef Unordered_map<const Output_segment*, const Output_segment*>
    Segment_states;

  // Save states of current output segments.
  void
  save_segments(Segment_states*);

  // Restore output segment states.
  void
  restore_segments(const Segment_states*);

  // Clean up after relaxation so that it is possible to lay out the
  // sections and segments again.
  void
  clean_up_after_relaxation();

  // Doing preparation work for relaxation.  This is factored out to make
  // Layout::finalized a bit smaller and easier to read.
  void
  prepare_for_relaxation();

  // Main body of the relaxation loop, which lays out the section.
  off_t
  relaxation_loop_body(int, Target*, Symbol_table*, Output_segment**,
		       Output_segment*, Output_segment_headers*,
		       Output_file_header*, unsigned int*);

  // A mapping used for kept comdats/.gnu.linkonce group signatures.
  typedef Unordered_map<std::string, Kept_section> Signatures;

  // Mapping from input section name/type/flags to output section.  We
  // use canonicalized strings here.

  typedef std::pair<Stringpool::Key,
		    std::pair<elfcpp::Elf_Word, elfcpp::Elf_Xword> > Key;

  struct Hash_key
  {
    size_t
    operator()(const Key& k) const;
  };

  typedef Unordered_map<Key, Output_section*, Hash_key> Section_name_map;

  // A comparison class for segments.

  struct Compare_segments
  {
    bool
    operator()(const Output_segment* seg1, const Output_segment* seg2)
    { return Layout::segment_precedes(seg1, seg2); }
  };

  typedef std::vector<Output_section_data*> Output_section_data_list;

  // Debug checker class.
  class Relaxation_debug_check
  {
   public:
    Relaxation_debug_check()
      : section_infos_()
    { }
 
    // Check that sections and special data are in reset states.
    void
    check_output_data_for_reset_values(const Layout::Section_list&,
				       const Layout::Data_list&);
  
    // Record information of a section list.
    void
    read_sections(const Layout::Section_list&);

    // Verify a section list with recorded information.
    void
    verify_sections(const Layout::Section_list&);
 
   private:
    // Information we care about a section.
    struct Section_info
    {
      // Output section described by this.
      Output_section* output_section;
      // Load address.
      uint64_t address;
      // Data size.
      off_t data_size;
      // File offset.
      off_t offset;
    };

    // Section information.
    std::vector<Section_info> section_infos_;
  };

  // The number of input files, for sizing tables.
  int number_of_input_files_;
  // Information set by scripts or by command line options.
  Script_options* script_options_;
  // The output section names.
  Stringpool namepool_;
  // The output symbol names.
  Stringpool sympool_;
  // The dynamic strings, if needed.
  Stringpool dynpool_;
  // The list of group sections and linkonce sections which we have seen.
  Signatures signatures_;
  // The mapping from input section name/type/flags to output sections.
  Section_name_map section_name_map_;
  // The list of output segments.
  Segment_list segment_list_;
  // The list of output sections.
  Section_list section_list_;
  // The list of output sections which are not attached to any output
  // segment.
  Section_list unattached_section_list_;
  // The list of unattached Output_data objects which require special
  // handling because they are not Output_sections.
  Data_list special_output_list_;
  // The section headers.
  Output_section_headers* section_headers_;
  // A pointer to the PT_TLS segment if there is one.
  Output_segment* tls_segment_;
  // A pointer to the PT_GNU_RELRO segment if there is one.
  Output_segment* relro_segment_;
  // The SHT_SYMTAB output section.
  Output_section* symtab_section_;
  // The SHT_SYMTAB_SHNDX for the regular symbol table if there is one.
  Output_symtab_xindex* symtab_xindex_;
  // The SHT_DYNSYM output section if there is one.
  Output_section* dynsym_section_;
  // The SHT_SYMTAB_SHNDX for the dynamic symbol table if there is one.
  Output_symtab_xindex* dynsym_xindex_;
  // The SHT_DYNAMIC output section if there is one.
  Output_section* dynamic_section_;
  // The dynamic data which goes into dynamic_section_.
  Output_data_dynamic* dynamic_data_;
  // The exception frame output section if there is one.
  Output_section* eh_frame_section_;
  // The exception frame data for eh_frame_section_.
  Eh_frame* eh_frame_data_;
  // Whether we have added eh_frame_data_ to the .eh_frame section.
  bool added_eh_frame_data_;
  // The exception frame header output section if there is one.
  Output_section* eh_frame_hdr_section_;
  // The space for the build ID checksum if there is one.
  Output_section_data* build_id_note_;
  // The output section containing dwarf abbreviations
  Output_reduced_debug_abbrev_section* debug_abbrev_;
  // The output section containing the dwarf debug info tree
  Output_reduced_debug_info_section* debug_info_;
  // A list of group sections and their signatures.
  Group_signatures group_signatures_;
  // The size of the output file.
  off_t output_file_size_;
  // Whether we have attached the sections to the segments.
  bool sections_are_attached_;
  // Whether we have seen an object file marked to require an
  // executable stack.
  bool input_requires_executable_stack_;
  // Whether we have seen at least one object file with an executable
  // stack marker.
  bool input_with_gnu_stack_note_;
  // Whether we have seen at least one object file without an
  // executable stack marker.
  bool input_without_gnu_stack_note_;
  // Whether we have seen an object file that uses the static TLS model.
  bool has_static_tls_;
  // Whether any sections require postprocessing.
  bool any_postprocessing_sections_;
  // Whether we have resized the signatures_ hash table.
  bool resized_signatures_;
  // Whether we have created a .stab*str output section.
  bool have_stabstr_section_;
  // In incremental build, holds information check the inputs and build the
  // .gnu_incremental_inputs section.
  Incremental_inputs* incremental_inputs_;
  // Whether we record output section data created in script
  bool record_output_section_data_from_script_;
  // List of output data that needs to be removed at relexation clean up.
  Output_section_data_list script_output_section_data_list_;
  // Structure to save segment states before entering the relaxation loop.
  Segment_states* segment_states_;
  // A relaxation debug checker.  We only create one when in debugging mode.
  Relaxation_debug_check* relaxation_debug_check_;
};

// This task handles writing out data in output sections which is not
// part of an input section, or which requires special handling.  When
// this is done, it unblocks both output_sections_blocker and
// final_blocker.

class Write_sections_task : public Task
{
 public:
  Write_sections_task(const Layout* layout, Output_file* of,
		      Task_token* output_sections_blocker,
		      Task_token* final_blocker)
    : layout_(layout), of_(of),
      output_sections_blocker_(output_sections_blocker),
      final_blocker_(final_blocker)
  { }

  // The standard Task methods.

  Task_token*
  is_runnable();

  void
  locks(Task_locker*);

  void
  run(Workqueue*);

  std::string
  get_name() const
  { return "Write_sections_task"; }

 private:
  class Write_sections_locker;

  const Layout* layout_;
  Output_file* of_;
  Task_token* output_sections_blocker_;
  Task_token* final_blocker_;
};

// This task handles writing out data which is not part of a section
// or segment.

class Write_data_task : public Task
{
 public:
  Write_data_task(const Layout* layout, const Symbol_table* symtab,
		  Output_file* of, Task_token* final_blocker)
    : layout_(layout), symtab_(symtab), of_(of), final_blocker_(final_blocker)
  { }

  // The standard Task methods.

  Task_token*
  is_runnable();

  void
  locks(Task_locker*);

  void
  run(Workqueue*);

  std::string
  get_name() const
  { return "Write_data_task"; }

 private:
  const Layout* layout_;
  const Symbol_table* symtab_;
  Output_file* of_;
  Task_token* final_blocker_;
};

// This task handles writing out the global symbols.

class Write_symbols_task : public Task
{
 public:
  Write_symbols_task(const Layout* layout, const Symbol_table* symtab,
		     const Input_objects* input_objects,
		     const Stringpool* sympool, const Stringpool* dynpool,
		     Output_file* of, Task_token* final_blocker)
    : layout_(layout), symtab_(symtab), input_objects_(input_objects),
      sympool_(sympool), dynpool_(dynpool), of_(of),
      final_blocker_(final_blocker)
  { }

  // The standard Task methods.

  Task_token*
  is_runnable();

  void
  locks(Task_locker*);

  void
  run(Workqueue*);

  std::string
  get_name() const
  { return "Write_symbols_task"; }

 private:
  const Layout* layout_;
  const Symbol_table* symtab_;
  const Input_objects* input_objects_;
  const Stringpool* sympool_;
  const Stringpool* dynpool_;
  Output_file* of_;
  Task_token* final_blocker_;
};

// This task handles writing out data in output sections which can't
// be written out until all the input sections have been handled.
// This is for sections whose contents is based on the contents of
// other output sections.

class Write_after_input_sections_task : public Task
{
 public:
  Write_after_input_sections_task(Layout* layout, Output_file* of,
				  Task_token* input_sections_blocker,
				  Task_token* final_blocker)
    : layout_(layout), of_(of),
      input_sections_blocker_(input_sections_blocker),
      final_blocker_(final_blocker)
  { }

  // The standard Task methods.

  Task_token*
  is_runnable();

  void
  locks(Task_locker*);

  void
  run(Workqueue*);

  std::string
  get_name() const
  { return "Write_after_input_sections_task"; }

 private:
  Layout* layout_;
  Output_file* of_;
  Task_token* input_sections_blocker_;
  Task_token* final_blocker_;
};

// This task function handles closing the file.

class Close_task_runner : public Task_function_runner
{
 public:
  Close_task_runner(const General_options* options, const Layout* layout,
		    Output_file* of)
    : options_(options), layout_(layout), of_(of)
  { }

  // Run the operation.
  void
  run(Workqueue*, const Task*);

 private:
  const General_options* options_;
  const Layout* layout_;
  Output_file* of_;
};

// A small helper function to align an address.

inline uint64_t
align_address(uint64_t address, uint64_t addralign)
{
  if (addralign != 0)
    address = (address + addralign - 1) &~ (addralign - 1);
  return address;
}

} // End namespace gold.

#endif // !defined(GOLD_LAYOUT_H)
