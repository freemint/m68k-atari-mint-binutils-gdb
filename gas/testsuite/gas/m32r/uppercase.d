#as:
#objdump: -dr
#name: uppercase

.*: +file format .*

Disassembly of section .text:

00000000 <foo>:
   0:	10 81 10 91 *	mv r0,r1 -> mvfc r0,cbr
