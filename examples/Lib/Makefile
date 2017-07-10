#*=====================================================================*/
#*    serrano/prgm/project/bigloo/examples/Lib/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 19 12:58:58 1998                          */
#*    Last change :  Sun Jul  9 10:23:54 2017 (serrano)                */
#*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the point library                          */
#*=====================================================================*/


#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/
BIGLOO          = $(BOOTBINDIR)/bigloo
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -v2 -heap-library point
BCOMMONFLAGGS	= -mkaddlib -fsharing -q $(VERBOSE) \
                  -copt '$(CCOMMONFLAGS)' -cc $(CC)
BSAFEFLAGS	= $(BCOMMONFLAGGS) -cg -O3 -g -cg -unsafev \
                  -eval '(set! *indent* 4)' -rm
BUNSAFEFLAGS	= $(BCOMMONFLAGS) -O4 -unsafe

LIBVERSION	= 1.0

# cigloo flags
CIGLOO		= $(BOOTBINDIR)/cigloo

# cflags
CC 		= `$(BIGLOO) -eval '(begin (print *cc*) (exit 0))'`
CCOMMONFLAGS	= -I.
CSAFEFLAGS	= $(CCOMMONFLAGS)
CUNSAFEFLAGS	= $(CCOMMONFLAGS) -O2

# library objects
SAFE_OBJECT	= olib/scm_point.o olib/point.o
UNSAFE_OBJECT	= olib_u/scm_point.o olib_u/point.o

# Population
POPULATION	= Makefile \
                  point.init make_lib.scm \
                  point.h point.c \
                  scm_point.scm \
                  example.scm

all: heap lib example

heap: point.heap

point.heap: point.sch scm_point.scm
	$(BIGLOO) $(BHEAPFLAGS) make_lib.scm -addheap point.heap

lib: lib_u lib.a

lib.a: olib $(SAFE_OBJECT)
	ar qcv libpoint_s-$(LIBVERSION).a $(SAFE_OBJECT) 

lib_u: olib_u $(UNSAFE_OBJECT)
	ar qcv libpoint_u-$(LIBVERSION).a $(UNSAFE_OBJECT) 

olib:
	mkdir olib

olib_u:
	mkdir olib_u

olib_u/scm_point.o olib/scm_point.o: scm_point.scm
	$(BIGLOO) $(BSAFEFLAGS) $(<F) -o $*.o -c

olib_u/point.o olib/point.o: point.c
	$(CC) $(CSAFEFLAGS) $(<F) -o $*.o -c

point.sch: point.h point.c
	cigloo $^ > point.sch

example: heap lib
	$(BIGLOO) -v2 -L . -library point -static-bigloo example.scm -o example

clean:
	@- $(RM) -f point.heap
	@- $(RM) -f point.sch scm_point.c
	@- $(RM) -fr olib olib_u
	@- $(RM) -f example example.c example.o
	@- $(RM) -f libpoint_s-$(LIBVERSION).a libpoint_u-$(LIBVERSION).a

test: example
	./example

pop: 
	echo $(POPULATION:%=examples/Lib/%)
