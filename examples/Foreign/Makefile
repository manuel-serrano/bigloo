#*=====================================================================*/
#*    serrano/prgm/project/bigloo/examples/Foreign/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Sep 16 15:05:03 1993                          */
#*    Last change :  Sun Jul  9 10:23:22 2017 (serrano)                */
#*    Copyright   :  1993-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the foreign example.                       */
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
BGLOPTFLAGS	= -O +rm
BFLAGS          = -v
CFLAGS	 	= -I.

POPULATION	= examples/Foreign/Makefile examples/Foreign/README \
                  examples/Foreign/el.c examples/Foreign/el.h       \
                  examples/Foreign/foreign.scm examples/Foreign/foreign2.scm

#*---------------------------------------------------------------------*/
#*    foreign                                                          */
#*---------------------------------------------------------------------*/
foreign: foreign.o foreign2.o el.o
	@ $(BIGLOO) $(BFLAGS) foreign.o foreign2.o el.o

foreign.o: el.o foreign2.o
	@ $(BIGLOO) $(BFLAGS) foreign.scm el.o -c

foreign2.o: foreign2.scm
	@ $(BIGLOO) $(BFLAGS) foreign2.scm -c

el.o: el.c
	@ echo el.c:
	@ $(CC) $(CFLAGS) -c el.c

test: foreign
	$(DEST)

pop:
	@ echo $(POPULATION)

#*---------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean

clean:
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec $(RM) {} \;   
	@- $(RM) -f *.o
	@- $(RM) -f foreign
	@- $(RM) -f a.out
