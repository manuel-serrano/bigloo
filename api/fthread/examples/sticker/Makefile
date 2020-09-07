#*=====================================================================*/
#*    .../project/bigloo/api/fthread/examples/sticker/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:11:58 2017 (serrano)                */
#*    Copyright   :  2002-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Fair threads tests                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../../../Makefile.buildconfig 
include ../../../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BIGLOO		= bigloo
BFLAGS		= -unsafe -O3

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/
SCM_FILE	= sticker

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= sticker.scm Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: sticker.jvm sticker.out

c: sticker.out
sticker.out: sticker.o
	$(BIGLOO) $(BFLAGS) sticker.o -o sticker.out

jvm: sticker.jvm
sticker.jvm: sticker.class
	$(BIGLOO) -jvm $(BFLAGS) sticker.class -o sticker.jvm

pop:
	@ echo $(POPULATION:%=fthread/examples/sticker/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast a.out sticker.c
	@- $(RM) -f *.o
	@- $(RM) -f JVMMAIN.class *.class
	@- $(RM) -f sticker.out sticker.jvm

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test: test-c test-jvm

test-c:
	(export LD_LIBRARY_PATH=../lib:$$LD_LIBRARY_PATH; ./sticker.out)

test-jvm:
	./sticker.jvm

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .bgl .scm .o .class

#*---------------------------------------------------------------------*/
#*    .scm.o                                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -c $(BFLAGS) $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(BFLAGS) $*.scm -o $*.class

