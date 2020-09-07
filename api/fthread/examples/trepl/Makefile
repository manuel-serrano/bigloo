#*=====================================================================*/
#*    .../prgm/project/bigloo/api/fthread/examples/trepl/Makefile      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:12:06 2017 (serrano)                */
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

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/
SCM_FILE	= trepl

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= trepl.scm ex.scm Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: trepl.jvm trepl.out

c: trepl.out
trepl.out: trepl.o
	$(BIGLOO) $(BFLAGS) trepl.o -o trepl.out

jvm: trepl.jvm
trepl.jvm: trepl.class
	$(BIGLOO) -jvm $(BFLAGS) trepl.class -o trepl.jvm

pop:
	@ echo $(POPULATION:%=fthread/examples/trepl/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast a.out trepl.c
	@- $(RM) -f *.o
	@- $(RM) -f JVMMAIN.class *.class
	@- $(RM) -f trepl.out trepl.jvm

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test: test-c test-jvm

test-c:
	(export LD_LIBRARY_PATH=../lib:$$LD_LIBRARY_PATH; ./trepl.out)

test-jvm:
	./trepl.jvm

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .bgl .scm .o .class

#*---------------------------------------------------------------------*/
#*    .scm.o                                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -c $(BFLAGS) -call/cc $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(BFLAGS) $*.scm -o $*.class

