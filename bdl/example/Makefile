#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdl/example/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:20:25 2017 (serrano)                */
#*    Copyright   :  2002-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Fair threads tests                     */
#*=====================================================================*/
 
#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig 
include ../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BIGLOO		= bigloo
BFLAGS		= -unsafe -O3

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/
SCM_FILE	= bdlex

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= bdlex.scm README Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: bdlex.jvm bdlex.out

c: bdlex.out
bdlex.out: bdlex.o
	$(BIGLOO) $(BFLAGS) bdlex.o -o bdlex.out

jvm: bdlex.jvm
bdlex.jvm: bdlex.class
	$(BIGLOO) -jvm $(BFLAGS) bdlex.class -o bdlex.jvm

pop:
	@ echo $(POPULATION:%=bdl/example/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast a.out bdlex.c
	@- $(RM) -f *.o
	@- $(RM) -f JVMMAIN.class *.class
	@- $(RM) -f bdlex.out bdlex.jvm

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test: test-c test-jvm

test-c:
	(export LD_LIBRARY_PATH=../lib:$$LD_LIBRARY_PATH; \
	 export DYLD_LIBRARY_PATH=../lib:$$DYLD_LIBRARY_PATH; \
         ./bdlex.out)

test-jvm:
	./bdlex.jvm

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

