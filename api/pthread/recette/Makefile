#*=====================================================================*/
#*    .../prgm/project/bigloo/bigloo/api/pthread/recette/Makefile      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Wed Jan 31 07:02:14 2018 (serrano)                */
#*    Copyright   :  2002-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Fair threads tests                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig 
include ../../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BCFLAGS		= -call/cc 

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/
SCM_FILE	= recette

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= recette.scm Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: c jvm

c: recette$(EXE_SUFFIX)
recette$(EXE_SUFFIX): recette.o
	$(BIGLOO) -static-all-bigloo $(EFLAGS) $(BFLAGS) recette.o -o recette$(EXE_SUFFIX)

jvm: 
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) recette-jvm$(SCRIPTEXTENSION); \
	  fi

recette-jvm$(SCRIPTEXTENSION): recette.class
	$(BIGLOO) -jvm $(EFLAGS) $(BFLAGS) recette.class -o recette-jvm$(SCRIPTEXTENSION)

pop:
	@ echo $(POPULATION:%=pthread/recette/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast recette.c recette$(EXE_SUFFIX)
	@- $(RM) -f recette.o
	@- $(RM) -f JVMMAIN.class *.class recette-jvm$(SCRIPTEXTENSION) 

distclean: clean

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test: test-c test-jvm

test-c:
	(export LD_LIBRARY_PATH=../lib:$$LD_LIBRARY_PATH; \
	 export DYLD_LIBRARY_PATH=../lib:$$DYLD_LIBRARY_PATH; \
         ./recette.out)

test-jvm:
	./recette-jvm$(SCRIPTEXTENSION)

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .bgl .scm .o .class

#*---------------------------------------------------------------------*/
#*    .scm -> .o/.class                                                */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -static-all-bigloo -c $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.class
