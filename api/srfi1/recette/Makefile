#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/srfi1/recette/Makefile           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:16:57 2017 (serrano)                */
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
	$(BIGLOO) $(EFLAGS) $(BFLAGS) recette.o -o recette$(EXE_SUFFIX) -L $(BOOTLIBDIR)

jvm: 
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) recette-jvm$(SCRIPTEXTENSION); \
	  fi

recette-jvm$(SCRIPTEXTENSION): recette.class
	$(BIGLOO) -jvm $(EFLAGS) $(BFLAGS) recette.class -o recette-jvm$(SCRIPTEXTENSION)

pop:
	@ echo $(POPULATION:%=srfi1/recette/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast recette.c recette$(EXE_SUFFIX)
	@- $(RM) -f recette.o
	@- $(RM) -f JVMMAIN.class *.class recette-jvm$(SCRIPTEXTENSION) 

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
.SUFFIXES: .bgl .scm .o .class .obj

#*---------------------------------------------------------------------*/
#*    .scm -> .o/.class/.obj                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -c $(EFLAGS) $(BFLAGS) $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(EFLAGS) $(BFLAGS) $*.scm -o $*.class
