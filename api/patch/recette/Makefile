#*=====================================================================*/
#*    .../prgm/project/bigloo/bigloo/api/patch/recette/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Thu Aug 16 11:05:00 2018 (serrano)                */
#*    Copyright   :  2002-18 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Gstreamer tests                        */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig 
include ../../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BCFLAGS		= 

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
all: c # jvm

c: recette$(EXE_SUFFIX)
recette$(EXE_SUFFIX): recette.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) recette.o -o recette$(EXE_SUFFIX)

jvm: 
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) recette-jvm$(SCRIPTEXTENSION); \
	  fi

recette-jvm$(SCRIPTEXTENSION): recette.class
	$(BIGLOO) -jvm $(EFLAGS) $(BFLAGS) recette.class -o recette-jvm$(SCRIPTEXTENSION)

pop:
	@ echo $(POPULATION:%=patch/recette/%)

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
#*    .scm -> .o/.class/.obj                                           */
#*---------------------------------------------------------------------*/
.scm.o: $*.scm
	$(BIGLOO) -c $(EFLAGS) $(BCFLAGS) $(BFLAGS) -copt "-I../src/Clib" $*.scm -o $*.o

.scm.class: $*.scm
	$(BIGLOO) -c -jvm $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.class
