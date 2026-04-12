#*=====================================================================*/
#*    bigloo/api/ssl/recette/Makefile                                  */
#*    -------------------------------------------------------------    */
#*    SSL library test suite                                           */
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
	@ echo $(POPULATION:%=ssl/recette/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast recette.c recette$(EXE_SUFFIX)
	@- $(RM) -f recette.o
	@- $(RM) -f JVMMAIN.class *.class recette-jvm$(SCRIPTEXTENSION)
	@- $(RM) -f recette-cert.pem recette-key.pem recette-key-pkcs1.pem
	@- $(RM) -f recette.p12 test.crl

distclean: clean

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test: c jvm test-c test-jvm

test-c:
	(export LD_LIBRARY_PATH=../lib:$$LD_LIBRARY_PATH; \
	 export DYLD_LIBRARY_PATH=../lib:$$DYLD_LIBRARY_PATH; \
         ./recette$(EXE_SUFFIX))

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
#*---------------------------------------------------------------------*/
#*    .scm -> .o/.class                                                */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -static-all-bigloo -c $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.class
