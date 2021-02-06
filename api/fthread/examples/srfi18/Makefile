#*=====================================================================*/
#*    .../prgm/project/bigloo/api/fthread/examples/srfi18/Makefile     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:11:30 2017 (serrano)                */
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
SCM_FILE	= srfi18

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= srfi18.scm Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: c jvm dotnet

c: srfi18$(EXE_SUFFIX)
srfi18$(EXE_SUFFIX): srfi18.o
	$(BIGLOO) $(BFLAGS) srfi18.o -o srfi18$(EXE_SUFFIX)

jvm: 
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) srfi18-jvm$(SCRIPTEXTENSION); \
	  fi

srfi18-jvm$(SCRIPTEXTENSION): srfi18.class
	$(BIGLOO) -jvm $(BFLAGS) srfi18.class -o srfi18-jvm$(SCRIPTEXTENSION)

dotnet: 
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
             $(MAKE) srfi18-dotnet$(SCRIPTEXTENSION); \
	  fi

srfi18-dotnet$(SCRIPTEXTENSION): srfi18.obj
	$(BIGLOO) -v -dotnet $(BFLAGS) srfi18.obj -o srfi18-dotnet$(SCRIPTEXTENSION)

pop:
	@ echo $(POPULATION:%=fthread/examples/srfi18/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast srfi18.c srfi18$(EXE_SUFFIX)
	@- $(RM) -f *.o
	@- $(RM) -f JVMMAIN.class *.class srfi18-jvm$(SCRIPTEXTENSION) 
	@- $(RM) -f *.obj
	@- $(RM) -f srfi18-dotnet.exe srfi18-dotnet$(SCRIPTEXTENSION)

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test: test-c test-jvm test-dotnet

test-c:
	(export LD_LIBRARY_PATH=../lib:$$LD_LIBRARY_PATH; ./srfi18.out)

test-jvm:
	./srfi18-jvm$(SCRIPTEXTENSION)

test-dotnet:
	./srfi18-dotnet$(SCRIPTEXTENSION)

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .bgl .scm .o .class .obj

#*---------------------------------------------------------------------*/
#*    .scm -> .o/.class/.obj                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -c $(BFLAGS) -call/cc $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(BFLAGS) $*.scm -o $*.class

.scm.obj:
	$(BIGLOO) -c -dotnet $(BFLAGS) $*.scm -o $*.obj
