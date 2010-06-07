#*=====================================================================*/
#*    serrano/prgm/project/bigloo/recette/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Nov  2 17:38:43 1992                          */
#*    Last change :  Mon Jun  7 16:58:46 2010 (serrano)                */
#*    Copyright   :  1992-2010 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the recette                                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the executable used to bootstrap
BIGLOO          = $(BGLBUILDBIGLOO)
# which C compiler to be used
CC              = `$(BIGLOO) -eval '(begin (print *cc*) (exit 0))'`
# the shell to be used
SHELL           = /bin/sh
# lint
LINT		= lint
# lint options
LINTFLAGS	= -u -v -I$(BOOTLIBDIR)
# The package to compute afile
AFILE		= $(BGLBUILDBINDIR)/$(AFILE_EXE)
# The package association tool
JFILE		= $(BGLBUILDBINDIR)/$(JFILE_EXE)
# The tags producer
BTAGS		= $(BGLBUILDBINDIR)/$(BTAGS_EXE)
#
JVMCLASSPATH	= -jvm-bigloo-classpath $(BOOTLIBDIR)

#*---------------------------------------------------------------------*/
#*  !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  WARNING !!!   */
#*    -------------------------------------------------------------    */
#*  The recette can't be compiled in `-unsafe' mode (due to try forms) */
#*---------------------------------------------------------------------*/
VERBOSE		= 
EFLAGS		= 
BFLAGS          = $(VERBOSE) -cg -g -O3 -q -Wall -init-lib \
                  -eval "(set! *indent* (= 1 2))"

BCFLAGS         = $(BFLAGS) -afile .afile-c
                  

BJVMFLAGS       = $(BFLAGS) -afile .afile-jvm -jfile .jfile-jvm -g
BPURIFYJVMFLAGS	=

BDNETFLAGS	= $(BFLAGS) -afile .afile-dotnet -jfile .jfile-dotnet -g -dotnet-managed -init-read -dotnet-mono-workaround -dotnet-pnet-workaround

#*---------------------------------------------------------------------*/
#*    Les objects destinations                                         */
#*---------------------------------------------------------------------*/
ICOMMON		= vital bps bool list vector srfi4 struct print \
                  string kwote case bexit vararity apply \
                  globalis filtre rgc_trap rgc_jm rgc_insert port \
                  read tail sqic eval inline match letrec macro \
                  flonum number bignum bchar define error cse \
                  kapture include 0cfa alias alias_aux main \
                  rgc_eval rgc hash module import1 import2 \
                  cfa2 cell hygiene wind dsssl sua peek callcc fringe \
                  unicode optim pregexp lalr system date process \
                  mmap weakptr crypto crc

OCOMMON		= object object1 object2 object3 object4 object5

COMMON		= $(ICOMMON) $(OCOMMON)

C_SPECIFIC	= external_c big_file
C_OBJECTS	= $(COMMON:%=%.$(C_OBJ_EXTENSION)) $(C_SPECIFIC:%=%.$(C_OBJ_EXTENSION))
OBJECT_C_FOREIGN= c-file.$(C_OBJ_EXTENSION)

JVM_SPECIFIC	= external
JVM_OBJECTS	= $(COMMON:%=%.class) $(JVM_SPECIFIC:%=%.class)
OBJECT_JVM_FORE	= Point.class ArrayTest.class Intf.class

DOTNET_SPECIFIC = external
DOTNET_OBJECTS  = $(COMMON:%=%.obj) $(DOTNET_SPECIFIC:%=%.obj)
OBJECT_DOTNET_CS= ArrayTest.cs foo.cs Intf.cs Point.cs
OBJECT_DOTNET_IL= objs_dotnet/patch.obj

OBJECT_DOTNET_FORE= $(OBJECT_DOTNET_CS) $(OBJECT_DOTNET_IL) 
IL_OBJECTS	= $(OBJECT_DOTNET_CS:%.cs=%.il) \
                  $(DOTNET_OBJECTS:%.obj=%.il)

LN_OBJECTS	= $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.ln)

C_TMPS		= $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.c)

SOURCE_FILES	= $(COMMON:%=%.scm) \
                  $(C_SPECIFIC:%=%.scm) \
                  $(JVM_SPECIFIC:%=%.scm) \
                  $(OBJECT_JVM_FORE:%.class=%.java) \
                  $(OBJECT_DOTNET_CS:%.obj=%.cs) \
                  $(OBJECT_DOTNET_IL:objs_dotnet/%.obj=%.il) \
                  c-file.c

POPULATION	= $(SOURCE_FILES) \
                  Makefile \
                  test.sch \
                  c-file.h \
		  include.sch \
		  include2.sch \
		  misc/dump.c \
		  misc/dump.dotnet \
		  misc/dump.jvm \
		  misc/input.txt \
		  misc/jm.txt \
		  misc/trap.txt

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .$(C_OBJ_EXTENSION) .scm .c .class .java .ln .il .obj

#*---------------------------------------------------------------------*/
#*    recette                                                          */
#*---------------------------------------------------------------------*/
recette: recette-c

recette-c: .etags .afile-c objs_c
	@ $(MAKE) recette-bin

recette-bin: $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%)
	$(BIGLOO) $(BCFLAGS) $(EFLAGS) -o recette $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%)
	@ echo "Recette (C) Done..."
	@ echo "-------------------------------"

new: 
	$(MAKE) BIGLOO=bigloo.new

static: recette-static
recette-static: .afile-c objs_c $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%)
	$(BIGLOO) $(BCFLAGS) $(EFLAGS) -o $@ $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%) -static-bigloo
	@ echo "Static Recette Done..."
	@ echo "-------------------------------"

jvm: recette-jvm-nojar
recette-jvm-nojar: .jfile-jvm .afile-jvm 
	@ $(MAKE) recette-jvm

recette-jvm: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	$(BIGLOO) -jvm -jvm-directory objs_jvm -jvm-classpath "objs_jvm" -no-jvm-jar $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	@ echo "Recette (JVM) Done..."
	@ echo "-------------------------------"

jvm-jar: recette-jvm-jar
recette-jvm-jar: .jfile-jvm .afile-jvm 
	@ $(MAKE) recette-jar

recette-jar: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	$(BIGLOO) -jvm -jvm-directory objs_jvm -jvm-classpath "objs_jvm" -jvm-jar $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $(OBJECT_JVM_FORE:%=objs_jvm/%) $(JVM_OBJECTS:%=objs_jvm/%)
	@ echo "Recette (JVM jar) Done..."
	@ echo "-------------------------------"

djvm: recette-jvm-debug
recette-jvm-debug: .jfile-dotnet .afile-dotnet 
	@ $(MAKE) recette-class-debug

recette-class-debug: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	$(BIGLOO) -jvm $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	@ echo "Recette (JVM debug) Done..."
	@ echo "-------------------------------"

jvm-purify:
	$(MAKE) jvm BPURIFYJVMFLAGS=-jvm-purify

.net: dotnet
dotnet: recette-exe

recette-exe: .jfile-dotnet .afile-dotnet 
	@ $(MAKE) recette-dotnet

recette-dotnet: objs_dotnet $(DOTNET_OBJECTS:%=objs_dotnet/%) $(OBJECT_DOTNET_FORE)
	$(BIGLOO) $(EFLAGS) -dotnet -dotnet-clr $(DOTNETCLR) -dotnet-ld-style $(DOTNETLDSTYLE) -dotnet-clr-style $(DOTNETCLRSTYLE) -dotnet-clr-opt "$(DOTNETCLROPT)" -o $@$(SCRIPTEXTENSION) $(DOTNET_OBJECTS:%=objs_dotnet/%) $(OBJECT_DOTNET_FORE)
	@ echo "Recette (DotNet) Done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    bee                                                              */
#*---------------------------------------------------------------------*/
getbinary:
	echo "recette"

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOODIR)/recette ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) cleanall
	@ $(MAKE) .jfile-jvm
	@ $(MAKE) .jfile-dotnet

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=recette/%)

#*---------------------------------------------------------------------*/
#*     touchall ...                                                    */
#*---------------------------------------------------------------------*/
touchall:
	@ touch *.scm c-file.c
	@ echo "touch done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: lintclean clean cleanall distclean

lintclean:
	@- $(RM) -f $(LN_OBJECTS)
	@- $(RM) -f all.lint

distclean: lintclean
	@- $(RM) -f -r objs_c
	@- $(RM) -f -r objs_jvm
	@- $(RM) -f -r objs_dotnet
	@- $(RM) -f recette
	@- $(RM) -f recette-static
	@- $(RM) -f recette.exe
	@- $(RM) -f recette-jvm
	@- $(RM) -f recette-jvm.bat
	@- $(RM) -f recette-jvm-jar
	@- $(RM) -f recette-jvm-jar.jar
	@- $(RM) -f recette-dotnet
	@- $(RM) -f recette-dotnet.bat
	@- $(RM) -f recette-dotnet.exe
	@- $(RM) -f recette.log
	@- $(RM) -f misc/binary.BIN
	@- $(RM) -f gc.log
	@- $(RM) -f *.tree
	@- $(RM) -f *.ast
	@- $(RM) -f *.class
	@- $(RM) -f *.jas
	@- $(RM) -f *.obj
	@- $(RM) -f *.escm
	@- $(RM) -f .afile-c .afile-jvm .afile-dotnet
	@- $(RM) -f .etags
	@- $(RM) -f Manifest*
	@- $(RM) -f dummydir
	@ find . \( -name '*[~%]'                   \
                       -o -name '.??*[~%]'          \
                       -o -name '#*#'               \
                       -o -name '?*#'               \
                       -o -name '*.BAK'             \
                       -o -name \*core \)           \
                     -type f -exec $(RM) {} \;   
	@ echo "Clean up done..."
	@ echo "-------------------------------"

clean: distclean
	@- $(RM) -f .jfile-jvm .jfile-dotnet

cleanall: clean

#*---------------------------------------------------------------------*/
#*    lint                                                             */
#*---------------------------------------------------------------------*/
lint: all.lint
	@ echo "lint done..."
	@ echo "-------------------------------"

all.lint: $(LN_OBJECTS)
	@- $(RM) -f all.lint
	@- touch all.lint
	@ ( for p in *.ln;                 \
            do echo "   " $$p >> all.lint; \
            cat $$p >> all.lint;           \
            done )

#*---------------------------------------------------------------------*/
#*    .etags                                                           */
#*---------------------------------------------------------------------*/
.etags:
	$(BTAGS) $(JVM_OBJECTS:%.class=%.scm) $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.scm) -o .etags

#*---------------------------------------------------------------------*/
#*    .jfile-jvm                                                       */
#*---------------------------------------------------------------------*/
.jfile-jvm:
	$(JFILE) $(JVM_OBJECTS:%.class=%.scm) -o .jfile-jvm

#*---------------------------------------------------------------------*/
#*    .jfile-dotnet                                                    */
#*---------------------------------------------------------------------*/
.jfile-dotnet:
	$(JFILE) $(DOTNET_OBJECTS:%.obj=%.scm) -o .jfile-dotnet

#*---------------------------------------------------------------------*/
#*    .afile-c                                                         */
#*---------------------------------------------------------------------*/
.afile-c:
	$(AFILE) $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.scm) -o .afile-c

#*---------------------------------------------------------------------*/
#*    .afile-jvm                                                       */
#*---------------------------------------------------------------------*/
.afile-jvm:
	$(AFILE) $(JVM_OBJECTS:%.class=%.scm) -o .afile-jvm

#*---------------------------------------------------------------------*/
#*    .afile-dotnet                                                    */
#*---------------------------------------------------------------------*/
.afile-dotnet:
	$(AFILE) $(DOTNET_OBJECTS:%.obj=%.scm) -o .afile-dotnet

#*---------------------------------------------------------------------*/
#*    .scm.$(C_OBJ_EXTENSION)                                          */
#*---------------------------------------------------------------------*/
objs_c/%.$(C_OBJ_EXTENSION): %.scm
	$(BIGLOO) -c $(BCFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    .scm.class                                                       */
#*---------------------------------------------------------------------*/
objs_jvm/%.class: %.scm
	$(BIGLOO) -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

#*---------------------------------------------------------------------*/
#*    .scm.obj                                                         */
#*---------------------------------------------------------------------*/
objs_dotnet/%.obj: %.scm
	$(BIGLOO) -dotnet $(BDNETFLAGS) $(EFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*    Specific compilations                                            */
#*---------------------------------------------------------------------*/
objs_c/callcc.$(C_OBJ_EXTENSION): callcc.scm
	$(BIGLOO) -call/cc -c $(BCFLAGS) $(EFLAGS) $< -o $@

objs_jvm/callcc.class: callcc.scm
	$(BIGLOO) -call/cc -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

objs_dotnet/callcc.obj: callcc.scm
	$(BIGLOO) -call/cc -dotnet $(BDNETFLAGS) $(EFLAGS) -c $< -o $@

objs_c/fringe.$(C_OBJ_EXTENSION): fringe.scm
	$(BIGLOO) -c $(BCFLAGS) -call/cc $(EFLAGS) $< -o $@

objs_jvm/fringe.class: fringe.scm
	$(BIGLOO) -call/cc -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

objs_dotnet/fringe.obj: fringe.scm
	$(BIGLOO) -call/cc -dotnet $(BDNETFLAGS) $(EFLAGS) -c $< -o $@

objs_c/wind.$(C_OBJ_EXTENSION): wind.scm
	$(BIGLOO) -c $(BCFLAGS) -call/cc $(EFLAGS) $< -o $@

objs_jvm/wind.class: wind.scm
	$(BIGLOO) -call/cc -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

objs_dotnet/wind.obj: wind.scm
	$(BIGLOO) -call/cc -dotnet $(BDNETFLAGS) $(EFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*    .c.$(C_OBJ_EXTENSION)                                            */
#*---------------------------------------------------------------------*/
objs_c/%.$(C_OBJ_EXTENSION): %.c
	@ echo $<
	$(CC) $(CFLAGS) -c -o $@ $<

#*---------------------------------------------------------------------*/
#*    .java.class                                                      */
#*---------------------------------------------------------------------*/
objs_jvm/%.class: %.java
	@ echo "$*.java:"
	(CLASSPATH=$$CLASSPATH:.:objs_jvm:$(BOOTLIBDIR)/bigloo_s.zip;\
         export CLASSPATH;\
         $(JAVAC) $(CYGWINJVMPATH) $(JVMRECETTEBOOTPATH) $(JCFLAGS) -d objs_jvm $*.java)

#*---------------------------------------------------------------------*/
#*    .il.obj                                                          */
#*---------------------------------------------------------------------*/
objs_dotnet/%.obj: %.il
	$(DOTNETASM) -o $@ $*.il

#*---------------------------------------------------------------------*/
#*    .c.ln                                                            */
#*---------------------------------------------------------------------*/
.c.ln:
	@ echo "$*.c:"
	@ $(LINT) $(LINTFLAGS) $*.c > $*.ln

#*---------------------------------------------------------------------*/
#*    sub-directories                                                  */
#*---------------------------------------------------------------------*/
objs_c:
	@ mkdir objs_c

objs_jvm:
	@ mkdir objs_jvm

objs_dotnet:
	@ mkdir objs_dotnet

#*---------------------------------------------------------------------*/
#*    misc/dump.XXX                                                    */
#*---------------------------------------------------------------------*/
misc/dump.jvm:
	($(MAKE) clean; $(MAKE) jvm; recette-jvm --dump misc/dump.jvm)
misc/dump.c:
	($(MAKE) clean; $(MAKE); recette --dump misc/dump.c)
misc/dump.dotnet:
	($(MAKE) clean; $(MAKE) dotnet; recette-dotnet --dump misc/dump.dotnet)
