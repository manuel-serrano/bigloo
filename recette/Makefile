#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/recette/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Nov  2 17:38:43 1992                          */
#*    Last change :  Wed Mar 11 05:28:28 2020 (serrano)                */
#*    Copyright   :  1992-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the recette                                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# which C compiler to be used
CC              = `$(BIGLOO) -eval '(begin (print *cc*) (exit 0))'`
# the shell to be used
SHELL           = /bin/sh
# lint
LINT		= lint
# lint options
LINTFLAGS	= -u -v -I$(BOOTLIBDIR)
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
                  -eval "(set! *indent* (= 1 2))" -gself \
                  -lib-dir $(BOOTLIBDIR)

BCFLAGS         = $(BFLAGS) -afile .afile-c
                  

BJVMFLAGS       = $(BFLAGS) -afile .afile-jvm -jfile .jfile-jvm -g
BPURIFYJVMFLAGS	=

#*---------------------------------------------------------------------*/
#*    Les objects destinations                                         */
#*---------------------------------------------------------------------*/
ICOMMON		= vital bps bool list vector srfi4 struct print \
                  string kwote case bexit vararity apply \
                  globalis filtre rgc_trap rgc_jm rgc_insert port \
                  read tail sqic eval inline match letrec macro \
                  flonum number bignum bchar define error cse \
                  kapture include cfa alias alias_aux main \
                  rgc_eval rgc hash module import1 import2 \
                  cfa2 cell hygiene wind dsssl sua peek callcc fringe \
                  unicode optim pregexp lalr system date process \
                  mmap weakptr crypto crc

OCOMMON		= object object_sans \
                  object1 object1_sans \
                  object2 object2_sans \
                  object3 object3_sans \
                  object5 object5_sans

COMMON		= $(ICOMMON) $(OCOMMON)

C_SPECIFIC	= external_c big_file
C_OBJECTS	= $(COMMON:%=%.$(C_OBJ_EXTENSION)) $(C_SPECIFIC:%=%.$(C_OBJ_EXTENSION))
OBJECT_C_FOREIGN= c-file.$(C_OBJ_EXTENSION)

JVM_SPECIFIC	= external
JVM_OBJECTS	= $(COMMON:%=%.class) $(JVM_SPECIFIC:%=%.class)
OBJECT_JVM_FORE	= Point.class ArrayTest.class Intf.class

LN_OBJECTS	= $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.ln)

C_TMPS		= $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.c)

SOURCE_FILES	= $(COMMON:%=%.scm) \
                  $(C_SPECIFIC:%=%.scm) \
                  $(JVM_SPECIFIC:%=%.scm) \
                  $(OBJECT_JVM_FORE:%.class=%.java) \
                  c-file.c

POPULATION	= $(SOURCE_FILES) \
                  Makefile \
                  test.sch \
                  eval.sch \
                  object.sch \
                  object5.sch \
                  c-file.h \
		  include.sch \
		  include2.sch \
		  misc/dump.c \
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
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) -o recette $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%)
	@ echo "Recette (C) Done..."
	@ echo "-------------------------------"

saw:
	$(MAKE) EFLAGS=-saw
new: 
	$(MAKE) BIGLOO=bigloo.new

new-jvm: 
	$(MAKE) jvm BIGLOO=bigloo.new

static: recette-static
recette-static: .afile-c objs_c $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%)
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) -o $@ $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%) -static-bigloo
	@ echo "Static Recette Done..."
	@ echo "-------------------------------"

jvm: recette-jvm-nojar
recette-jvm-nojar: .jfile-jvm .afile-jvm 
	@ $(MAKE) recette-jvm

recette-jvm: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	$(BIGLOO) $(BFLAGS) -jvm -jvm-directory objs_jvm -jvm-classpath "objs_jvm" -no-jvm-jar $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	@ echo "Recette (JVM) Done..."
	@ echo "-------------------------------"

jvm-jar: recette-jvm-jar
recette-jvm-jar: .jfile-jvm .afile-jvm 
	@ $(MAKE) recette-jar

recette-jar: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	$(BIGLOO) $(BFLAGS) -jvm -jvm-directory objs_jvm -jvm-classpath "objs_jvm" -jvm-jar $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $(OBJECT_JVM_FORE:%=objs_jvm/%) $(JVM_OBJECTS:%=objs_jvm/%)
	@ echo "Recette (JVM jar) Done..."
	@ echo "-------------------------------"

djvm: recette-jvm-debug
recette-jvm-debug: .jfile-jvm .afile-jvm
	@ $(MAKE) recette-class-debug

recette-class-debug: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	$(BIGLOO) $(BFLAGS) -jvm $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%)
	@ echo "Recette (JVM debug) Done..."
	@ echo "-------------------------------"

jvm-purify:
	$(MAKE) jvm BPURIFYJVMFLAGS=-jvm-purify

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
	@- $(RM) -f recette
	@- $(RM) -f recette-static
	@- $(RM) -f recette.exe
	@- $(RM) -f recette-jvm
	@- $(RM) -f recette-jvm.bat
	@- $(RM) -f recette-jvm-jar
	@- $(RM) -f recette-jvm-jar.jar
	@- $(RM) -f recette.log
	@- $(RM) -f misc/binary.BIN
	@- $(RM) -f gc.log
	@- $(RM) -f *.tree
	@- $(RM) -f *.ast
	@- $(RM) -f *.class
	@- $(RM) -f *.jas
	@- $(RM) -f *.obj
	@- $(RM) -f *.escm
	@- $(RM) -f .afile-c .afile-jvm
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
	@- $(RM) -f .jfile-jvm

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
	$(BGLTAGS) $(JVM_OBJECTS:%.class=%.scm) $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.scm) -o .etags

#*---------------------------------------------------------------------*/
#*    .jfile-jvm                                                       */
#*---------------------------------------------------------------------*/
.jfile-jvm:
	$(BGLJFILE) $(JVM_OBJECTS:%.class=%.scm) -o .jfile-jvm

#*---------------------------------------------------------------------*/
#*    .afile-c                                                         */
#*---------------------------------------------------------------------*/
.afile-c:
	$(BGLAFILE) $(C_OBJECTS:%.$(C_OBJ_EXTENSION)=%.scm) -o .afile-c

#*---------------------------------------------------------------------*/
#*    .afile-jvm                                                       */
#*---------------------------------------------------------------------*/
.afile-jvm:
	$(BGLAFILE) $(JVM_OBJECTS:%.class=%.scm) -o .afile-jvm

#*---------------------------------------------------------------------*/
#*    .scm.$(C_OBJ_EXTENSION)                                          */
#*---------------------------------------------------------------------*/
objs_c/%.$(C_OBJ_EXTENSION): %.scm
	$(BIGLOO) $(BFLAGS) -c $(BCFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    .scm.class                                                       */
#*---------------------------------------------------------------------*/
objs_jvm/%.class: %.scm
	$(BIGLOO) $(BFLAGS) -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

#*---------------------------------------------------------------------*/
#*    Specific compilations                                            */
#*---------------------------------------------------------------------*/
objs_c/callcc.$(C_OBJ_EXTENSION): callcc.scm
	$(BIGLOO) $(BFLAGS) -call/cc -c $(BCFLAGS) $(EFLAGS) $< -o $@

objs_jvm/callcc.class: callcc.scm
	$(BIGLOO) $(BFLAGS) -call/cc -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

objs_c/fringe.$(C_OBJ_EXTENSION): fringe.scm
	$(BIGLOO) $(BFLAGS) -c $(BCFLAGS) -call/cc $(EFLAGS) $< -o $@

objs_jvm/fringe.class: fringe.scm
	$(BIGLOO) $(BFLAGS) -call/cc -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

objs_c/wind.$(C_OBJ_EXTENSION): wind.scm
	$(BIGLOO) $(BFLAGS) -c $(BCFLAGS) -call/cc $(EFLAGS) $< -o $@

objs_jvm/wind.class: wind.scm
	$(BIGLOO) $(BFLAGS) -call/cc -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

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

#*---------------------------------------------------------------------*/
#*    misc/dump.XXX                                                    */
#*---------------------------------------------------------------------*/
misc/dump.jvm:
	($(MAKE) clean; $(MAKE) jvm; recette-jvm --dump misc/dump.jvm)
misc/dump.c:
	($(MAKE) clean; $(MAKE); recette --dump misc/dump.c)
