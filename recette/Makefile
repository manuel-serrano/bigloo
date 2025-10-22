#*=====================================================================*/
#*    serrano/prgm/project/bigloo/wasm/recette/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Nov  2 17:38:43 1992                          */
#*    Last change :  Wed Oct 22 10:46:45 2025 (serrano)                */
#*    Copyright   :  1992-2025 Manuel Serrano, see LICENSE file        */
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

BCFLAGS         = -afile .afile-c
                  

BJVMFLAGS       = $(BFLAGS) -afile .afile-jvm -jfile .jfile-jvm -g
BPURIFYJVMFLAGS	=

BWASMFLAGS      = $(BFLAGS) -afile .afile-wasm -g

#*---------------------------------------------------------------------*/
#*    Les objects destinations                                         */
#*---------------------------------------------------------------------*/
ICOMMON		= utils vital bps bool list vector srfi4 struct print \
                  string kwote case bexit vararity apply \
                  globalis filtre rgc_trap rgc_jm rgc_insert port \
                  read tail sqic eval inline match letrec macro \
                  flonum number bignum bchar define error cse \
                  kapture include cfa alias alias_aux \
                  rgc_eval rgc hash module import1 import2 \
                  cfa2 cell hygiene wind dsssl sua peek callcc fringe \
                  unicode optim pregexp lalr system date process \
                  mmap input_mmap_port weakptr crypto crc ssr

OCOMMON		= object object_sans \
                  object1 object1_sans \
                  object2 object2_sans \
                  object3 object3_sans \
                  object5 object5_sans

MODULE5		= module5 \
                  module5_ex0 \
                  module5_ex1 \
                  module5_ex2 \
                  module5_ex3 \
                  module5_ex4 \
                  module5_ex5 \
                  module5_ex6 \
                  module5_ex7 \
                  module5_ex8 \
                  module5_ex9 \
                  module5_ex10 \
                  module5_ex11 \
                  module5_ex12 \
                  module5_ex13 \
                  module5_ex14 \
                  module5_ex15 \
                  module5_ex16 \
                  module5_ex17 \
                  module5_ex18

COMMON		= $(ICOMMON) $(OCOMMON)

C_SPECIFIC	= external_c big_file
C_OBJECTS	= $(COMMON:%=%.$(C_OBJ_EXTENSION)) $(C_SPECIFIC:%=%.$(C_OBJ_EXTENSION))
C_OBJECTS5	= $(MODULE5:%=%.$(C_OBJ_EXTENSION))
OBJECT_C_FOREIGN= c-file.$(C_OBJ_EXTENSION)

JVM_SPECIFIC	= external_jvm
JVM_OBJECTS	= $(COMMON:%=%.class) $(JVM_SPECIFIC:%=%.class)
OBJECT_JVM_FORE	= Point.class ArrayTest.class Intf.class

WASM_OBJECTS	= $(COMMON:%=%.wat) $(WASM_SPECIFIC:%=%.wat)

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
		  misc/dump \
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
all: recette-c

recette-c: .etags .afile-c objs_c objs5_c
	@ $(MAKE) recette4
	@ $(MAKE) recette5

recette4: .etags .afile-c objs_c
	$(MAKE) recette
recette: $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%) objs_c/main.o
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

recette5: $(C_OBJECTS:%=objs_c/%) $(C_OBJECTS5:%=objs5_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%) objs5_c/main5.o
	$(BIGLOO) -no-auto-link-main -module5 $(BFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

saw:
	$(MAKE) EFLAGS=-saw
new: 
	$(MAKE) BIGLOO=$(BOOTBINDIR)/bigloo.new

new-jvm:  objs_jvm
	$(MAKE) jvm BIGLOO=$(BOOTBINDIR)/bigloo.new

gstatic: recette-static
recette-static: .afile-c objs_c $(C_OBJECTS:%=objs_c/%) $(OBJECT_C_FOREIGN:%=objs_c/%) objs_c/main.o
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) -o $@ $^ -static-bigloo
	@ echo "\e[1;35m$@\e[0m done."

wasm: objs_wasm .afile-wasm
	$(MAKE) recette-wasm

wasm-static: objs_wasm .afile-wasm
	$(MAKE) recette-wasm-static

recette-wasm: $(WASM_OBJECTS:%=objs_wasm/%) objs_wasm/main.wasm
	$(BIGLOO) -wasm $(BFLAGS) $(BCFLAGS) $(EFLAGS) -o recette.wasm $^
	@ echo "\e[1;36mrecette.wasm\e[0m done."

recette-wasm-static: $(WASM_OBJECTS:%=objs_wasm/%)
	$(BIGLOO) -static-bigloo -wasm $(BFLAGS) $(BCFLAGS) $(EFLAGS) -o recette.wasm-static $^
	@ echo "\e[1;36mrecette.wasm-static\e[0m done."

jvm: recette-jvm-nojar
recette-jvm-nojar: .jfile-jvm .afile-jvm .afile-c objs_jvm
	@ $(MAKE) recette-jvm

recette-jvm: $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%) objs_jvm/main.class
	$(BIGLOO) $(BFLAGS) -jvm -jvm-directory objs_jvm -jvm-classpath "objs_jvm" -no-jvm-jar $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $^
	@ echo "\e[1;37m$@$(SCRIPTEXTENSION)\e[0m done."

jvm-jar: recette-jvm-jar
recette-jvm-jar: .jfile-jvm .afile-jvm 
	@ $(MAKE) recette-jar

recette-jar: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%) objs_jvm/main.class
	$(BIGLOO) $(BFLAGS) -jvm -jvm-directory objs_jvm -jvm-classpath "objs_jvm" -jvm-jar $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $^
	@ echo "\e[1;38m$@$(SCRIPTEXTENSION)\e[0m done."

djvm: recette-jvm-debug
recette-jvm-debug: .jfile-jvm .afile-jvm
	@ $(MAKE) recette-class-debug

recette-class-debug: objs_jvm $(JVM_OBJECTS:%=objs_jvm/%) $(OBJECT_JVM_FORE:%=objs_jvm/%) objs_jvm/main.class
	$(BIGLOO) $(BFLAGS) -jvm $(BCFLAGS) $(EFLAGS) $(JVMCLASSPATH) -o $@$(SCRIPTEXTENSION) $^
	@ echo "\e[1;39m$@$(SCRIPTEXTENSION)\e[0m done."

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
	@- $(RM) -f -r objs5_c
	@- $(RM) -f -r objs_jvm
	@- $(RM) -f -r objs5_jvm
	@- $(RM) -f -r objs_wasm
	@- $(RM) -f -r objs5_wasm
	@- $(RM) -f recette
	@- $(RM) -f recette5
	@- $(RM) -f recette-static
	@- $(RM) -f recette.exe
	@- $(RM) -f recette-jvm
	@- $(RM) -f recette-jvm.bat
	@- $(RM) -f recette-jvm-jar
	@- $(RM) -f recette-jvm-jar.jar
	@- $(RM) -f recette.wasm
	@- $(RM) -f recette.wasm-static
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
	$(BGLJFILE) $(JVM_OBJECTS:%.class=%.scm) main.scm -o .jfile-jvm

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
#*    .afile-wasm                                                      */
#*---------------------------------------------------------------------*/
.afile-wasm:
	$(BGLAFILE) $(WASM_OBJECTS:%.wat=%.scm) -o .afile-wasm

#*---------------------------------------------------------------------*/
#*    .scm.$(C_OBJ_EXTENSION)                                          */
#*---------------------------------------------------------------------*/
objs_c/%.$(C_OBJ_EXTENSION): %.scm
	$(BIGLOO) $(BFLAGS) -c $(BCFLAGS) $(EFLAGS) $< -o $@

objs5_c/%.$(C_OBJ_EXTENSION): %.scm
	$(BIGLOO) $(BFLAGS) -module5 -c $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    .scm.class                                                       */
#*---------------------------------------------------------------------*/
objs_jvm/%.class: %.scm
	$(BIGLOO) $(BFLAGS) -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

objs5_jvm/%.class: %.scm
	$(BIGLOO) $(BFLAGS) -module5 -jvm $(BJVMFLAGS) $(BPURIFYJVMFLAGS) $(EFLAGS) -c $< -jvm-directory objs_jvm -o $(@F)

#*---------------------------------------------------------------------*/
#*    .scm.wat                                                         */
#*---------------------------------------------------------------------*/
objs_wasm/%.wat: %.scm
	$(BIGLOO) $(BFLAGS) -wasm $(BWASMFLAGS) $(EFLAGS) -c $< -o $@

objs5_wasm/%.wat: %.scm
	$(BIGLOO) $(BFLAGS) -module5 -wasm $(BWASMFLAGS) $(EFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*    Specific compilations                                            */
#*---------------------------------------------------------------------*/
objs5_c/main5.o: main.scm
	$(BIGLOO) $(BFLAGS) -module5 -c $(EFLAGS) $< -o $@

objs5_c/module5_ex12.o: module5_ex12.scm
	$(BIGLOO) $(BFLAGS) -c $(EFLAGS) $< -o $@

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

objs_jvm/Point.class: objs_jvm/external_jvm.class

objs_wasm/callcc.wat: callcc.scm
	$(BIGLOO) $(BFLAGS) -call/cc -wasm $(BWASMFLAGS) $(EFLAGS) -c $< -o $@

objs_wasm/fringe.wat: fringe.scm
	$(BIGLOO) $(BFLAGS) -call/cc -wasm $(BWASMFLAGS) $(EFLAGS) -c $< -o $@

objs_wasm/wind.wat: wind.scm
	$(BIGLOO) $(BFLAGS) -call/cc -wasm $(BWASMFLAGS) $(EFLAGS) -c $< -o $@

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
	@ mkdir -p $@
objs5_c:
	@ mkdir -p $@
objs_jvm:
	@ mkdir -p $@
objs5_jvm:
	@ mkdir -p $@
objs_wasm:
	@ mkdir -p $@
objs5_wasm:
	@ mkdir -p $@

#*---------------------------------------------------------------------*/
#*    misc/dump.XXX                                                    */
#*---------------------------------------------------------------------*/
misc/dump:
	($(MAKE) clean; $(MAKE); recette --dump misc/dump)
