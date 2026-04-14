#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0a/test/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Tue Apr 14 08:37:49 2026 (serrano)                */
#*    Copyright   :  2026 manuel serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the non-regression tests                   */
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
CC = `$(BIGLOO) -eval '(begin (print *cc*) (exit 0))'`
# the shell to be used
SHELL = /bin/sh
#
JVMCLASSPATH = -jvm-bigloo-classpath $(BOOTLIBDIR)

#*---------------------------------------------------------------------*/
#*  !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  WARNING !!!   */
#*    -------------------------------------------------------------    */
#*  The recette can't be compiled in `-unsafe' mode (due to try forms) */
#*---------------------------------------------------------------------*/
VERBOSE = 
EFLAGS = 
BFLAGS = $(VERBOSE) -cg -g -O3 -q -Wall -init-lib \
  -eval "(set! *indent* (= 1 2))" -gself \
  -lib-dir $(BOOTLIBDIR) -no-auto-link-main -module5

BCFLAGS =
BWFLAGS = -wasm                  
BJFLAGS = -jfile .jfile-jvm -g

#*---------------------------------------------------------------------*/
#*    objects                                                          */
#*---------------------------------------------------------------------*/
COMMON = main string vector

C_OBJECTS = $(COMMON:%=%.$(C_OBJ_EXTENSION)) $(C_SPECIFIC:%=%.$(C_OBJ_EXTENSION))
W_OBJECTS = $(COMMON:%=%.wat) $(W_SPECIFIC:%=%.wat)

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .$(C_OBJ_EXTENSION) .scm .c .class .java .obj

#*---------------------------------------------------------------------*/
#*    tests                                                            */
#*---------------------------------------------------------------------*/
all: c

c: objs_c
	$(MAKE) test_c
wasm: objs_wasm
	$(MAKE) test_wasm
jvm: objs_jvm
	$(MAKE) test_jvm

test_c: $(C_OBJECTS:%=objs_c/%)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

test_wasm: $(W_OBJECTS:%=objs_wasm/%)
	$(BIGLOO) $(BFLAGS) $(BWFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

#*---------------------------------------------------------------------*/
#*    .scm.$(C_OBJ_EXTENSION)                                          */
#*---------------------------------------------------------------------*/
objs_c/%.$(C_OBJ_EXTENSION): %.bgl test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

objs_wasm/%.wat: %.bgl test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BWFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    sub-directories                                                  */
#*---------------------------------------------------------------------*/
objs_c:
	@ mkdir -p $@
objs_jvm:
	@ mkdir -p $@
objs_wasm:
	@ mkdir -p $@

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	rm -rf objs_c objs_jvm objs_wasm
	rm -f test_c test_wasm test_jvm
	rm -f $(C_OBJECTS) $(W_OBJECTS) $(J_OBJECTS)
