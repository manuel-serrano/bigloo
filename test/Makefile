#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0a/test/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Sat May  2 10:30:40 2026 (serrano)                */
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
CC = `$(BIGLOO) -eval '(begin (print *cc*) (exit 0))'`
SHELL = /bin/sh

#*---------------------------------------------------------------------*/
#*  !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  WARNING !!!   */
#*    -------------------------------------------------------------    */
#*  The recette can't be compiled in `-unsafe' mode (due to try forms) */
#*---------------------------------------------------------------------*/
EFLAGS = 
BFLAGS = -cg -g -O3 -q -Wall -init-lib -gself \
  -lib-dir $(BOOTLIBDIR) -no-auto-link-main

BLFLAGS = -I src
BCFLAGS =
BWFLAGS = -wasm
BJFLAGS = -jvm

#*---------------------------------------------------------------------*/
#*    objects                                                          */
#*---------------------------------------------------------------------*/
COMMON = main hash io pair port string vector

C_OBJECTS = $(COMMON:%=%.o) $(C_SPECIFIC:%=%.o)
J_OBJECTS = $(COMMON:%=%.class)
W_OBJECTS = $(COMMON:%=%.wat) $(W_SPECIFIC:%=%.wat)

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .o .scm .c .class .java .wasm .wat

#*---------------------------------------------------------------------*/
#*    tests                                                            */
#*---------------------------------------------------------------------*/
all: c jvm wasm
	$(MAKE) c
	if [ "$(JVMBACKEND)" = "yes" ]; then \
	  $(MAKE) jvm; \
        fi
	if [ "$(WASMBACKEND)" = "yes" ]; then \
	  $(MAKE) wasm; \
        fi

check: check_c check_jvm check_wasm

c: objs_c
	$(MAKE) test_c

test_c: $(C_OBJECTS:%=objs_c/%)
	$(BIGLOO) $(BLFLAGS) $(BFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

check_c:
	@$(MAKE) c && ./test_c

jvm: objs_jvm .jfile
	$(MAKE) test_jvm

test_jvm: $(J_OBJECTS:%=objs_jvm/%)
	$(BIGLOO) $(BLFLAGS) $(BFLAGS) $(BJFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

check_jvm:
	@if [ "$(JVMBACKEND)" = "yes" ]; then \
	  $(MAKE) jvm && ./test_jvm; \
        fi

wasm: objs_wasm
	$(MAKE) test_wasm

test_wasm: $(W_OBJECTS:%=objs_wasm/%)
	$(BIGLOO) $(BLFLAGS) $(BFLAGS) $(BWFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

check_wasm:
	@if [ "$(WASMBACKEND)" = "yes" ]; then \
	  $(MAKE) wasm && BIGLOOLIBDIR=$(BOOTDIR)/lib/bigloo/$(RELEASE) BIGLOOJSDIR=$(BOOTDIR)/runtime/Wlib ./test_wasm; \
        fi

#*---------------------------------------------------------------------*/
#*    compilation rules                                                */
#*---------------------------------------------------------------------*/
objs_c/%.o: src/%.bgl src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

objs_wasm/%.wat: src/%.bgl src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BWFLAGS) $(EFLAGS) $< -o $@

objs_jvm/%.class: src/%.bgl src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BJFLAGS) $(EFLAGS) $< -o $@

.jfile:
	$(BGLJFILE) --strip 1 $(J_OBJECTS:%.class=src/%.bgl) -o $@

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
clean_c:
	rm -rf objs_c test_c

clean_jvm:
	rm -rf objs_jvm test_jvm .jfile

clean_wasm:
	rm -rf objs_wasm test_wasm

clean: clean_c clean_jvm clean_wasm
	$(BIGLOO) -cache-clean

