#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/test/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Wed Jul  1 07:35:31 2026 (serrano)                */
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
BFLAGS = -cg -O3 -q -Wall -g -lib-dir $(BOOTLIBDIR) -no-auto-link-main
BM$FLAGS = -module4

BLFLAGS = -I src -I src/modules
BCFLAGS =
BWFLAGS = -wasm
BJFLAGS = -jvm

#*---------------------------------------------------------------------*/
#*    objects                                                          */
#*---------------------------------------------------------------------*/
TESTS = main argsparse bigloo bignum bit bool char condexpand date dsssl \
  error hash int1 int2 io match mmap module net number object os pair \
  port real search string symbol thread unicode vector

MODULES4 = module4_ex12

MODULES5 = module5_ex14 module5_ex1 module5_ex6 module5_ex0 module5_ex15 \
  module5_ex2 module5_ex7 module5_ex10 module5_ex16 module5_ex3 \
  module5_ex8 module5_ex11 module5_ex17 module5_ex4 module5_ex9 \
  module5_ex13 module5_ex18 module5_ex5 module5_ex19 module5_ex20

COMMON=$(TESTS:%=%) $(MODULES4:%=modules/%) $(MODULES5:%=modules/%)

BACKEND_C = ssl0 extern_c c

BACKEND_J = ssl0 extern_jvm Intf Point

BACKEND_W = extern_wasm


C_OBJECTS = $(COMMON:%=%.o) $(BACKEND_C:%=%.o)
J_OBJECTS = $(COMMON:%=%.class) $(BACKEND_J:%=%.class)
W_OBJECTS = $(COMMON:%=%.wat) $(BACKEND_W:%=%.wat)

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .o .scm .c .class .java .wasm .wat

#*---------------------------------------------------------------------*/
#*    tests                                                            */
#*---------------------------------------------------------------------*/
all: c
	$(MAKE) c
	if [ "$(JVMBACKEND)" = "yes" ]; then \
	  $(MAKE) jvm; \
        fi
	if [ "$(WASMBACKEND)" = "yes" ]; then \
	  $(MAKE) wasm; \
        fi

check:
	$(MAKE) check_c
	$(MAKE) check_jvm
	$(MAKE) check_wasm

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

distrib: clean

#*---------------------------------------------------------------------*/
#*    compilation rules                                                */
#*---------------------------------------------------------------------*/
objs_c/%.o: src/%.bgl src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

objs_c/%.o: src/%.c
	$(CC) -c $(CFLAGS) $< -o $@

objs_wasm/%.wat: src/%.bgl src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BWFLAGS) $(EFLAGS) $< -o $@

objs_jvm/%.class: src/%.bgl src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BJFLAGS) $(EFLAGS) $< -o $@

objs_jvm/%.class: src/%.java
	$(JAVAC) $< -d objs_jvm -cp objs_jvm:$(BOOTLIBDIR)/bigloo_s.zip

objs_c/%.o: src/%.scm src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BM$FLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

objs_wasm/%.wat: src/%.scm src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BM4FLAGS) $(BWFLAGS) $(EFLAGS) $< -o $@

objs_jvm/%.class: src/%.scm src/test.bgh
	$(BIGLOO) -c $(BFLAGS) $(BM4FLAGS) $(BJFLAGS) $(EFLAGS) $< -o $@

.jfile:
	$(BGLJFILE) --strip 1 $(TESTS:%=src/%.bgl) $(MODULES5:%=src/modules/%.bgl) $(MODULES4:%=src/modules/%.scm) -o $@

objs_jvm/Point.class: objs_jvm/Intf.class objs_jvm/extern_jvm.class

#*---------------------------------------------------------------------*/
#*    sub-directories                                                  */
#*---------------------------------------------------------------------*/
objs_c:
	@ mkdir -p $@
	@ mkdir -p $@/modules

objs_jvm:
	@ mkdir -p $@
	@ mkdir -p $@/modules

objs_wasm:
	@ mkdir -p $@
	@ mkdir -p $@/modules

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

