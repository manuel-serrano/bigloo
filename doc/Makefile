#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/doc/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Sun Jun 28 08:53:50 2026 (serrano)                */
#*    Copyright   :  2026 manuel serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the documentation                          */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Variables                                                        */
#*---------------------------------------------------------------------*/
VERBOSE = 
EFLAGS =
xUFLAGS = -unsafe
BFLAGS = $(VERBOSE) -cg $(UFLAGS) -O3 -q -Wall -init-lib \
  -lib-dir $(BOOTLIBDIR) -no-auto-link-main -module5 -I src

BCFLAGS =

#*---------------------------------------------------------------------*/
#*    objects                                                          */
#*---------------------------------------------------------------------*/
COMMON = bgldoc fontifier

C_OBJECTS = $(COMMON:%=%.o)

#*---------------------------------------------------------------------*/
#*    Documentation                                                    */
#*---------------------------------------------------------------------*/
MD = argsparse bigloo bool c date hash error int io jvm net number os \
 pair port real ssl string thread vector wasm \
 condexpand match module5 module4 object

EX = c/fib c/sock1 c/sock2 c/sock3 c/sock4 c/wrap c/function c/infix \
  c/var \
  java/fib wasm/fib 

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .o .scm .c .class .java

#*---------------------------------------------------------------------*/
#*    tests                                                            */
#*---------------------------------------------------------------------*/
all: doc

doc: objs_c bin index.html idx.html
	$(MAKE) bin/bgldoc

bin/bgldoc: $(C_OBJECTS:%=objs_c/%)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

index.html: $(MD:%=%.html) $(EX:%=objs_c/%.ast.coerce)

distrib: clean

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install install-html install-index uninstall

install: install-html install-index

install-html:
	if [ $(DOCDIR) != $(BOOTDIR)/doc ]; then \
	    mkdir -p $(DOCDIR)/bigloo; \
	    chmod $(MODDIR) $(DOCDIR)/bigloo; \
	    echo "cp *.html $(DOCDIR)/bigloo"; \
	    cp *.html $(DOCDIR)/bigloo && \
            chmod $(MODFILE) $(DOCDIR)/bigloo/*.html; \
        fi

install-index:
	if [ $(DOCDIR) != $(BOOTDIR)/doc ]; then \
	    mkdir -p $(DOCDIR)/bigloo; \
	    chmod $(MODDIR) $(DOCDIR)/bigloo; \
	    echo "cp index.sexp $(DOCDIR)/bigloo"; \
	    cp index.sexp $(DOCDIR)/bigloo && \
            chmod $(MODFILE) $(DOCDIR)/bigloo/index.sexp; \
        fi

uninstall:
	if [ $(DOCDIR) != $(BOOTDIR)/manuals ]; then \
	  rm -rf $(DOCDIR)/bigloo;
	fi

#*---------------------------------------------------------------------*/
#*    .scm.o                                                           */
#*---------------------------------------------------------------------*/
objs_c/%.o: src/%.bgl
	$(BIGLOO) -c $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

objs_c/c/fib.ast.coerce: examples/c/fib.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/sock1.ast.coerce: examples/c/sock1.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/sock2.ast.coerce: examples/c/sock2.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/sock3.ast.coerce: examples/c/sock3.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/sock4.ast.coerce: examples/c/sock4.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/wrap.ast.coerce: examples/c/wrap.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/function.ast.coerce: examples/c/function.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/infix.ast.coerce: examples/c/infix.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/c/var.ast.coerce: examples/c/var.bgl objs_c/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs_c/java/fib.ast.coerce: examples/java/fib.bgl objs_c/java
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -jvm -coerce

objs_c/wasm/fib.ast.coerce: examples/wasm/fib.bgl objs_c/wasm
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -wasm -coerce

%.html: %.md bin/bgldoc
	bin/bgldoc -t libs $< > $@ || rm -f $@

condexpand.html: condexpand.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

match.html: match.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

module5.html: module5.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

module4.html: module4.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

object.html: object.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

c.html: c.md bin/bgldoc
	bin/bgldoc -t backends $< > $@ || rm -f $@

jvm.html: jvm.md bin/bgldoc
	bin/bgldoc -t backends $< > $@ || rm -f $@

wasm.html: wasm.md bin/bgldoc
	bin/bgldoc -t backends $< > $@ || rm -f $@

index.html: index.md bin/bgldoc
	bin/bgldoc -t home $< > $@ || rm -f $@

idx.html: index.sexp bin/bgldoc
	bin/bgldoc -t index $< > $@ || rm -f $@

#*---------------------------------------------------------------------*/
#*    sub-directories                                                  */
#*---------------------------------------------------------------------*/
objs_c:
	@ mkdir -p $@

objs_c/c:
	@ mkdir -p $@

objs_c/java:
	@ mkdir -p $@

objs_c/wasm:
	@ mkdir -p $@

bin:
	@ mkdir -p $@

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean_html:
	rm -rf $(MD:%=%.html)

clean:
	rm -rf objs_c bin
	rm -rf $(MD:%=%.html)
	rm -f index.sexp
