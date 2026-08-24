#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/doc/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Mon Aug 24 23:49:46 2026 (serrano)                */
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
UFLAGS = -g
BFLAGS = $(VERBOSE) -cg $(UFLAGS) -O3 -q -Wall -init-lib \
  -lib-dir $(BOOTLIBDIR) -no-auto-link-main -module5 -I src
BCFLAGS =
JIGLOO = $(BOOTBINDIR)/jigloo

#*---------------------------------------------------------------------*/
#*    objects                                                          */
#*---------------------------------------------------------------------*/
COMMON = bgldoc fontifier

C_OBJECTS = $(COMMON:%=%.o)

#*---------------------------------------------------------------------*/
#*    Documentation                                                    */
#*---------------------------------------------------------------------*/
MD = argsparse bigloo bool browser csv date digest graph hash error int \
 internet io mmap net number os pair parser port process real regexp \
 serial sqlite ssl string symbol tar trace thread vector weakptr web zip \
 c jvm wasm \
 condexpand core match module5 module4 object trace \
 bgldemangle bglmem bglperf

EX_C = fib sock1 sock2 sock3 sock4 wrap function infix var grep untar wget trace
EX_J = fib Point.bgh
EX_W = 
EX = $(EX_C:%=c/%) $(EX_J:%=jvm/%) $(EX_W:%=wasm/%)

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .o .scm .c .class .java

#*---------------------------------------------------------------------*/
#*    tests                                                            */
#*---------------------------------------------------------------------*/
all: doc

doc: objs bin index.html download.html idx.html index.json
	$(MAKE) bin/bgldoc

bin/bgldoc: $(C_OBJECTS:%=objs/%)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

index.html: $(MD:%=%.html) $(EX_C:%=objs/c/%.ast.coerce)

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
#*    Compilation rules                                                */
#*---------------------------------------------------------------------*/
objs/%.o: src/%.bgl
	$(BIGLOO) -c $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

objs/c/fib.ast.coerce: examples/c/fib.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/sock1.ast.coerce: examples/c/sock1.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/sock2.ast.coerce: examples/c/sock2.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/sock3.ast.coerce: examples/c/sock3.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/sock4.ast.coerce: examples/c/sock4.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/wrap.ast.coerce: examples/c/wrap.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/function.ast.coerce: examples/c/function.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/infix.ast.coerce: examples/c/infix.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/var.ast.coerce: examples/c/var.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/grep.ast.coerce: examples/c/grep.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/untar.ast.coerce: examples/c/untar.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/wget.ast.coerce: examples/c/wget.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/c/trace.ast.coerce: examples/c/trace.bgl objs/c
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -coerce

objs/java/fib.ast.coerce: examples/java/fib.bgl objs/java
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -jvm -coerce

objs/wasm/fib.ast.coerce: examples/wasm/fib.bgl objs/wasm
	$(BIGLOO) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ -wasm -coerce

%.html: %.md bin/bgldoc
	bin/bgldoc -t libs $< > $@ || rm -f $@

condexpand.html: condexpand.md bin/bgldoc 
	bin/bgldoc -t lang $< > $@ || rm -f $@

match.html: match.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

parser.html: parser.md bin/bgldoc
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

download.html: download.md bin/bgldoc
	bin/bgldoc -t download $< > $@ || rm -f $@

idx.html: index.sexp bin/bgldoc
	bin/bgldoc -t index $< > $@ || rm -f $@

index.json: index.sexp
	bigloo -i src/bglidx.bgl $+ > $@ || rm -f $@

bgldemangle.html: bgldemangle.md bin/bgldoc
	bin/bgldoc -t dev $< > $@ || rm -f $@

bglmem.html: bglmem.md bin/bgldoc
	bin/bgldoc -t dev $< > $@ || rm -f $@

bglperf.html: bglperf.md bin/bgldoc
	bin/bgldoc -t dev $< > $@ || rm -f $@

examples/c/trace-full.txt: objs/c/trace
	BIGLOOTRACE="foo loop liip" objs/c/trace 2> $@

examples/c/trace-loop.txt: objs/c/trace
	BIGLOOTRACE="loop" objs/c/trace 2> $@

objs/c/trace: examples/c/trace.bgl objs/c
	$(BIGLOO) -g $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@ 

trace.html: examples/c/trace-full.txt examples/c/trace-loop.txt

#*---------------------------------------------------------------------*/
#*    sub-directories                                                  */
#*---------------------------------------------------------------------*/
objs:
	@ mkdir -p $@

objs/c:
	@ mkdir -p $@

objs/java:
	@ mkdir -p $@

objs/wasm:
	@ mkdir -p $@

bin:
	@ mkdir -p $@

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean_html:
	rm -rf $(MD:%=%.html)

clean:
	rm -rf objs bin
	rm -rf $(MD:%=%.html)
	rm -f index.sexp
	rm -f index.json
	rm -f examples/c/trace examples/c/trace-full.txt examples/c/trace-loop.txt
