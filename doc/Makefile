#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/doc/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Sun May 31 09:24:12 2026 (serrano)                */
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
BFLAGS = $(VERBOSE) -cg -g -O3 -q -Wall -init-lib \
  -eval "(set! *indent* (= 1 2))" -gself \
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
MD = bool date hash int io pair port real string thread vector \
 module5 module4 object

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .o .scm .c .class .java

#*---------------------------------------------------------------------*/
#*    tests                                                            */
#*---------------------------------------------------------------------*/
all: doc

doc: objs_c bin index.html
	$(MAKE) bin/bgldoc

bin/bgldoc: $(C_OBJECTS:%=objs_c/%)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^
	@ echo "\e[1;34m$@\e[0m done."

index.html: $(MD:%=%.html)

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

%.html: %.md bin/bgldoc
	bin/bgldoc -t libs $< > $@ || rm -f $@

module5.html: module5.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

module4.html: module4.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

object.html: object.md bin/bgldoc
	bin/bgldoc -t lang $< > $@ || rm -f $@

index.html: index.md bin/bgldoc
	bin/bgldoc -t home $< > $@ || rm -f $@

#*---------------------------------------------------------------------*/
#*    sub-directories                                                  */
#*---------------------------------------------------------------------*/
objs_c:
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
