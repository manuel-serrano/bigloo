#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0a/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Mon Apr 13 12:38:34 2026                          */
#*    Last change :  Wed Apr 29 16:03:29 2026 (serrano)                */
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
COMMON = main fontifier

C_OBJECTS = $(COMMON:%=%.o)

#*---------------------------------------------------------------------*/
#*    Documentation                                                    */
#*---------------------------------------------------------------------*/
MD = hash io pair string vector

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

distrib: index.html

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install install-html install-index uninstall

install: install-html install-index

install-html:
	if [ $(DOCDIR) != $(BOOTDIR)/manuals ]; then \
	    mkdir -p $(DOCDIR)/bigloo; \
	    chmod $(MODDIR)$(DOCDIR)/bigloo; \
	    echo "cp *.html $(DOCDIR)/bigloo"; \
	    cp *.html $(DOCDIR)/bigloo && \
            chmod $(MODFILE) $(DOCDIR)/bigloo/*.html; \
          fi \
        fi

install-index:
	if [ $(DOCDIR) != $(BOOTDIR)/manuals ]; then \
	    mkdir -p $(DOCDIR)/bigloo; \
	    chmod $(MODDIR)$(DOCDIR)/bigloo; \
	    echo "cp index.sexp $(DOCDIR)/bigloo"; \
	    cp index.sexp $(DOCDIR)/bigloo && \
            chmod $(MODFILE) $(DOCDIR)/bigloo/index.sexp; \
          fi \
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
	bin/bgldoc -t stdlib $< > $@ || rm -f $@

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
clean:
	rm -rf objs_c bin
	rm -rf $(MD:%=%.html)
