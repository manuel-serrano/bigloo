#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/bpm/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Dec 13 10:04:17 2006                          */
#*    Last change :  Wed Jul  8 14:39:59 2026 (serrano)                */
#*    Copyright   :  2006-26 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The makefile to build the Bigloo bglbpm command.                 */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
BGLBPM		= bpm
BACKEND		= auto

# the executable used to compile
EFLAGS		= 

AFILE_BIN	= $(BGLBUILDBINDIR)/bglafile.sh
JFILE_BIN	= $(BGLBUILDBINDIR)/bgljfile.sh
BTAGS_BIN	= $(BGLBUILDBINDIR)/bgltags.sh
DEPEND_BIN	= $(BGLBUILDBINDIR)/bgldepend.sh

UNZIP		= unzip
JAR		= jar cfm

# standard Jvm library
JVMZIPLIBS	= $(BOOTLIBDIR)/bigloo_s.zip \
                  $(BOOTLIBDIR)/bigloosqlite_s-$(RELEASE).zip \
                  $(BOOTLIBDIR)/bigloobpmlib_s-$(RELEASE).zip

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
SOURCES		= main config database utils
FILES		= $(SOURCES:%=%.bgl)

MODULES		= $(SOURCES)

INCLUDES	= 

#*---------------------------------------------------------------------*/
#*    The bglbpm executable                                            */
#*---------------------------------------------------------------------*/
.PHONY: build auto build.auto native build.native jvm build.jvm

build: build.$(BACKEND)

auto: build.auto
build.auto:
	if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	  $(MAKE) build.native; \
        else \
	  if [ "$(JVMBACKEND)" = "yes" ]; then \
	    $(MAKE) build.jvm; \
          else \
            $(MAKE) build.wasm; \
          fi \
        fi

#*---------------------------------------------------------------------*/
#*    native version                                                   */
#*---------------------------------------------------------------------*/
native: build.native
build.native: o
	@ $(MAKE) $(BOOTBINDIR)/$(BGLBPM)

$(BOOTBINDIR)/$(BGLBPM): $(MODULES:%=o/%.o)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $+

#*---------------------------------------------------------------------*/
#*    jvm version                                                      */
#*---------------------------------------------------------------------*/
jvm: build.jvm
build.jvm: o/class_s/bigloo/bglbpm .etags .jfile jvm-stdlibs
	@ $(MAKE) $(BOOTBINDIR)/$(BGLBPM).jar

$(BOOTBINDIR)/$(BGLBPM).jar: $(MODULES:%=o/class_s/bigloo/bglbpm/%.class) \
     META-INF/MANIFEST.MF
	$(JAR) $@ META-INF/MANIFEST.MF -C o/class_s .

META-INF/MANIFEST.MF: META-INF
	echo "Manifest-Version: 1.0" > $@
	echo "Main-Class: bigloo/bglbpm/main" >> $@
	echo "Created-By: Bigloo $(RELEASE)" >> $@

META-INF:
	mkdir -p $@

jvm-stdlibs: o/class_s
	for p in $(JVMZIPLIBS); do \
          if [ -f $$p ]; then \
             $(UNZIP) -d o/class_s -o -u $$p; \
          else \
             echo "*** ERROR: Can't find zip file $$p..." || exit 1; \
          fi; \
        done

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=bglbpm/%)

#*---------------------------------------------------------------------*/
#*    directory dependencies                                           */
#*---------------------------------------------------------------------*/
o:
	mkdir -p $@

o/class_s:
	mkdir -p $@

o/class_s/bigloo/bglbpm:
	mkdir -p $@

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .bgl .o

#*---------------------------------------------------------------------*/
#*    The implicit `.bgl.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.bgl
	$(BIGLOO) -c $(BFLAGS) $(EFLAGS) $< -o $@

o/class_s/bigloo/bglbpm/%.class: %.bgl
	$(BIGLOO) -c -jvm $(BFLAGS) $(BJVMFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.jfile: $(FILES)
	@ $(JFILE_BIN) -o .jfile -pbase bigloo.bglbpm $(FILES) configure.bgl

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: 
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          if [ "$$num " != " " ]; then \
            head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux; \
          else \
            cp Makefile /tmp/Makefile.aux; \
          fi)
	@ $(DEPEND_BIN) -strict-obj-dir o -fno-mco $(FILES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(BOOTBINDIR)/bglbpm$(EXE_SUFFIX) $(DESTDIR)$(BINDIR); \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglbpm$(EXE_SUFFIX); \
        fi

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglbpm$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare bglbpm for a distribution                             */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOODIR)/bglbpm ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ cp $(BOOTDIR)/bglbpm/configure.bgl configure.bgl

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -rf o
	$(RM) -rf META-INF

devclean: clean
	$(RM) -f $(BOOTBINDIR)/$(BGLBPM)
	$(RM) -f $(BOOTBINDIR)/$(BGLBPM).jar

cleanall: devclean

distclean: devclean

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
$(OBJECTS):

#bdepend stop
