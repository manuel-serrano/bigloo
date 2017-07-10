#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bglpkg/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Dec 13 10:04:17 2006                          */
#*    Last change :  Sun Jul  9 10:21:32 2017 (serrano)                */
#*    Copyright   :  2006-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The makefile to build the Bigloo bglpkg command.                 */
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
BGLPKG		= bglpkg
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
                  $(BOOTLIBDIR)/bigloopkglib_s-$(RELEASE).zip

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
SOURCES		= parseargs param utils package action extract tuning \
                  install testing chicken mzscheme snow man main
FILES		= $(SOURCES:%=%.scm)

MODULES		= $(SOURCES) configure

INCLUDES	= param.sch makefile.sch

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION 	= $(INCLUDES) $(FILES) configure.scm.in Makefile \
	          README etc/Makefile etc/make_lib.scm etc/init

#*---------------------------------------------------------------------*/
#*    The bglpkg executable                                            */
#*---------------------------------------------------------------------*/
.PHONY: build auto build.auto native build.native jvm build.jvm build.dotnet

build: build.$(BACKEND)

auto: build.auto
build.auto:
	if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	  $(MAKE) build.native; \
        else \
	  if [ "$(JVMBACKEND)" = "yes" ]; then \
	    $(MAKE) build.jvm; \
          else \
            $(MAKE) build.dotnet; \
          fi \
        fi

#*---------------------------------------------------------------------*/
#*    native version                                                   */
#*---------------------------------------------------------------------*/
native: build.native
build.native: o .afile .etags
	@ $(MAKE) $(BOOTBINDIR)/$(BGLPKG)

$(BOOTBINDIR)/$(BGLPKG): $(MODULES:%=o/%.o)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $+

#*---------------------------------------------------------------------*/
#*    jvm version                                                      */
#*---------------------------------------------------------------------*/
jvm: build.jvm
build.jvm: o/class_s/bigloo/bglpkg .afile .etags .jfile jvm-stdlibs
	@ $(MAKE) $(BOOTBINDIR)/$(BGLPKG).jar

$(BOOTBINDIR)/$(BGLPKG).jar: $(MODULES:%=o/class_s/bigloo/bglpkg/%.class) \
     META-INF/MANIFEST.MF
	$(JAR) $@ META-INF/MANIFEST.MF -C o/class_s .

META-INF/MANIFEST.MF: META-INF
	echo "Manifest-Version: 1.0" > $@
	echo "Main-Class: bigloo/bglpkg/main" >> $@
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
	@ echo $(POPULATION:%=bglpkg/%)

#*---------------------------------------------------------------------*/
#*    directory dependencies                                           */
#*---------------------------------------------------------------------*/
o:
	mkdir -p $@

o/class_s:
	mkdir -p $@

o/class_s/bigloo/bglpkg:
	mkdir -p $@

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    The implicit `.scm.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.scm
	$(BIGLOO) -c $(BFLAGS) $(EFLAGS) $< -o $@

o/class_s/bigloo/bglpkg/%.class: %.scm
	$(BIGLOO) -c -jvm $(BFLAGS) $(BJVMFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags dep

.afile: $(FILES) configure.scm Makefile
	@ $(AFILE_BIN) -o .afile $(FILES) configure.scm

.jfile: $(FILES)
	@ $(JFILE_BIN) -o .jfile -pbase bigloo.bglpkg $(FILES) configure.scm

.etags: .afile
	@ $(BTAGS_BIN) -o .etags $(FILES) configure.scm

getsources:
	@ echo $(FILES) configure.scm

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
	   cp $(BOOTBINDIR)/bglpkg$(EXE_SUFFIX) $(DESTDIR)$(BINDIR); \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglpkg$(EXE_SUFFIX); \
        fi

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglpkg$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare bglpkg for a distribution                            */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOODIR)/bglpkg ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ cp $(BOOTDIR)/bglpkg/configure.scm configure.scm
	@ $(MAKE) .afile

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -rf o
	$(RM) -rf META-INF

devclean: clean
	$(RM) -f $(BOOTBINDIR)/$(BGLPKG)
	$(RM) -f $(BOOTBINDIR)/$(BGLPKG).jar

cleanall: devclean
	$(RM) -f .afile .jfile

distclean: devclean

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
$(OBJECTS): .afile

o/install.o: etc/Makefile etc/make_lib.scm etc/init

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/param.o param.class: param.sch 
o/param.o ./param.class: param.sch 
o/mzscheme.o ./mzscheme.class: param.sch 
o/install.o ./install.class: makefile.sch 
o/install.o install.class: makefile.sch 
o/mzscheme.o mzscheme.class: param.sch 

#bdepend stop
