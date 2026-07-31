#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/bde/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jun  8 08:36:43 1998                          */
#*    Last change :  Fri Jul 31 07:39:14 2026 (serrano)                */
#*    Copyright   :  1998-2026 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile for the Bigloo development environment.             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# etags for info
ETAGS		= etags
# etags flags for info scanning
ETFLAGS		= --language=none \
                  --r '/^ - \(essential \|library \|bigloo \)?\(procedure\|syntax\): [^ ]+/' \
                  --r '/^ - bigloo module clause: [^ ]+/' \
                  --r '/^ - bigloo rgc procedure: [^ ]+/' \
                  --r '/^ - bigloo generic: [^ ]+/'
# the shell to be used
SHELL           = /bin/sh

#*---------------------------------------------------------------------*/
#*    Compilation flags                                                */
#*---------------------------------------------------------------------*/
#BFLAGS		= -jvm -jvm-jar -O2 -fsharing -unsafe
#BFLAGS		= -O2 -fsharing -unsafe $(SHRD_BDE_OPT) -lib-dir $(BOOTLIBDIR)
BFLAGS		= -O2 -fsharing $(SHRD_BDE_OPT) -lib-dir $(BOOTLIBDIR)
EFLAGS		=
BWATIBFLAGS	= -srfi enable-pthread

#*---------------------------------------------------------------------*/
#*    Sources and Objects                                              */
#*---------------------------------------------------------------------*/
#*--- afile -----------------------------------------------------------*/
AFILE_FILE	= afile
AFILE_OBJ	= $(AFILE_FILE:%=afile/%.o)

#*--- jfile -----------------------------------------------------------*/
JFILE_FILE	= jfile
JFILE_OBJ	= $(JFILE_FILE:%=jfile/%.o)

#*--- bpp -------------------------------------------------------------*/
BPP_FILE	= bpp reader
BPP_OBJ		= $(BPP_FILE:%=bpp/%.o)

#*--- demangle --------------------------------------------------------*/
DEMANGLE_FILE	= bgldemangle
DEMANGLE_OBJ	= $(DEMANGLE_FILE:%=bgldemangle/%.o)

#*--- jas -------------------------------------------------------------*/
JAS_LINKFILE	= lib classfile peep opcode wide labels stack produce as \
                  profile
JAS_FILE	= jas 
JAS_AFILE	= jas/.afile
JAS_LINKSRC	= $(JAS_LINKFILE:%=jas/%.scm)
JAS_LINKOBJ	= $(JAS_LINKFILE:%=jas/%.o)
JAS_OBJ		= $(JAS_FILE:%=jas/%.o)

#*--- walit -----------------------------------------------------------*/
WATIB_LINKFILE  = Val/validate Val/instructions \
                  Asm/binary  Asm/leb128 Opt/optimise  \
                  Opt/TestBr/walk Opt/UnCast/walk \
                  Opt/Unreachable/walk Opt/Const/walk Opt/PureDrop/walk \
                  Opt/CopyProp/walk \
                  Opt/CFG/dominance Opt/CFG/dump Opt/CFG/node \
                  Opt/CFG/order Opt/CFG/walk Opt/CFG/read \
                  Opt/BBV/walk \
                  Opt/Peephole/walk \
                  Env/env \
                  Ast/node \
                  Misc/list Misc/parse Misc/let-if \
                  Type/type Type/match
WATIB_FILE	= watib 
WATIB_AFILE	= watib/.afile
WATIB_LINKSRC	= $(WATIB_LINKFILE:%=watib/%.scm)
WATIB_LINKOBJ	= $(WATIB_LINKFILE:%=watib/%.o)
WATIB_OBJ  	= $(WATIB_FILE:%=watib/%.o)

#*--- bcache ----------------------------------------------------------*/
BCACHE_FILE	= bcache
BCACHE_OBJ	= $(BCACHE_FILE:%=bcache/%.o)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
OBJECTS		= $(AFILE_OBJ) $(BPP_OBJ) \
                  $(DEMANGLE_OBJ) $(JFILE_OBJ) \
                  $(JAS_OBJ) $(WATIB_OBJ) $(BCACHE_OBJ)
SOURCES_FILES	= $(OBJECTS:%.o=%.scm)

#*---------------------------------------------------------------------*/
#*     Suffixes ...                                                    */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .sch .o

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: all-common dobglmem 
	if [ "$(JVMBACKEND)" != "no" ]; then \
	  $(MAKE) all-jvm; \
	fi
	if [ "$(WASMBACKEND)" != "no" ]; then \
	  $(MAKE) all-wasm; \
	fi
	if [ "$(HOSTOS)" = "linux" ]; then \
	  $(MAKE) all-linux; \
	fi

all-common: $(BOOTBINDIR)/bglafile \
     $(BOOTBINDIR)/bgldemangle \
     $(BOOTBINDIR)/bglpp \
     $(BOOTBINDIR)/bgljfile \
     $(BOOTBINDIR)/bglcache

all-jvm: $(BOOTBINDIR)/bgljas

all-wasm: $(BOOTBINDIR)/bglwatib

all-linux: bglperf

boot:
	$(MAKE) all BFLAGS="$(BFLAGS) -ld-relative" WASMBACKEND=$(WASMBACKEND)

dobglmem: $(BOOTBINDIR)/bglafile
	@ if [ "$(LDPRELOADSUPPORT)" = "yes" \
                -a "$(PTHREADLIBS)" != "no" ]; then \
	    (cd bglmem && $(MAKE)) || exit 1; \
	  fi

$(BOOTBINDIR)/bglafile: $(AFILE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

afile.class: afile/afile.scm
	(cd afile; \
         $(BIGLOO) -c -jvm $(BFLAGS) $(EFLAGS) afile.scm -o afile.class; \
         mv afile.class ..)

$(BOOTBINDIR)/bgljfile: $(JFILE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

jfile.class: jfile/jfile.scm
	(cd jfile; $(BIGLOO) -c -jvm $(BFLAGS) $(EFLAGS) jfile.scm -o jfile.class; \
         mv jfile.class ..)

$(BOOTBINDIR)/bgldemangle: $(DEMANGLE_OBJ)
	$(BIGLOO) -no-hello $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/bglpp: $(BPP_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

jas: $(BOOTBINDIR)/bgljas
$(BOOTBINDIR)/bgljas: $(JAS_LINKSRC) $(JAS_AFILE) $(JAS_OBJ) $(JAS_LINKOBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $(JAS_OBJ) $(JAS_LINKOBJ)

watib: $(BOOTBINDIR)/bglwatib
$(BOOTBINDIR)/bglwatib: $(WATIB_LINKSRC) $(WATIB_AFILE) $(WATIB_OBJ) $(WATIB_LINKOBJ)
	(cd watib; $(BIGLOO) -cg $(BFLAGS) $(EFLAGS) -o $@ $(WATIB_OBJ:watib/%=%) $(WATIB_LINKOBJ:watib/%=%))

bglperf: $(BOOTBINDIR)/bglperf
$(BOOTBINDIR)/bglperf:
	$(MAKE) -C bglperf

$(BOOTBINDIR)/bglcache: $(BCACHE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: all
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	  targets="bglafile bgldemangle bglpp"; \
	  if [ "$(JVMBACKEND) " != "no " ]; then \
	     targets="$$targets bgljfile bgljas"; \
          fi; \
	  if [ "$(WASMBACKEND) " != "no " ]; then \
	     targets="$$targets bglwatib"; \
          fi; \
	  for p in $$targets; do \
	    cp $(BOOTBINDIR)/$$p$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$$p$(EXE_SUFFIX); \
            chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$$p$(EXE_SUFFIX); \
            cat $(BOOTBINDIR)/$$p.sh | \
              sed -e "s|$(BOOTBINDIR)|$(BINDIR)|g" \
                  -e "s|$(BOOTLIBDIR)|$(LIBDIR)/$(FILDIR)|g" \
                  -e "s|(BGLBUILDBINDIR)|$(BINDIR)|g" \
              > $(DESTDIR)$(BINDIR)/$$p.sh && \
            chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$$p.sh; \
          done; \
        fi
	if [ "$(LDPRELOADSUPPORT)" = "yes" -a "$(PTHREADLIBS)" != "no" ]; then \
	  (cd bglmem; $(MAKE) install) || exit 1; \
        fi
	if [ "$(HOSTOS)" = "linux" ]; then \
	  $(MAKE) -C bglperf install; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglafile$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgljfile$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgldemangle$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglpp$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgljas$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglcache$(EXE_SUFFIX)
	- $(MAKE) -C bglmem uninstall
	- $(MAKE) -C bglperf uninstall

#*---------------------------------------------------------------------*/
#*    touchall                                                         */
#*---------------------------------------------------------------------*/
touchall:
	touch $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@- $(RM) -f $(OBJECTS)
	@- $(RM) -f $(JAS_LINKOBJ)
	@- $(RM) -f $(JAS_LINKFILE:%=jas/%.c)
	@- $(RM) -f $(WATIB_LINKOBJ)
	@- $(RM) -f $(OBJECTS:%.o=%.c)
	@- $(RM) -f .afile
	@- $(RM) -f jas/.afile
	@- $(RM) -f *.class
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec rm {} \;   
	@ $(MAKE) -C bglmem clean
	@ $(MAKE) -C bglperf clean
	@ $(BEEP)
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@- $(RM) -f $(BOOTBINDIR)/bglafile 
	@- $(RM) -f $(BOOTBINDIR)/bgljfile 
	@- $(RM) -f $(BOOTBINDIR)/bgldemangle
	@- $(RM) -f $(BOOTBINDIR)/bglpp 
	@- $(RM) -f $(BOOTBINDIR)/bgljas
	@- $(RM) -f $(BOOTBINDIR)/bglwatib
	@- $(RM) -f $(BOOTBINDIR)/bglcache
	@- $(RM) -f $(JAS_LINKSRC)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    Prepare Bde for a distribution                                   */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/bde ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	$(MAKE) -C bglmem .afile
	$(MAKE) $(JAS_AFILE)

#*---------------------------------------------------------------------*/
#*    jvm                                                              */
#*---------------------------------------------------------------------*/
jvm: afile.class jfile.class

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
watib/%.o: watib/%.scm $(WATIB_AFILE)
	(cd watib; $(BIGLOO) $(BWATIBFLAGS) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o -afile .afile)

jas/%.o: jas/%.scm $(JAS_AFILE)
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c jas/$*.scm -o jas/$*.o -afile jas/.afile

%.o: %.scm
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c $*.scm -o $*.o

%.o: %.bgl
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*     .scm --> .class                                                 */
#*---------------------------------------------------------------------*/
jas/%.class: jas/%.scm $(JAS_AFILE)
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c jas/$*.scm -o jas/$*.class -afile jas/.afile

%.class: %.scm
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.class

%.class: %.bgl
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*    checksum.o                                                       */
#*---------------------------------------------------------------------*/
../comptime/Module/checksum.o: ../comptime/Module/checksum.scm
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o \
          -access engine_param demangle/demangle.scm

#*---------------------------------------------------------------------*/
#*    jas link files                                                   */
#*---------------------------------------------------------------------*/
jas/%.scm: ../comptime/Jas/%.scm
	cp $< $@

jas/classfile.scm: ../comptime/Jas/classfile.scm
	cat $< | sed 's|Jas/classfile.sch|jas/classfile.sch|' > $@ || exit $@

jas/classfile.scm: jas/classfile.sch

jas/classfile.sch: ../comptime/Jas/classfile.sch
	cp $< $@

#*---------------------------------------------------------------------*/
#*    Jas afile                                                        */
#*---------------------------------------------------------------------*/
$(JAS_AFILE): $(JAS_LINKSRC)
	if [ ! -f jas/$(JAS_AFILE) ]; then \
	  (cd jas; $(BGLAFILE) *.scm -o ../$(JAS_AFILE)); \
	fi

#*---------------------------------------------------------------------*/
#*    Watib afile                                                      */
#*---------------------------------------------------------------------*/
$(WATIB_AFILE): $(WATIB_LINKSRC)
	if [ ! -f watib/$(WATIB_AFILE) ]; then \
	  (cd watib; $(BGLAFILE) *.scm */*.scm -o ../$(WATIB_AFILE)); \
	fi

#*---------------------------------------------------------------------*/
#*    bmake dependencies                                               */
#*---------------------------------------------------------------------*/
bmake/template.o: bmake/makefile.library bmake/makefile.appli
