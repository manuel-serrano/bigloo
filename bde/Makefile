#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bde/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jun  8 08:36:43 1998                          */
#*    Last change :  Wed Nov 16 12:19:39 2011 (serrano)                */
#*    Copyright   :  1998-2011 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile for the Bigloo development environment.             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# Bigloo
BIGLOO		= $(BGLBUILDBIGLOO) -no-hello
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
BFLAGS		= -O2 -fsharing -unsafe $(SHRD_BDE_OPT)
EFLAGS		=

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

#*--- btags -----------------------------------------------------------*/
BTAGS_FILE	= btags
BTAGS_OBJ	= $(BTAGS_FILE:%=btags/%.o)

#*--- bdepend ---------------------------------------------------------*/
BDEPEND_FILE	= bdepend
BDEPEND_OBJ	= $(BDEPEND_FILE:%=bdepend/%.o)

#*--- bmake -----------------------------------------------------------*/
BMAKE_FILE	= bmake template
BMAKE_OBJ	= $(BMAKE_FILE:%=bmake/%.o)

#*--- bprof -----------------------------------------------------------*/
BPROF_FILE	= bprof
BPROF_OBJ	= $(BPROF_FILE:%=bprof/%.o)

#*--- mco -------------------------------------------------------------*/
MCO_FILE	= mco
MCO_OBJ		= $(MCO_FILE:%=mco/%.o)

#*--- jas -------------------------------------------------------------*/
JAS_LINKFILE	= lib classfile peep opcode wide labels stack produce as \
                  profile
JAS_FILE	= jas 
JAS_AFILE	= jas/.afile
JAS_LINKSRC	= $(JAS_LINKFILE:%=jas/%.scm)
JAS_LINKOBJ	= $(JAS_LINKFILE:%=jas/%.o)
JAS_OBJ		= $(JAS_FILE:%=jas/%.o)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
OBJECTS		= $(AFILE_OBJ) $(BPP_OBJ) $(BTAGS_OBJ) $(BDEPEND_OBJ) \
                  $(BMAKE_OBJ) $(BPROF_OBJ) $(MCO_OBJ) $(JFILE_OBJ) \
                  $(JAS_OBJ)
SOURCES_FILES	= $(OBJECTS:%.o=%.scm)

POPULATION	= $(SOURCES_FILES) \
                  bpp/README \
	          bmake/makefile.appli \
	          bmake/makefile.library \
                  jas/jas.tex \
                  Makefile

#*---------------------------------------------------------------------*/
#*     Suffixes ...                                                    */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .sch .o .el .elc

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: $(BOOTBINDIR)/$(AFILE_EXE) \
     $(BOOTBINDIR)/$(JFILE_EXE) \
     $(BOOTBINDIR)/$(BDEPEND_EXE) \
     $(BOOTBINDIR)/$(MCO_EXE) \
     $(BOOTBINDIR)/$(BMAKE_EXE) \
     $(BOOTBINDIR)/$(BPP_EXE) \
     $(BOOTBINDIR)/$(BTAGS_EXE) \
     $(BOOTBINDIR)/$(BPROF_EXE) \
     $(BOOTBINDIR)/$(JAS_EXE) \
     dobmem

boot:
	$(MAKE) all BFLAGS="$(BFLAGS) -ld-relative"

dobmem: $(BOOTBINDIR)/$(AFILE_EXE)
	@ if [ "$(LDPRELOADSUPPORT)" = "yes" \
                -a "$(PTHREADLIBS)" != "no" ]; then \
	    (cd bmem && $(MAKE)) || exit 1; \
	  fi

$(BOOTBINDIR)/$(AFILE_EXE): $(AFILE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

afile.class: afile/afile.scm
	(cd afile; \
         $(BIGLOO) -c -jvm $(BFLAGS) $(EFLAGS) afile.scm -o afile.class; \
         mv afile.class ..)

$(BOOTBINDIR)/$(JFILE_EXE): $(JFILE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

jfile.class: jfile/jfile.scm
	(cd jfile; $(BIGLOO) -c -jvm $(BFLAGS) $(EFLAGS) jfile.scm -o jfile.class; \
         mv jfile.class ..)

$(BOOTBINDIR)/$(BDEPEND_EXE): $(BDEPEND_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/$(BMAKE_EXE): $(BMAKE_OBJ)
	$(BIGLOO) -no-hello $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/$(BPROF_EXE): $(BPROF_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/$(MCO_EXE): $(MCO_OBJ) ../comptime/Module/checksum.o
	$(BIGLOO) -no-hello $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/$(BPP_EXE): $(BPP_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/$(BTAGS_EXE): $(BTAGS_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

jas: $(BOOTBINDIR)/$(JAS_EXE)
$(BOOTBINDIR)/$(JAS_EXE): $(JAS_LINKSRC) $(JAS_AFILE) $(JAS_OBJ) $(JAS_LINKOBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $(JAS_OBJ) $(JAS_LINKOBJ)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: all
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	  cp $(BOOTBINDIR)/$(AFILE_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(AFILE_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(AFILE_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(JFILE_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(JFILE_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(JFILE_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(BDEPEND_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(BDEPEND_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(BDEPEND_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(BMAKE_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(BMAKE_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(BMAKE_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(BPROF_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(BPROF_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(BPROF_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(MCO_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(MCO_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(MCO_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(BPP_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(BPP_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(BPP_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(BTAGS_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(BTAGS_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(BTAGS_EXE)$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/$(JAS_EXE)$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/$(JAS_EXE)$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(JAS_EXE)$(EXE_SUFFIX); \
	  if [ "$(LDPRELOADSUPPORT)" = "yes" \
               -a "$(PTHREADLIBS)" != "no" ]; then \
	    (cd bmem; $(MAKE) install) || exit 1; \
          fi; \
        fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(AFILE_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(JFILE_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(BDEPEND_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(BMAKE_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(BPROF_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(MCO_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(BPP_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(BTAGS_EXE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(JAS_EXE)$(EXE_SUFFIX)
	- (cd bmem; $(MAKE) uninstall)

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
	@ (cd bmem; $(MAKE) clean)
	@ $(BEEP)
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@- $(RM) -f $(BOOTBINDIR)/$(AFILE_EXE) 
	@- $(RM) -f $(BOOTBINDIR)/$(JFILE_EXE) 
	@- $(RM) -f $(BOOTBINDIR)/$(BDEPEND_EXE) 
	@- $(RM) -f $(BOOTBINDIR)/$(BMAKE_EXE) 
	@- $(RM) -f $(BOOTBINDIR)/$(BPROF_EXE)
	@- $(RM) -f $(BOOTBINDIR)/$(MCO_EXE)
	@- $(RM) -f $(BOOTBINDIR)/$(BPP_EXE) 
	@- $(RM) -f $(BOOTBINDIR)/$(BTAGS_EXE)
	@- $(RM) -f $(BOOTBINDIR)/$(JAS_EXE)
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

#*---------------------------------------------------------------------*/
#*    jvm                                                              */
#*---------------------------------------------------------------------*/
jvm: afile.class jfile.class

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=bde/%)
	@ (cd bmem; $(MAKE) pop)

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
jas/%.o: jas/%.scm $(JAS_AFILE)
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c jas/$*.scm -o jas/$*.o -afile jas/.afile

%.o: %.scm
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .class                                                 */
#*---------------------------------------------------------------------*/
jas/%.class: jas/%.scm $(JAS_AFILE)
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c jas/$*.scm -o jas/$*.class -afile jas/.afile

%.class: %.scm
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.class

#*---------------------------------------------------------------------*/
#*    checksum.o                                                       */
#*---------------------------------------------------------------------*/
../comptime/Module/checksum.o: ../comptime/Module/checksum.scm
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o \
          -access engine_param mco/mco.scm

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
$(JAS_AFILE): $(BGLBUILDBINDIR)/$(AFILE_EXE) $(JAS_LINKSRC)
	(cd jas; $(BGLBUILDBINDIR)/$(AFILE_EXE) *.scm -o ../$(JAS_AFILE))

#*---------------------------------------------------------------------*/
#*    bmake dependencies                                               */
#*---------------------------------------------------------------------*/
bmake/template.o: bmake/makefile.library bmake/makefile.appli
