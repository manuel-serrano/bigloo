#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/patch/src/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Mon Jul  3 17:29:43 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Patch configuration                                     */
#*---------------------------------------------------------------------*/
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= patch
# Directories
BIGLOO_SRC_DIR 	= Llib
C_SRC_DIR 	= Clib
MISC_SRC_DIR 	= Misc
# The Bigloo compiler
AFILE		= $(BGLBUILDBINDIR)/bglafile.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
DEPEND		= $(BGLBUILDBINDIR)/$(BDEPEND_EXE)
CIGLOO		= $(BGLBUILDBINDIR)/cigloo
# Bigloo compilation options
BAPISAFE	= -unsafe -safee
BAPISAFE	= -safee -cg
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) $(BAPISAFE) \
                  -srfi $(API) -copt "$(PATCHCFLAGS)"
CAPIFLAGS       = $(PATCHCFLAGS)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API) -srfi $(API)
# C flags
CEFLAGS		= $(PATCHCFLAGS)

# Additional pthread lib for C
ifeq "$(PATCHLIBS) " " "
EXTRALIBS	= 
else
EXTRALIBS	= $(PATCHLIBS)
endif

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= patch

#_C_OBJECTS	= self-mod bglpatch
_C_OBJECTS	= cpatch

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

EOBJECTS	= objs/make_lib.o

_WIN32_SRC	= 

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)
_C_SOURCES	= $(_C_OBJECTS:%=$(C_SRC_DIR)/%.c)
_WIN32_SOURCES	= $(_WIN32_SRC:%=$(WIN32_SRC_DIR)/%)

SOURCES		= $(_BGL_SOURCES) \
                  $(_C_SOURCES)
INCLUDES	= 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) $(_WIN32_SOURCES) \
		  $(C_SRC_DIR)/bglpatch.h \
                  $(C_SRC_DIR)/self-mod.h $(C_SRC_DIR)/cpatch.h \
		  $(MISC_SRC_DIR)/make_lib.scm \
                  $(MISC_SRC_DIR)/$(API).init.in \
                  Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: c jvm build-c build-jvm

all: 
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
             $(MAKE) build-c; \
	  fi

c: build-c
build-c: api-c

jvm: build-jvm
build-jvm:

$(MISC_SRC_DIR)/$(API).init: $(MISC_SRC_DIR)/$(API).init.in
	@ echo "*** ERROR: $@ is out of date (older than $?)"
	@ echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""
	@ exit 1

#*---------------------------------------------------------------------*/
#*    $(C_SRC_DIR)/bglpatch_config.h.in ...                             */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglpatch_config.h.in: $(C_SRC_DIR)/bglpatch_config.h
	@ echo "*** ERROR: $@ is out of date (older than $?)"
	@ echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""
	@ exit 1

#*---------------------------------------------------------------------*/
#*    bglpcm.h                                                         */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglpcm.h: Llib/pcm.scm
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    bglctl.h                                                         */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglctl.h: Llib/control.scm
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    bglmixer.h                                                         */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglmixer.h: Llib/mixer.scm
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=$(API)/src/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean: api-clean

cleanall: api-cleanall

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    Common API rules                                                 */
#*---------------------------------------------------------------------*/
include ../../Makefile.api

#*---------------------------------------------------------------------*/
#*    install-c-include                                                */
#*---------------------------------------------------------------------*/
install-c-include:
	cp Clib/bglpatch.h $(LIBDIR)/$(FILDIR)/bglpatch.h \
          && chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/bglpatch.h
	cp Clib/cpatch.h $(LIBDIR)/$(FILDIR)/cpatch.h \
          && chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/cpatch.h

uninstall-c-include:
	$(RM) $(LIBDIR)/$(FILDIR)/bglpatch.h
	$(RM) $(LIBDIR)/$(FILDIR)/cpatch.h


#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
objs/patch.o Llib/patch.class:

#bdepend stop
