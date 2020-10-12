#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/alsa/src/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Fri Mar 27 12:34:05 2020 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Alsa configuration                                      */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= alsa
# Directories
BIGLOO_SRC_DIR 	= Llib
C_SRC_DIR 	= Clib
PLUGIN_SRC_DIR 	= Plugin
MISC_SRC_DIR 	= Misc
# Where to store the library class files
PBASE		= bigloo.$(API)
CLASS_DIR	= objs/class_s/bigloo/$(API)
CLASS_EDIR	= objs/class_es/bigloo/$(API)
# The Bigloo compiler
AFILE		= $(BGLBUILDBINDIR)/bglafile.sh
JFILE		= $(BGLBUILDBINDIR)/bgljfile.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
DEPEND		= $(BGLBUILDBINDIR)/$(BDEPEND_EXE)
CIGLOO		= $(BGLBUILDBINDIR)/cigloo
# Bigloo compilation options
BAPISAFE	= -unsafe -safee
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) $(BAPISAFE) \
                  -srfi $(API) -copt "$(ALSACFLAGS) -DBGL_DUMMY"
CAPIFLAGS       = $(ALSACFLAGS) $(CGCTHREADFLAGS)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API) -srfi $(API)
# C flags
CEFLAGS		= $(ALSACFLAGS)

# Additional pthread lib for C
ifeq "$(ALSALIBS) " " "
EXTRALIBS	= 
else
EXTRALIBS	+= $(ALSALIBS)
BIGLOOEXTRALIBS	+= bigloomultimedia
endif

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= alsa pcm card control mixer music rawmidi

ifeq "$(ALSALIBS) " " "
_C_OBJECTS	= 
else
_C_OBJECTS	= bglalsa
endif
_JAVA_CLASSES	= 

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

EOBJECTS	= objs/make_lib.o

BGL_CLASSES	= $(_BGL_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class

JAVA_CLASSES	= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.class)
JAVA_CLASSES_SRC= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.java)

_WIN32_SRC	= 

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)
_C_SOURCES	= $(_C_OBJECTS:%=$(C_SRC_DIR)/%.c)
_JAVA_SOURCES	= $(_JAVA_CLASSES:%=$(JAVA_SRC_DIR)/%.java)
_WIN32_SOURCES	= $(_WIN32_SRC:%=$(WIN32_SRC_DIR)/%)

SOURCES		= $(_BGL_SOURCES) \
                  $(_C_SOURCES) \
                  $(_JAVA_SOURCES)
INCLUDES	= $(BIGLOO_SRC_DIR)/alsa.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) $(_WIN32_SOURCES) \
		  $(C_SRC_DIR)/bglalsa.h \
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
#*    $(C_SRC_DIR)/bglalsa_config.h.in ...                             */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglalsa_config.h.in: $(C_SRC_DIR)/bglalsa_config.h
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
#*    bglmixer.h                                                       */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglmixer.h: Llib/mixer.scm
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    bglrawmidi.h                                                     */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglrawmidi.h: Llib/rawmidi.scm
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
	$(RM) -f $(C_SRC_DIR)/bglpcm.h
	$(RM) -f $(C_SRC_DIR)/bglctl.h
	$(RM) -f $(C_SRC_DIR)/bglmixer.h
	$(RM) -f $(C_SRC_DIR)/bglrawmidi.h

cleanall: api-cleanall

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    Common API rules                                                 */
#*---------------------------------------------------------------------*/
include ../../Makefile.api

#*---------------------------------------------------------------------*/
#*    explicit depencies                                               */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglalsa.c: $(C_SRC_DIR)/bglpcm.h \
  $(C_SRC_DIR)/bglctl.h \
  $(C_SRC_DIR)/bglmixer.h \
  $(C_SRC_DIR)/bglrawmidi.h

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
objs/alsa.o Llib/alsa.class:

#bdepend stop
