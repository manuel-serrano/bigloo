#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/mpg123/src/Makefile       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Thu Apr 30 08:15:05 2020 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= mpg123
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
DEPEND		= $(BGLBUILDBINDIR)/bglbdepend.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
CIGLOO		= $(BGLBUILDBINDIR)/cigloo
# Bigloo compilation options
BAPISAFE	= -unsafe -safee
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) $(BAPISAFE) \
                  -srfi $(API) -copt "$(MPG123CFLAGS) -DBGL_DUMMY"
CAPIFLAGS       = $(MPG123CFLAGS) $(CGCTHREADFLAGS)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API) -srfi $(API)
# C flags
CEFLAGS		= $(MPG123CFLAGS)

# Additional pthread lib for C
ifeq "$(MPG123LIBS) " " "
else
EXTRALIBS	+= $(MPG123LIBS) -lm
endif

BIGLOOEXTRALIBS	+= bigloomultimedia


ifneq "$(ALSALIBS) " " "
EXTRALIBS	+= $(ALSALIBS)
BIGLOOEXTRALIBS	+= biglooalsa
endif
ifneq "$(PULSEAUDIOLIBS) " " "
EXTRALIBS	+= $(PULSEAUDIOLIBS)
BIGLOOEXTRALIBS	+= bigloopulseaudio
endif

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= mpg123 mpg123_decoder mpg123_alsadec mpg123_padec

ifeq "$(MPG123LIBS) " " "
_C_OBJECTS	= 
else
_C_OBJECTS	= bglmpg123
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
INCLUDES	= $(BIGLOO_SRC_DIR)/mpg123.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) $(_WIN32_SOURCES) \
		  $(C_SRC_DIR)/bglmpg123.h \
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
#*    bglhandle.h                                                      */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglhandle.h: Llib/mpg123.scm
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
	$(RM) -f $(C_SRC_DIR)/bglhandle.h

cleanall: api-cleanall

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    Common API rules                                                 */
#*---------------------------------------------------------------------*/
include ../../Makefile.api

#*---------------------------------------------------------------------*/
#*    explicit depencies                                               */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/bglmpg123.c: $(C_SRC_DIR)/bglhandle.h

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
objs/mpg123.o Llib/mpg123.class:

#bdepend stop
