#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/pthread/src/Makefile      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Tue Jan 21 09:03:37 2020 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Pthread configuration                                   */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= pthread
# Directories
BIGLOO_SRC_DIR 	= Llib
JAVA_SRC_DIR 	= Java
C_SRC_DIR 	= Posix
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
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) 
BAPIUNSAFEFLAGS = -unsafe -safee
CAPIFLAGS	= $(CGCTHREADFLAGS)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)

# Additional pthread lib for C
EXTRALIBS	+= $(PTHREADLIBS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= pmutex pcondvar pthread pbackend psemaphore

_POSIX_OBJECTS	= bglsetup bglbmem bglpmutex bglpcondvar bglpthread \
                  bglpsemaphore
_JAVA_CLASSES	= bglpmutex bglpcondvar bglpthread bglpdynamic \
                  bglpsemaphore
 

_OBJECTS	= $(_POSIX_OBJECTS) $(_BGL_OBJECTS)
SAFE_OBJECTS	= $(_OBJECTS:%=objs/objs_s/%.o)
SAFE_EOBJECTS	= objs/objs_s/make_lib.o
UNSAFE_OBJECTS	= $(_OBJECTS:%=objs/objs_u/%.o)
UNSAFE_EOBJECTS	= objs/objs_u/make_lib.o

BGL_CLASSES	= $(_BGL_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class
JAVA_CLASSES	= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.class)
JAVA_CLASSES_SRC= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.java)

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)
_POSIX_SOURCES	= $(_POSIX_OBJECTS:%=$(C_SRC_DIR)/%.c)
_JAVA_SOURCES	= $(_JAVA_CLASSES:%=$(JAVA_SRC_DIR)/%.java)

SOURCES		= $(_BGL_SOURCES) \
                  $(_POSIX_SOURCES) \
                  $(_JAVA_SOURCES)
INCLUDES	= $(C_SRC_DIR)/bglpthread.h \
                  $(BIGLOO_SRC_DIR)/pthread.sch \
                  $(BIGLOO_SRC_DIR)/pmutex.sch \
                  $(BIGLOO_SRC_DIR)/pcondvar.sch \
                  $(BIGLOO_SRC_DIR)/psemaphore.sch \
 
#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) \
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
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) build-jvm; \
	  fi

c: build-c
build-c: api-c_mt

jvm: build-jvm
build-jvm: api-jvm

$(MISC_SRC_DIR)/$(API).init: $(MISC_SRC_DIR)/$(API).init.in
	@ echo "*** ERROR: $@ is out of date (older than $?)"
	@ echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""
	@ exit 1

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
include ../../Makefile.api-safe

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
objs/pthread.o Llib/pthread.class: Llib/pthread.sch 
objs/pcondvar.o Llib/pcondvar.class: Llib/pcondvar.sch 
objs/pmutex.o Llib/pmutex.class: Llib/pmutex.sch 

#bdepend stop
