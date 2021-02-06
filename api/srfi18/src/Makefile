#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/srfi18/src/Makefile       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Tue Jan 21 09:03:29 2020 (serrano)                */
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
API		= srfi18
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
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee \
                   -copt -I$(BOOTDIR)/api/pthread/src/Posix
CAPIFLAGS	= $(CGCTHREADFLAGS) -I$(BOOTDIR)/api/pthread/src/Posix
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)

# Additional pthread lib for C
EXTRALIBS	+= $(PTHREADLIBS)
BIGLOOEXTRALIBS += bigloopthread

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= mutex condvar thread backend

_POSIX_OBJECTS	= csetup cmutex ccondvar cthread
_JAVA_CLASSES	= jmutex jcondvar jthread jdynamic

_OBJECTS	= $(_POSIX_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

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
INCLUDES	= $(C_SRC_DIR)/srfi18.h \
                  $(BIGLOO_SRC_DIR)/srfi18.sch \
                  $(BIGLOO_SRC_DIR)/mutex.sch \
                  $(BIGLOO_SRC_DIR)/condvar.sch
 
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
include ../../Makefile.api

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
