#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/pkgcomp/src/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Jul  9 10:15:22 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the library.                               */
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
API		= pkgcomp
# Directories
BIGLOO_SRC_DIR 	= Llib
C_SRC_DIR	= C
JAVA_SRC_DIR 	= Java
MISC_SRC_DIR 	= Misc
# Where to store the library class files
PBASE		= bigloo.$(API)
CLASS_DIR	= objs/class_s/bigloo/$(API)
CLASS_EDIR	= objs/class_es/bigloo/$(API)
# The Bigloo compiler
AFLAGS		= -m package*
AFILE		= $(BGLBUILDBINDIR)/bglafile.sh
JFILE		= $(BGLBUILDBINDIR)/bgljfile.sh
DEPEND		= $(BGLBUILDBINDIR)/bglbdepend.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)
# Bigloo pkgcomp compilation options
PKGCOMPFLAGS	= -load $(BOOTLIBDIR)/pkgcomp.init
BMISCFLAGS	= -I Llib

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= configure interface

_OBJECTS	= $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)

SOURCES		= $(_BGL_SOURCES)

_INCLUDES	= srfi89.sch interface.sch expanders.sch \
                  class.sch configure.sch
INCLUDES	= $(_INCLUDES:%=$(BIGLOO_SRC_DIR)/%)

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) \
	          $(INCLUDES) \
                  $(MISC_SRC_DIR)/make_lib.scm \
                  $(MISC_SRC_DIR)/$(API).init.in \
                  $(BIGLOO_SRC_DIR)/configure.scm.in \
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
build-c: api-c

jvm: build-jvm
build-jvm: api-jvm

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
