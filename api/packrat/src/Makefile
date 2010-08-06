#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/packrat/src/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Fri Aug  6 08:10:06 2010 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Packrat configuration                                   */
#*---------------------------------------------------------------------*/
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= packrat
# Directories
BIGLOO_SRC_DIR 	= Llib
DOTNET_SRC_DIR 	= DotNet
JAVA_SRC_DIR 	= Java
C_SRC_DIR 	= Posix
MISC_SRC_DIR 	= Misc
# Where to store the library class files
PBASE		= bigloo.$(API)
CLASS_DIR	= objs/class_s/bigloo/$(API)
CLASS_EDIR	= objs/class_es/bigloo/$(API)
DOTNET_OBJ_DIR	= objs/dotnet_s
DOTNET_OBJ_EDIR	= objs/dotnet_es
# The Bigloo compiler
BIGLOO		= $(BGLBUILDBIGLOO)
AFILE		= $(BGLBUILDBINDIR)/$(AFILE_EXE)
JFILE		= $(BGLBUILDBINDIR)/$(JFILE_EXE)
DEPEND		= $(BGLBUILDBINDIR)/$(BDEPEND_EXE)
BTAGS		= $(BGLBUILDBINDIR)/$(BTAGS_EXE)
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) \
                  -srfi $(API) -unsafe -safee
#BLFLAGS	= -unsafe $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q \
                  -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)

# Extra libs
EXTRALIBS_S	= -lbigloosrfi1_s-$(RELEASE)
EXTRALIBS_U	= -lbigloosrfi1_u-$(RELEASE)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= packrat json

_OBJECTS	= $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make_lib.obj

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)

SOURCES		= $(_BGL_SOURCES)
INCLUDES	= $(BIGLOO_SRC_DIR)/packrat.sch

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
.PHONY: c jvm dotnet build-c build-jvm build-dotnet

all:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
             $(MAKE) build-c; \
	  fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) build-jvm; \
	  fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
             $(MAKE) build-dotnet;  \
	  fi

c: build-c
build-c: api-c

jvm: build-jvm
build-jvm: api-jvm

dotnet: build-dotnet
build-dotnet: api-dotnet

$(MISC_SRC_DIR)/$(API).init: $(MISC_SRC_DIR)/$(API).init.in
	cp $^ $@ && cat $(BIGLOO_SRC_DIR)/packrat.sch >> $@

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
