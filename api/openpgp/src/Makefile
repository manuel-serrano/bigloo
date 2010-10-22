#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/openpgp/src/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Aug 11 15:36:25 2010                          */
#*    Last change :  Thu Oct 21 10:51:10 2010 (serrano)                */
#*    Copyright   :  2010 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the OpenPGP Bigloo library.                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Makefile configuration                                  */
#*---------------------------------------------------------------------*/
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= openpgp
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
                  -fno-reflection \
                  -unsafe -safee
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)
# Additional crypto
EXTRALIBS_S	= -lbigloocrypto_s-$(RELEASE)
EXTRALIBS_U	= -lbigloocrypto_u-$(RELEASE)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= pgp_algo pgp_composition pgp_conversion \
                  pgp_decode pgp_encode pgp_enums pgp_facade \
                  pgp_human pgp_key_manager pgp_logic pgp_packets \
                  pgp_s2k port_util util

_OBJECTS	= $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

_CLASSES	= $(_BGL_OBJECTS)
BGL_CLASSES	= $(_CLASSES:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class


_OBJS		= $(_BGL_OBJECTS)
BGL_DOTNET_OBJ	= $(_OBJS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make_lib.obj

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)

SOURCES		= $(_BGL_SOURCES)
 
#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) \
                  $(MISC_SRC_DIR)/make_lib.scm \
                  $(MISC_SRC_DIR)/$(API).init \
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
