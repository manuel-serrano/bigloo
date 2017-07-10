#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/crypto/src/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Jul  9 10:01:24 2017                          */
#*    Last change :  Sun Jul  9 10:06:30 2017 (serrano)                */
#*    Copyright   :  2017 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the library                                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Makefile configuration                                  */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= crypto
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
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= block_ciphers ciphers aes cast128 des idea pad \
                  util string2key \
                  dsa rsa elgamal DER pem


_OBJECTS	= $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

_CLASSES	= $(_BGL_OBJECTS)
BGL_CLASSES	= $(_CLASSES:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class


_OBJS		= $(_BGL_OBJECTS)

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)

SOURCES		= $(_BGL_SOURCES)
 
#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) \
                  $(BIGLOO_SRC_DIR)/idea-round.png \
                  $(BIGLOO_SRC_DIR)/bits32.sch \
                  $(MISC_SRC_DIR)/make_lib.scm \
                  $(MISC_SRC_DIR)/$(API).init \
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
