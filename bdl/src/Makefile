#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdl/src/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Jul  9 10:20:30 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the bdl library.                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Bdl configuration                                       */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config
include ../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= bdl
# Where to store the library class files
CLASS_DIR	= objs/class_s/bigloo/bdl
BIGLOO_SRC_DIR 	= .
DOTNET_SRC_DIR 	= .
JAVA_SRC_DIR 	= .
C_SRC_DIR 	= .
MISC_SRC_DIR 	= .
# Where to store the library class files
PBASE		= bigloo.$(API)
CLASS_DIR	= objs/class_s/bigloo/$(API)
CLASS_EDIR	= objs/class_es/bigloo/$(API)
DOTNET_OBJ_DIR	= objs/dotnet_s
DOTNET_OBJ_EDIR	= objs/dotnet_es
# # The Bigloo compiler
BIGLOO		= $(BOOTBINDIR)/bigloo.sh
AFILE		= $(BOOTBINDIR)/bglafile.sh
JFILE		= $(BOOTBINDIR)/bgljfile.sh
DEPEND		= $(BOOTBINDIR)/bgldepend.sh
BTAGS		= $(BOOTBINDIR)/bgltags.sh
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) \
                  -heap-library $(API)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= etags misc prgm types env

_OBJECTS	= $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make_lib.obj

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)

SOURCES		= $(_BGL_SOURCES)

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) Makefile bdl.init make_lib.scm

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
#*    Installation ...                                                 */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install: api-install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) api-install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-jvm; \
          fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-dotnet; \
          fi

uninstall: api-uninstall-init \
           api-uninstall-c \
           api-uninstall-jvm \
           api-uninstall-dotnet

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
include ../../api/Makefile.api
