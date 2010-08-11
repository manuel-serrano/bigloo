#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdl/src/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Wed Aug 11 15:44:59 2010 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the bdl library.                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Bdl configuration                                       */
#*---------------------------------------------------------------------*/
include ../../Makefile.config
include ../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= bdl
# Where to store the library class files
PBASE		= bigloo.bdl
CLASS_DIR	= objs/class_s/bigloo/bdl
BIGLOO_SRC_DIR 	= .
DOTNET_SRC_DIR 	= .
JAVA_SRC_DIR 	= .
C_SRC_DIR 	= .
MISC_SRC_DIR 	= .
# # The Bigloo compiler
BIGLOO		= $(BOOTBINDIR)/bigloo
AFILE		= $(BOOTBINDIR)/$(AFILE_EXE)
JFILE		= $(BOOTBINDIR)/$(JFILE_EXE)
DEPEND		= $(BOOTBINDIR)/$(BDEPEND_EXE)
BTAGS		= $(BOOTBINDIR)/$(BTAGS_EXE)
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) \
                  -heap-library $(API)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= etags misc prgm types env

_C_OBJECTS	= 
_JAVA_CLASSES	= 

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

BGL_CLASSES	= $(_BGL_OBJECTS:%=$(CLASS_DIR)/%.class)
JAVA_CLASSES	= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.class)
JAVA_CLASSES_SRC= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.java)
CLASSES		= $(_BGL_CLASSES) $(JAVA_CLASSES)

_BGL_SOURCES	= $(_BGL_OBJECTS:%=%.scm) 
_C_SOURCES	= $(_C_OBJECTS:%=%.c) 
_JAVA_SOURCES	= $(_JAVA_CLASSES:%=%.java) 

SOURCES		= $(_BGL_SOURCES) $(_C_SOURCES) $(_JAVA_SOURCES)
INCLUDES	= 

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
