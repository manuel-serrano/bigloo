#*=====================================================================*/
#*    serrano/prgm/project/bigloo/wasm/api/browser/src/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Wed Oct  1 15:54:20 2025 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Browser configuration                                   */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= browser
# Directories
BIGLOO_SRC_DIR 	= Llib
JAVA_SRC_DIR 	= Java
C_SRC_DIR 	= Posix
MISC_SRC_DIR 	= Misc
# The Bigloo compiler
AFILE		= $(BGLBUILDBINDIR)/bglafile.sh
JFILE		= $(BGLBUILDBINDIR)/bgljfile.sh
DEPEND		= $(BGLBUILDBINDIR)/bglbdepend.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) \
                  -srfi $(API) -unsafe -safee
#BAPIFLAGS	= -unsafe $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q \
                  -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)
# wasm as
WASMAS		= $(BGLWATIB)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= dom

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

EOBJECTS	= objs/make_lib.o


WOBJECTS	= $(_BGL_OBJECTS:%=objs/%.wat) Wlib/wbrowser.wat
WEOBJECTS	= objs/make_lib.wat

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
.PHONY: c jvm build-c build-jvm build-wasm

all:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
             $(MAKE) build-c; \
	  fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) build-jvm; \
	  fi
	@ if [ "$(WASMBACKEND)" = "yes" ]; then \
             $(MAKE) build-wasm; \
	  fi

c: build-c
build-c: api-c

jvm: build-jvm
build-jvm: api-jvm

wasm: build-wasm
build-wasm: api-wasm

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
