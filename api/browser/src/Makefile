#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/api/browser/src/Makefile       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Thu Jul 23 17:43:16 2026 (serrano)                */
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
JAVA_SRC_DIR 	= 
C_SRC_DIR 	= 
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

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= dom
_BGL5_OBJECTS	= make_lib5

_OBJECTS	= $(_BGL_OBJECTS) $(_BGL5_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o


WOBJECTS	= $(_BGL_OBJECTS:%=objs/%.wat) $(_BGL5_OBJECTS:%=objs/%.wat) \
                  Wlib/wbrowser.wat
WEOBJECTS	= $(EOBJECTS:%.o=%.wat)

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)

SOURCES		= $(_BGL_SOURCES)
 
#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: c jvm wasm build-c build-jvm build-wasm

all:
	@ if [ "$(WASMBACKEND)" = "yes" ]; then \
             $(MAKE) build-wasm; \
	  fi

c: build-c
build-c: api-c

jvm: build-jvm
build-jvm: api-jvm

wasm: build-wasm
build-wasm: api-wasm

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
