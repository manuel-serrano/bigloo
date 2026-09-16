#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/api/mail/src/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Wed Sep 16 07:56:56 2026 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Mail configuration                                      */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= mail
# Directories
PKG_SRC_DIR	= Pkgs
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
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) # -unsafe -safee
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q \
                  -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                  -heap-library $(API)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= rfc2045 rfc2047 rfc2822 mailbox maildir vcard
_BGL5_OBJECTS	= make_lib5
_STK_OBJECTS	= imap

_OBJECTS	= $(_BGL_OBJECTS) $(_BGL5_OBJECTS) $(_STK_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)
EOBJECTS	= objs/make_lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class

WOBJECTS	= $(_BGL_OBJECTS:%=objs/%.wat) $(_STK_OBJECTS:%=objs/%.wat) $(_BGL5_OBJECTS:%=objs/%.wat)
WEOBJECTS	= $(EOBJECTS:%.o=%.wat)

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm) \
                  $(_STK_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.bgl) \
                  $(_STK_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.stk)

SOURCES		= $(_BGL_SOURCES)
 
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

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .stk

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
objs/%.o: $(PKG_SRC_DIR)/%.stk
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" $< -o $@ -c

objs/%.o: $(PKG_SRC_DIR)/%.scm
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" $< -o $@ -c

objs/%.o: $(BIGLOO_SRC_DIR)/%.bgl $(BIGLOO_SRC_DIR)/%.stk
	$(BIGLOO) -suffix stk $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" $^ -o $@ -c -module4

$(CLASS_DIR)/%.class: $(PKG_SRC_DIR)/%.stk
	$(BIGLOO) -jvm $(EFLAGS) $(BJVMFLAGS) $(BAPIFLAGS) -c -jvm-purify $< -o $@

$(CLASS_DIR)/%.class: $(PKG_SRC_DIR)/%.scm
	$(BIGLOO) -jvm $(EFLAGS) $(BJVMFLAGS) $(BAPIFLAGS) -c -jvm-purify $< -o $@

$(CLASS_DIR)/%.class: $(BIGLOO_SRC_DIR)/%.bgl $(BIGLOO_SRC_DIR)/%.stk
	$(BIGLOO) -suffix stk -jvm  $(EFLAGS) $(BJVMFLAGS) $(BAPIFLAGS) -c -jvm-purify $^ -o $@ -module4

objs/%.wat: $(BIGLOO_SRC_DIR)/%.bgl $(BIGLOO_SRC_DIR)/%.stk
	$(BIGLOO) -wasm -suffix stk $(EFLAGS) $(BAPIFLAGS) $^ -o $@ -c -module4

