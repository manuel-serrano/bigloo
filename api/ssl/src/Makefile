#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/ssl/src/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Jul  9 10:18:03 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Ssl configuration                                       */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= ssl
# Directories
BIGLOO_SRC_DIR 	= Llib
JAVA_SRC_DIR 	= Java
C_SRC_DIR 	= C
MISC_SRC_DIR 	= Misc
WIN32_SRC_DIR 	= Win32
# Where to store the library class files
CLASS_DIR	= objs/class_s/bigloo/$(API)
CLASS_EDIR	= objs/class_es/bigloo/$(API)
# The Bigloo compiler
AFILE		= $(BGLBUILDBINDIR)/bglafile.sh
JFILE		= $(BGLBUILDBINDIR)/bgljfile.sh
DEPEND		= $(BGLBUILDBINDIR)/bglbdepend.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee \
                   -copt $(OPENSSLLIBSCFLAGS)
CAPIFLAGS       = $(OPENSSLLIBSCFLAGS) 
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                   -heap-library $(API)
BJVMFLAGS       := $(BJVMFLAGS)

# C flags
CEFLAGS		= $(CAPIFLAGS)

# Additional SSL lib for C
EXTRALIBS	+= $(OPENSSLLIBS)

# Minimum Java version for SSL JVM backend
MIN_JAVA_VERSION = 8

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= ssl

_C_OBJECTS	= bglssl
_JAVA_CLASSES	= certificate private_key ctx pem_utils ssl_utils hash_ctx hmac_ctx sign_ctx verify_ctx cipher_ctx dh_ctx
_JAVA_ECLASSES	= ssl_client_socket ssl_server_socket 

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

EOBJECTS	= objs/make_lib.o

BGL_CLASSES	= $(_BGL_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make_lib.class

JAVA_CLASSES	= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.class) \
                  $(_JAVA_ECLASSES:%=$(CLASS_DIR)/%.class)
JAVA_CLASSES_SRC= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.java) \
                  $(_JAVA_ECLASSES:%=$(CLASS_DIR)/%.java)

_WIN32_SRC	= 

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)
_C_SOURCES	= $(_C_OBJECTS:%=$(C_SRC_DIR)/%.c)
_JAVA_SOURCES	= $(_JAVA_CLASSES:%=$(JAVA_SRC_DIR)/%.java)
_WIN32_SOURCES	= $(_WIN32_SRC:%=$(WIN32_SRC_DIR)/%)

SOURCES		= $(_BGL_SOURCES) \
                  $(_C_SOURCES) \
                  $(_JAVA_SOURCES)
INCLUDES	= $(C_SRC_DIR)/bglssl.h $(C_SRC_DIR)/root_certs.h \
                  $(C_SRC_DIR)/ssl_debug.h

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(C_SRC_DIR)/bglssl.h.in $(_WIN32_SOURCES) \
                  $(JAVA_SRC_DIR)/ssl_client_socket.java \
                  $(JAVA_SRC_DIR)/ssl_server_socket.java \
                  $(JAVA_SRC_DIR)/certificate.java \
                  $(JAVA_SRC_DIR)/private_key.java \
                  $(JAVA_SRC_DIR)/ctx.java \
                  $(JAVA_SRC_DIR)/pem_utils.java \
                  $(JAVA_SRC_DIR)/ssl_utils.java \
                  $(JAVA_SRC_DIR)/hash_ctx.java \
                  $(JAVA_SRC_DIR)/hmac_ctx.java \
                  $(JAVA_SRC_DIR)/sign_ctx.java \
                  $(JAVA_SRC_DIR)/verify_ctx.java \
                  $(JAVA_SRC_DIR)/cipher_ctx.java \
                  $(JAVA_SRC_DIR)/dh_ctx.java \
		  $(MISC_SRC_DIR)/make_lib.scm \
                  $(MISC_SRC_DIR)/$(API).init.in \
	          $(C_SRC_DIR)/root_certs.h \
	          $(C_SRC_DIR)/ssl_debug.h \
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
build-jvm: lib-jvm

$(MISC_SRC_DIR)/$(API).init: $(MISC_SRC_DIR)/$(API).init.in
	@ echo "*** ERROR: $@ is out of date (older than $?)"
	@ echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""
	@ exit 1

.PHONY: lib-jvm check-java-version

check-java-version:
	@ java_ver=$(JAVA_VERSION); \
	  min_ver=$(MIN_JAVA_VERSION); \
	  java_major=`echo $$java_ver | sed -e 's/^1\.//' -e 's/\..*//'`; \
	  if [ "$$java_major" -lt "$$min_ver" ] 2>/dev/null; then \
	    echo "*** ERROR: SSL JVM backend requires Java >= $(MIN_JAVA_VERSION), found $(JAVA_VERSION)"; \
	    exit 1; \
	  fi

lib-jvm: check-java-version
	@ $(MAKE) api-jvm
	@ $(MAKE) jvm-extra

jvm-extra: $(JAVA_CLASSES_SRC)
	(cd objs/class_s && \
          $(JAVAC) $(CYGWINJVMPATH) -classpath $(JVMAPIBOOTPATH) $(JAVA_CLASSES_SRC:objs/class_s/%=%))

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo $(POPULATION:%=$(API)/src/%)

#*---------------------------------------------------------------------*/
#*    ssl.h                                                            */
#*---------------------------------------------------------------------*/
$(C_SRC_DIR)/ssl.h: Llib/ssl.scm
	$(BIGLOO) $(EFLAGS) $(BAPIFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    extra-clean                                                      */
#*---------------------------------------------------------------------*/
clean: api-clean
	$(RM) -f $(C_SRC_DIR)/ssl.h

cleanall: api-cleanall

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    Common API rules                                                 */
#*---------------------------------------------------------------------*/
include ../../Makefile.api

#*---------------------------------------------------------------------*/
#*    explicit depencies                                               */
#*---------------------------------------------------------------------*/
objs/bglssl.o: $(C_SRC_DIR)/bglssl.h $(C_SRC_DIR)/ssl.h \
  $(C_SRC_DIR)/root_certs.h

objs/bglssl.o: $(C_SRC_DIR)/ssl_debug.h
objs/ssl.o: $(C_SRC_DIR)/ssl_debug.h

