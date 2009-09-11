#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/ssl/src/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Thu Sep 10 11:31:36 2009 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Ssl configuration                                       */
#*---------------------------------------------------------------------*/
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME	= ssl
LIBNAME		= bigloo$(TARGETNAME)
# Directories
LIB         	= $(BOOTLIBDIR)
BIGLOO_SRC_DIR 	= Llib
DOTNET_SRC_DIR 	= DotNet
JAVA_SRC_DIR 	= Java
C_SRC_DIR 	= C
MISC_SRC_DIR 	= Misc
WIN32_SRC_DIR 	= Win32
# The heap file
HEAP_FILE	= $(LIB)/$(TARGETNAME).heap
HEAPJVM_FILE	= $(LIB)/$(TARGETNAME).jheap
# Where to store the library class files
PBASE		= bigloo.$(TARGETNAME)
CLASS_DIR	= objs/class_s/bigloo/$(TARGETNAME)
CLASS_EDIR	= objs/class_e/bigloo/$(TARGETNAME)
DOTNET_OBJ_DIR	= objs/dotnet_s
DOTNET_OBJ_EDIR	= objs/dotnet_e
# The Bigloo compiler
BIGLOO		= $(BGLBUILDBIGLOO)
AFILE		= $(BGLBUILDBINDIR)/$(AFILE_EXE)
JFILE		= $(BGLBUILDBINDIR)/$(JFILE_EXE)
DEPEND		= $(BGLBUILDBINDIR)/$(BDEPEND_EXE)
BTAGS		= $(BGLBUILDBINDIR)/$(BTAGS_EXE)
# Bigloo compilation options
BFTFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(LIB) -I Llib \
                   -heap-library $(TARGETNAME)
BJVMFLAGS       := $(BJVMFLAGS)
# Additional SSL lib for java
SSL_ZIP		= commons-ssl.zip
JVMAPIBOOTPATH  := $(JVMAPIBOOTPATH):$(LIB)/$(SSL_ZIP)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= ssl

_C_OBJECTS	= bglssl
_JAVA_CLASSES	= ssl_client_socket ssl_server_socket certificate private_key
_DOTNET_OBJ	= # bglssl

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

EOBJECTS	= objs/make-lib.o

BGL_CLASSES	= $(_BGL_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make-lib.class

JAVA_CLASSES	= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.class)
JAVA_CLASSES_SRC= $(_JAVA_CLASSES:%=$(CLASS_DIR)/%.java)

BGL_DOTNET_OBJ	= $(_BGL_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
DOTNET_OBJ	= $(_DOTNET_OBJ:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

DOTNET_OBJ_SRC	= # $(_DOTNET_OBJ:%=$(DOTNET_OBJ_DIR)/%.cs)

_WIN32_SRC	= 

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)
_C_SOURCES	= $(_C_OBJECTS:%=$(C_SRC_DIR)/%.c)
_JAVA_SOURCES	= $(_JAVA_CLASSES:%=$(JAVA_SRC_DIR)/%.java)
_DOTNET_SOURCES	= $(_DOTNET_OBJ:%=$(DOTNET_SRC_DIR)/%.cs)
_WIN32_SOURCES	= $(_WIN32_SRC:%=$(WIN32_SRC_DIR)/%)

SOURCES		= $(_BGL_SOURCES) \
                  $(_C_SOURCES) \
                  $(_JAVA_SOURCES) \
                  $(_DOTNET_SOURCES)
INCLUDES	= $(C_SRC_DIR)/bglssl.h

#*---------------------------------------------------------------------*/
#*    Java objects                                                     */
#*---------------------------------------------------------------------*/
ifeq ($(JAVA_VERSION), 1.6)
  JAVA_CLASSES_SRC+=$(CLASS_DIR)/certificate.java $(CLASS_DIR)/private_key.java
else
 ifeq ($(JAVA_VERSION), 1.5)
   JAVA_CLASSES_SRC+=$(CLASS_DIR)/certificate.java $(CLASS_DIR)/private_key.java
 endif
endif

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(C_SRC_DIR)/bglssl.h.in $(_WIN32_SOURCES) \
                  $(JAVA_SRC_DIR)/ssl_client_socket13.java \
                  $(JAVA_SRC_DIR)/ssl_client_socket15.java \
                  $(JAVA_SRC_DIR)/ssl_server_socket13.java \
                  $(JAVA_SRC_DIR)/ssl_server_socket15.java \
                  $(JAVA_SRC_DIR)/certificate.java \
                  $(JAVA_SRC_DIR)/private_key.java \
		  $(MISC_SRC_DIR)/make-lib.scm \
                  $(MISC_SRC_DIR)/$(TARGETNAME).init.in \
                  Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: c jvm dotnet

all: 
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
             $(MAKE) c; \
	  fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) jvm; \
	  fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
             $(MAKE) dotnet;  \
	  fi

c: lib-c heap-c
jvm: lib-jvm heap-jvm
dotnet: lib-dotnet heap-jvm

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .class .java .o .cs .obj

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
objs/%.o: $(BIGLOO_SRC_DIR)/%.scm
	$(BIGLOO) $(EFLAGS) $(BFTFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" $< -o $@ -c

objs/%.o: $(MISC_SRC_DIR)/%.scm
	$(BIGLOO) $(EFLAGS) $(BFTFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" $< -o $@ -c

objs/%.o: $(C_SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CPICFLAGS) -I. -I $(LIB) $(GCINCLUDE) -I $(C_SRC_DIR) $< -o $@ -c

$(CLASS_DIR)/%.class: $(BIGLOO_SRC_DIR)/%.scm
	$(BIGLOO) -jvm  $(EFLAGS) $(BJVMFLAGS) $(BFTFLAGS) -c -jvm-purify $< -o $@

$(CLASS_DIR)/%.java: $(JAVA_SRC_DIR)/%.java
	cp $< $@

$(CLASS_EDIR)/%.class: $(MISC_SRC_DIR)/%.scm
	$(BIGLOO) -jvm  $(EFLAGS) $(BJVMFLAGS) $(BFTFLAGS) -c -jvm-purify $< -o $@

$(DOTNET_OBJ_DIR)/%.cs: $(DOTNET_SRC_DIR)/%.cs
	cp $< $@

$(DOTNET_OBJ_DIR)/%.obj: $(BIGLOO_SRC_DIR)/%.scm
	$(BIGLOO) -dotnet \
            -dotnet-mono-workaround -dotnet-pnet-workaround \
             $(EFLAGS) $(BDOTNETFLAGS) $(BFTFLAGS) -c -dotnet-managed $< -o $@

$(DOTNET_OBJ_EDIR)/%.obj: $(MISC_SRC_DIR)/%.scm
	$(BIGLOO) -dotnet \
            -dotnet-mono-workaround -dotnet-pnet-workaround \
             $(EFLAGS) $(BDOTNETFLAGS) $(BFTFLAGS) -c -dotnet-managed $< -o $@

#*---------------------------------------------------------------------*/
#*    The heap construction                                            */
#*---------------------------------------------------------------------*/
.PHONY: heap heap-c heap-jvm

heap: heap-c heap-jvm
heap-c: $(HEAP_FILE)
heap-jvm: $(HEAPJVM_FILE)

$(HEAP_FILE): .afile $(MISC_SRC_DIR)/make-lib.scm
	@ $(RM) -f $(HEAP_FILE)
	@ $(BIGLOO) $(EFLAGS) $(BHEAPFLAGS) $(MISC_SRC_DIR)/make-lib.scm -addheap $(HEAP_FILE)
	@ echo "$@ done..."
	@ echo "-------------------------------"

$(HEAPJVM_FILE): .jfile .afile $(MISC_SRC_DIR)/make-lib.scm
	@ $(RM) -f $(HEAPJVM_FILE)
	@ $(BIGLOO) -jvm  $(EFLAGS) $(BHEAPFLAGS) $(MISC_SRC_DIR)/make-lib.scm -addheap $(HEAPJVM_FILE)
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    lib                                                              */
#*---------------------------------------------------------------------*/
.PHONY: lib lib-c lib-jvm lib-dotnet

lib: lib-c lib-jvm lib-dotnet

#*--- lib-c -----------------------------------------------------------*/
lib-c: $(TAGS) .afile objs
	@ $(MAKE) lib-c-static && \
          $(MAKE) lib-c-shared

objs:
	mkdir -p $@

lib-c-static: $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a \
              $(LIB)/lib$(LIBNAME)_u-$(RELEASE).a

lib-c-shared:
	if [ "$(SHAREDLIBRARYSUPPORT)" = "yes" ]; then \
           $(MAKE) lib-c.$(SHAREDSUFFIX); \
        fi

lib-c.$(SHAREDSUFFIX):
	@ $(MAKE) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) && \
	  $(MAKE) $(LIB)/lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX) && \
          $(MAKE) $(LIB)/lib$(LIBNAME)_e-$(RELEASE).$(SHAREDSUFFIX)

$(LIB)/lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX): $(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX)
	@ cd $(LIB) && \
        $(RM) -f lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX) && \
        $(LN_S) lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) \
              lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX)

$(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX): .afile $(OBJECTS)
	@ $(MAKE) shared-lib \
             FORCELD=true \
             LIBDEST=$@ \
             SONAME=lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) \
             EXTRALIBS="$(OPENSSLLIBS)" \
             CLOSELIBS="-lbigloo_s-$(RELEASE)"

$(LIB)/lib$(LIBNAME)_e-$(RELEASE).$(SHAREDSUFFIX): .afile $(EOBJECTS)
	@ $(MAKE) shared-lib \
             OBJECTS=$(EOBJECTS) \
             FORCELD=true \
             LIBDEST=$@ \
             SONAME=lib$(LIBNAME)_e-$(RELEASE).$(SHAREDSUFFIX) \
             EXTRALIBS="$(OPENSSLLIBS)" \
             LDLIBS="-l$(LIBNAME)_s-$(RELEASE) $(LDLIBS)" \
             CLOSELIBS="-l$(LIBNAME)_s-$(RELEASE) -lbigloo_s-$(RELEASE)"

$(LIB)/lib$(LIBNAME)_u-$(RELEASE).a: $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ cd $(LIB) && \
        $(RM) -f lib$(LIBNAME)_u-$(RELEASE).a && \
        $(LN_S) lib$(LIBNAME)_s-$(RELEASE).a lib$(LIBNAME)_u-$(RELEASE).a

$(LIB)/lib$(LIBNAME)_s-$(RELEASE).a: .afile $(OBJECTS)
	@ $(RM) -f $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ $(AR) $(ARFLAGS) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a $(OBJECTS)
	@ $(RANLIB) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*--- lib-jvm ---------------------------------------------------------*/
lib-jvm: $(JAVA_SRC_DIR)/ssl_client_socket.java \
         $(JAVA_SRC_DIR)/ssl_server_socket.java
	@ $(MAKE) lib-jvmfull

lib-jvmfull: .jfile $(TAGS) $(CLASS_DIR) $(CLASS_EDIR)
	@ $(MAKE) lib.zip

$(CLASS_DIR): 
	mkdir -p $(CLASS_DIR)

$(CLASS_EDIR): 
	mkdir -p $(CLASS_EDIR)

lib.zip: $(LIB)/bigloo$(TARGETNAME)_s-$(RELEASE).zip \
         $(LIB)/bigloo$(TARGETNAME)_e-$(RELEASE).zip \
         $(LIB)/$(SSL_ZIP)

$(LIB)/$(SSL_ZIP): ../lib/$(SSL_ZIP)
	@ echo Copying $(SSL_ZIP)
	@ cp $< $@

$(LIB)/bigloo$(TARGETNAME)_s-$(RELEASE).zip: .afile .jfile $(BGL_CLASSES) dojavac
	@ $(RM) -f $@
	@ (cd objs/class_s && \
	  $(ZIP) -q $(ZFLAGS) $@ $(CLASS_DIR:objs/class_s/%=%)/*.class)
	@ echo "$@ done..."
	@ echo "-------------------------------"

dojavac: $(JAVA_CLASSES_SRC) $(LIB)/$(SSL_ZIP)
	(cd objs/class_s && \
          $(JAVAC) $(CYGWINJVMPATH) -classpath $(JVMAPIBOOTPATH) $(JAVA_CLASSES_SRC:objs/class_s/%=%))

$(LIB)/bigloo$(TARGETNAME)_e-$(RELEASE).zip: .afile .jfile $(BGL_ECLASSES)
	@ $(RM) -f $@
	@ (cd objs/class_e && \
	  $(ZIP) -q $(ZFLAGS) $@ $(CLASS_EDIR:objs/class_e/%=%)/*.class)
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*--- lib-dotnet ---------------------------------------------------------*/
lib-dotnet: $(TAGS) $(DOTNET_OBJ_DIR) $(DOTNET_OBJ_EDIR)
	@ $(MAKE) lib.dll

$(DOTNET_OBJ_DIR):
	mkdir -p $(DOTNET_OBJ_DIR)

$(DOTNET_OBJ_EDIR):
	mkdir -p $(DOTNET_OBJ_EDIR)

lib.dll: 
	@ $(MAKE) $(LIB)/$(LIBNAME)_u-$(RELEASE).dll
	@ $(MAKE) $(LIB)/$(LIBNAME)_e-$(RELEASE).dll

$(LIB)/$(LIBNAME)_u-$(RELEASE).dll: .afile $(BGL_DOTNET_OBJ) $(DOTNET_OBJ_SRC)
	@ $(RM) -f $(LIB)/$(LIBNAME)_s-$(RELEASE).dll
	@ $(RM) -f $(LIB)/$(LIBNAME)_u-$(RELEASE).dll
	@ (cd $(DOTNET_OBJ_DIR) && \
	  $(DOTNETCSCC) \
		$(DOTNETFTDLLPATH) $(DOTNETLINKBIGLOODLL) \
                -shared -o $(LIB)/$(LIBNAME)_s-$(RELEASE).dll \
                $(DOTNET_OBJ_SRC:$(DOTNET_OBJ_DIR)/%=%) \
		$(BGL_DOTNET_OBJ:$(DOTNET_OBJ_DIR)/%=%))
	@ echo "$@ done..."
	 (cd objs/dotnet_s && \
	  $(DOTNETCSCC) \
		$(DOTNETFTDLLPATH) $(DOTNETLINKBIGLOODLL) \
                -shared -o $(LIB)/$(LIBNAME)_u-$(RELEASE).dll \
                $(DOTNET_OBJ_SRC:$(DOTNET_OBJ_DIR)/%=%) \
		$(BGL_DOTNET_OBJ:$(DOTNET_OBJ_DIR)/%=%))
	@ echo "$@ done..."
	@ echo "-------------------------------"

$(LIB)/$(LIBNAME)_e-$(RELEASE).dll: .afile $(BGL_DOTNET_EOBJ)
	@ $(RM) -f $(LIB)/$(LIBNAME)_e-$(RELEASE).dll
	@ (cd $(DOTNET_OBJ_EDIR) && \
	  $(DOTNETCSCC) \
		$(DOTNETFTDLLPATH) $(DOTNETLINKBIGLOODLL) \
                 -shared -o $(LIB)/$(LIBNAME)_e-$(RELEASE).dll \
                            $(LIB)/$(LIBNAME)_u-$(RELEASE).dll \
		$(BGL_DOTNET_EOBJ:$(DOTNET_OBJ_EDIR)/%=%))
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile

.afile: $(_BGL_SOURCES)
	@ $(AFILE) -o .afile $(_BGL_SOURCES) 

.jfile: $(_BGL_SOURCES)
	@ $(JFILE) -o .jfile \
                   -I $(BIGLOO_SRC_DIR) -I $(MISC_SRC_DIR) \
                   -pbase $(PBASE) $(_BGL_OBJECTS:%=%.scm) make-lib.scm

.etags: $(_BGL_SOURCES)
	@ $(BTAGS) -o .etags $(_BGL_SOURCES)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: 
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(DEPEND) -fno-mco \
                    -search-path $(BIGLOO_SRC_DIR) \
                    -strict-obj-dir objs \
                    $(_BGL_SOURCES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    Java auconfiguration                                             */
#*---------------------------------------------------------------------*/
ifeq ($(JAVA_VERSION), 1.6)
  JDK=15
 else
  ifeq ($(JAVA_VERSION), 1.5)
    JDK=15
  else
    JDK=13
 endif
endif

$(JAVA_SRC_DIR)/ssl_client_socket.java: \
    $(JAVA_SRC_DIR)/ssl_client_socket$(JDK).java
	echo "// Generated file, don't edit" > $@
	cat $< >> $@

$(JAVA_SRC_DIR)/ssl_server_socket.java: \
    $(JAVA_SRC_DIR)/ssl_server_socket$(JDK).java
	echo "// Generated file, don't edit" > $@
	cat $< >> $@

#*---------------------------------------------------------------------*/
#*    stdclean                                                         */
#*---------------------------------------------------------------------*/
stdclean:
	$(RM) -f $(OBJECTS) $(_BGL_OBJECTS:%=%.c)
	$(RM) -f $(LIB)/lib$(LIBNAME)_s.$(SHAREDSUFFIX)
	$(RM) -f $(LIB)/lib$(LIBNAME)_u.$(SHAREDSUFFIX)
	$(RM) -f $(LIB)/lib$(LIBNAME)_e.$(SHAREDSUFFIX)
	$(RM) -f $(LIB)/bigloo$(TARGETNAME)_s-$(RELEASE).zip
	$(RM) -f $(LIB)/bigloo$(TARGETNAME)_u-$(RELEASE).zip
	$(RM) -f $(LIB)/bigloo$(TARGETNAME)_e-$(RELEASE).zip
	$(RM) -f $(LIB)/$(LIBNAME)_s-$(RELEASE).dll
	$(RM) -f $(LIB)/$(LIBNAME)_u-$(RELEASE).dll
	$(RM) -f $(LIB)/$(LIBNAME)_e-$(RELEASE).dll
	$(RM) -f $(LIB)/$(TARGETNAME).jheap $(LIB)/$(TARGETNAME).heap
	$(RM) -rf $(CLASS_DIR)
	$(RM) -rf $(DOTNET_OBJ_DIR)
	$(RM) -f $(JAVA_SRC_DIR)/ssl_client_socket.java
	$(RM) -f $(JAVA_SRC_DIR)/ssl_server_socket.java
	$(RM) -f *~
	$(RM) -f *.mco
	$(RM) -f *.ast

udeclean:
	$(RM) -f .afile .etags .jfile

cleanall: stdclean
	$(RM) -f $(LIB)/$(TARGETNAME).init

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=ssl/src/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean: stdclean
	$(RM) -rf objs

cleanall: stdclean udeclean

distclean: cleanall
