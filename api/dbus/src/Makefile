#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/dbus/src/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Jul  9 10:08:09 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the library.                               */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard dbus configuration                                      */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig
include ../../../Makefile.config
include ../../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
API		= dbus
# Directories
BIGLOO_SRC_DIR 	= Llib
C_SRC_DIR 	= Clib
MISC_SRC_DIR 	= Misc
# The heap file
HEAP_FILE	= $(BOOTLIBDIR)/$(API).heap
# The Bigloo compiler
AFILE		= $(BGLBUILDBINDIR)/bglafile.sh
JFILE		= $(BGLBUILDBINDIR)/bgljfile.sh
DEPEND		= $(BGLBUILDBINDIR)/bglbdepend.sh
BTAGS		= $(BGLBUILDBINDIR)/bgltags.sh
# Bigloo compilation options
BAPIFLAGS	= $(BFLAGS) -I Llib -lib-dir $(BOOTLIBDIR) -unsafe -safee \
                   -copt "$(CDBUSFLAGS)" 
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(BOOTLIBDIR) -I Llib \
                   -heap-library $(API)

# Extra C lib
EXTRALIBS="$(DBUSLIBS)"

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= dbus

_C_OBJECTS	= bgldbus

_OBJECTS	= $(_C_OBJECTS) $(_BGL_OBJECTS)
OBJECTS		= $(_OBJECTS:%=objs/%.o)

EOBJECTS	= objs/make-lib.o

_BGL_SOURCES	= $(_BGL_OBJECTS:%=$(BIGLOO_SRC_DIR)/%.scm)
_C_SOURCES	= $(_C_OBJECTS:%=$(C_SRC_DIR)/%.c)

SOURCES		= $(_BGL_SOURCES) \
                  $(_C_SOURCES) \
INCLUDES	= $(BIGLOO_SRC_DIR)/dbus.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(C_SRC_DIR)/bgldbus.h \
		  $(MISC_SRC_DIR)/make-lib.scm \
                  $(MISC_SRC_DIR)/$(API).init.in \
                  Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: c build-c

all: 
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
             $(MAKE) c; \
	  fi

c: build-c
build-c: dbusheaders api-c

$(BOOTLIBDIR)/$(API).init: $(MISC_SRC_DIR)/$(API).init.in
	@ echo "*** ERROR: $@ is out of date (older than $?)"
	@ echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""
	@ exit 1

#*---------------------------------------------------------------------*/
#*    Headers                                                          */
#*---------------------------------------------------------------------*/
dbusheaders:
	cp $(C_SRC_DIR)/bglconnection.h $(BOOTLIBDIR)/bglconnection.h
	cp $(C_SRC_DIR)/bglmessage.h $(BOOTLIBDIR)/bglmessage.h
	cp $(C_SRC_DIR)/bglservice.h $(BOOTLIBDIR)/bglservice.h


#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo $(POPULATION:%=$(API)/src/%)

#*---------------------------------------------------------------------*/
#*    extra-clean                                                      */
#*---------------------------------------------------------------------*/
clean: api-clean
	$(RM) -f $(JAVA_SRC_DIR)/ssl_client_socket.java
	$(RM) -f $(JAVA_SRC_DIR)/ssl_server_socket.java

cleanall: api-cleanall
	$(RM) $(BOOTLIBDIR)/$(SSL_ZIP)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    Common API rules                                                 */
#*---------------------------------------------------------------------*/
include ../../Makefile.api
