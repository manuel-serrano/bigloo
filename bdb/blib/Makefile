#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdb/blib/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Jul  9 10:20:03 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the bdb library.                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard Bdb configuration                                       */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config
include ../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME	= bdb
LIBNAME		= bigloobdb
# Directories
LIB         	= $(BOOTLIBDIR)
# The heap file
HEAP_FILE	= $(LIB)/$(TARGETNAME).heap
HEAPJVM_FILE	= $(LIB)/$(TARGETNAME).jheap
# Where to store the library class files
PBASE		= bigloo.bdb
CLASS_DIR	= objs/class/bigloo/bdb
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(LIB) -heap-library bdb
BFLAGS		+= -lib-dir $(LIB)
# C flags
CFLAGS		= -I$(BGL_INC_DIR) -I. -I$(BOOTLIBDIR) $(CPICFLAGS) $(GCINCLUDE)
# Bigloo flags
BDBFLAGS	= $(BFLAGS) -farithmetic -init-lib

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= bdb env

_C_OBJECTS	= stat table
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
POPULATION	= $(SOURCES) $(INCLUDES) Makefile make_lib.scm

#*---------------------------------------------------------------------*/
#*    all & jvm                                                        */
#*---------------------------------------------------------------------*/
all: lib-c heap-c

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .class .java .o

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
objs/%.o: %.scm
	$(BIGLOO) -no-hello $(BDBFLAGS) -copt $(CPICFLAGS) $< -o $@ -c

objs/%.o: %.c
	$(CC) $(CFLAGS) $(CPICFLAGS) -I. -I $(LIB) $< -o $@ -c

$(CLASS_DIR)/%.class: %.scm
	$(BIGLOO) -no-hello -jvm $(BJVMFLAGS) $(BDBFLAGS) -c $< -o $@

$(CLASS_DIR)/%.java: %.java
	cp $< $@

#*---------------------------------------------------------------------*/
#*    The heap construction                                            */
#*---------------------------------------------------------------------*/
.PHONY: heap heap-c heap-jvm

heap: heap-c heap-jvm
heap-c: $(HEAP_FILE)
heap-jvm: $(HEAPJVM_FILE)

$(HEAP_FILE): .afile make_lib.scm
	@ $(RM) -f $(HEAP_FILE)
	@ $(BIGLOO) $(BHEAPFLAGS) make_lib.scm -addheap $(HEAP_FILE)
	@ echo "$(HEAP_FILE) Done..."
	@ echo "-------------------------------"

$(HEAPJVM_FILE): .jfile .afile make_lib.scm
	@ $(RM) -f $(HEAPJVM_FILE)
	@ $(BIGLOO) -jvm $(BHEAPFLAGS) make_lib.scm -addheap $(HEAPJVM_FILE)
	@ echo "$(HEAPJVM_FILE) Done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    lib                                                              */
#*---------------------------------------------------------------------*/
.PHONY: lib lib-c lib-jvm

lib: lib-c lib-jvm

#*--- lib-c -----------------------------------------------------------*/
lib-c: $(TAGS) .afile $(CLASS_DIR) \
        lib.$(SHAREDSUFFIX) \
        lib.a

lib.$(SHAREDSUFFIX): $(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX)
lib.a: $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a


$(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX): .afile $(OBJECTS)
	@ $(MAKE) bigloo-shared-lib \
             LIBDEST=$@ \
	     LIBPATH=$(BOOTLIBDIR) \
             SONAME=lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) \
             CLOSELIBS="-lbigloo_s-$(RELEASE)"

$(LIB)/lib$(LIBNAME)_s-$(RELEASE).a: .afile $(OBJECTS)
	@ $(RM) -f $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ $(AR) $(ARFLAGS) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a $(OBJECTS)
	@ $(RANLIB) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ echo "$(LIB)/lib$(LIBNAME)_s-$(RELEASE).a Done..."
	@ echo "-------------------------------"

#*--- lib-jvm ---------------------------------------------------------*/
lib-jvm: $(TAGS) $(CLASS_DIR) lib.zip

$(CLASS_DIR): 
	mkdir -p $(CLASS_DIR)

lib.zip: .afile .jfile $(BGL_CLASSES) $(JAVA_CLASSES_SRC) dojavac
	@ $(RM) -f $(LIB)/$(TARGETNAME)_s.zip
	@ (cd objs/class && \
	  $(ZIP) -q $(ZFLAGS) \
                 $(LIB)/$(TARGETNAME)_s.zip \
                 $(CLASS_DIR:objs/class/%=%)/*.class)
	@ echo "$(LIB)/lib$(TARGETNAME)_s.zip done..."
	@ echo "-------------------------------"

dojavac: 
	if [ "$(JAVA_CLASSES_SRC) " != " " ]; then \
	   echo $(JAVA_CLASSES_SRC); \
	   (CLASSPATH=$(LIB)/bigloo.zip:$$CLASSPATH; export CLASSPATH; \
            cd objs/class/bigloo && \
            $(JAVAC) $(CYGWINJVMPATH) $(JAVA_CLASSES_SRC:objs/class/bigloo/%=%)) \
        fi

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile

.afile: $(SOURCES)
	@ $(BGLAFILE) -o .afile $(_BGL_SOURCES) 

.jfile: $(SOURCES)
	@ $(BGLJFILE) -o .jfile -pbase $(PBASE) $(SOURCES) 

.etags: $(SOURCES)
	@ $(BGLTAGS) -o .etags $(_BGL_SOURCES)

#*---------------------------------------------------------------------*/
#*    stdclean                                                         */
#*---------------------------------------------------------------------*/
stdclean:
	$(RM) -f $(OBJECTS) $(_BGL_OBJECTS:%=%.c)
	$(RM) -f $(LIB)/lib$(LIBNAME)_s.$(SHAREDSUFFIX)
	$(RM) -f $(LIB)/$(TARGETNAME)_s.zip
	$(RM) -f $(LIB)/$(TARGETNAME).jheap $(LIB)/$(TARGETNAME).heap
	$(RM) -f $(LIB)/$(TARGETNAME).init
	$(RM) -rf $(CLASS_DIR)
	$(RM) -f *~
	$(RM) -f *.mco
	$(RM) -f *.ast

#*---------------------------------------------------------------------*/
#*    udeclean                                                         */
#*---------------------------------------------------------------------*/
udeclean:
	$(RM) -f .afile .etags .jfile

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean: stdclean
	$(RM) -rf objs

cleanall: stdclean udeclean

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
