#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdl/src/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sat Feb  7 07:10:52 2009 (serrano)                */
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
TARGETNAME	= bdl
LIBNAME		= bigloobdl
# Directories
LIB         	= $(BOOTLIBDIR)
# The heap file
HEAP_FILE	= $(LIB)/$(TARGETNAME).heap
HEAPJVM_FILE	= $(LIB)/$(TARGETNAME).jheap
# Where to store the library class files
PBASE		= bigloo.bdl
CLASS_DIR	= objs/class_s/bigloo/bdl
# The Bigloo compiler
BIGLOO		= $(BOOTBINDIR)/bigloo
AFILE		= $(BOOTBINDIR)/$(AFILE_EXE)
JFILE		= $(BOOTBINDIR)/$(JFILE_EXE)
DEPEND		= $(BOOTBINDIR)/$(BDEPEND_EXE)
BTAGS		= $(BOOTBINDIR)/$(BTAGS_EXE)
# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L $(LIB) -heap-library bdl
BFLAGS		+= -lib-dir $(LIB)

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
#*    all & jvm                                                        */
#*---------------------------------------------------------------------*/
all: c jvm
c: init lib-c heap-c
jvm: init lib-jvm heap-jvm

#*---------------------------------------------------------------------*/
#*    Init                                                             */
#*---------------------------------------------------------------------*/
init: $(LIB)/bdl.init

$(LIB)/bdl.init: bdl.init
	cat bdl.init > $(LIB)/bdl.init

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .class .java .o

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
objs/%.o: %.scm
	$(BIGLOO) -no-hello $(BFLAGS) -copt $(CPICFLAGS) $< -o $@ -c

objs/%.o: %.c
	$(CC) $(CFLAGS) $(CPICFLAGS) -I. -I $(LIB) $< -o $@ -c

$(CLASS_DIR)/%.class: %.scm
	$(BIGLOO) -no-hello -jvm $(BJVMFLAGS) $(BFLAGS) -c $< -o $@

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
lib-c: $(TAGS) .afile $(CLASS_DIR) lib-c-static lib-c-shared

lib-c-static: $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a \
              $(LIB)/lib$(LIBNAME)_u-$(RELEASE).a

lib-c-shared:
	if [ "$(SHAREDLIBRARYSUPPORT)" = "yes" ]; then \
	  $(MAKE) lib_s.$(SHAREDSUFFIX); \
        fi

lib_s.$(SHAREDSUFFIX): $(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) \
                       $(LIB)/lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX)

$(LIB)/lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX): \
         $(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX)
	@ cd $(LIB) && \
        $(RM) -f lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX) && \
        $(LN_S) lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) \
              lib$(LIBNAME)_u-$(RELEASE).$(SHAREDSUFFIX)

$(LIB)/lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX): .afile $(OBJECTS)
	@ $(MAKE) shared-lib \
             LIBDEST=$@ \
             SONAME=lib$(LIBNAME)_s-$(RELEASE).$(SHAREDSUFFIX) \
             CLOSELIBS="-lbigloo_s-$(RELEASE)"

$(LIB)/lib$(LIBNAME)_u-$(RELEASE).a: $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ cd $(LIB) && \
        $(RM) -f lib$(LIBNAME)_u-$(RELEASE).a && \
        $(LN_S) lib$(LIBNAME)_s-$(RELEASE).a lib$(LIBNAME)_u-$(RELEASE).a

$(LIB)/lib$(LIBNAME)_s-$(RELEASE).a: .afile $(OBJECTS)
	@ $(RM) -f $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ $(AR) $(ARFLAGS) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a $(OBJECTS)
	@ $(RANLIB) $(LIB)/lib$(LIBNAME)_s-$(RELEASE).a
	@ echo "$(LIB)/lib$(LIBNAME)_s-$(RELEASE).a Done..."
	@ echo "-------------------------------"

#*--- lib-jvm ---------------------------------------------------------*/
lib-jvm: $(TAGS) $(CLASS_DIR) lib_s.zip

$(CLASS_DIR): 
	mkdir -p $(CLASS_DIR)

lib_s.zip: .afile .jfile $(BGL_CLASSES) $(JAVA_CLASSES_SRC) dojavac
	@ $(RM) -f $(LIB)/$(TARGETNAME)_s.zip
	@ (cd objs/class_s && \
	  $(ZIP) -q $(ZFLAGS) \
                 $(LIB)/$(TARGETNAME)_s.zip \
                 $(CLASS_DIR:objs/class_s/%=%)/*.class)
	@ echo "$(LIB)/lib$(TARGETNAME)_s.zip done..."
	@ echo "-------------------------------"

dojavac: 
	if [ "$(JAVA_CLASSES_SRC) " != " " ]; then \
	   echo $(JAVA_CLASSES_SRC); \
	   (CLASSPATH=$(LIB)/bigloo_s.zip:$$CLASSPATH; export CLASSPATH; \
            cd objs/class_s/bigloo && \
            $(JAVAC) $(CYGWINJVMPATH) $(JAVA_CLASSES_SRC:objs/class_s/bigloo/%=%)) \
        fi

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(_BGL_SOURCES) 

.jfile: $(SOURCES)
	@ $(JFILE) -o .jfile -pbase $(PBASE) $(SOURCES) 

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(_BGL_SOURCES)

#*---------------------------------------------------------------------*/
#*    stdclean                                                         */
#*---------------------------------------------------------------------*/
stdclean:
	$(RM) -f $(OBJECTS) $(_BGL_OBJECTS:%=%.c)
	$(RM) -f $(LIB)/lib$(LIBNAME)_s.$(SHAREDSUFFIX)
	$(RM) -f $(LIB)/lib$(LIBNAME)_u.$(SHAREDSUFFIX)
	$(RM) -f $(LIB)/$(TARGETNAME)_s.zip
	$(RM) -f $(LIB)/$(TARGETNAME)_u.zip
	$(RM) -f $(LIB)/$(TARGETNAME).jheap $(LIB)/$(TARGETNAME).heap
	$(RM) -f $(LIB)/$(TARGETNAME).init
	$(RM) -f .afile .etags .jfile
	$(RM) -rf $(CLASS_DIR)
	$(RM) -f *~
	$(RM) -f *.mco
	$(RM) -f *.ast

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=bdl/src/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean: stdclean
	$(RM) -rf objs

cleanall: stdclean

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
