#*=====================================================================*/
#*    serrano/prgm/project/bigloo/wasm/runtime/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Mar 21 08:14:18 1995                          */
#*    Last change :  Wed Jan  8 08:39:26 2025 (serrano)                */
#*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The (gnu) makefile to build the Bigloo's library.                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# The Bigloo used to bootstrap the library and to produce the heap file
EFLAGS		=
SAWFLAGS	= -saw
#CSAWFLAGS	= -DBGL_SAW=1 -DBGL_GC=BGL_SAW_GC
CSAWFLAGS	= -DBGL_SAW=1
# the shell to be used
SHELL           = /bin/sh
# configure
CONFIGURE	= $(BOOTDIR)/configure
# The heap file
HEAPC_FILE	= bigloo.heap
HEAPJVM_FILE	= bigloo.jheap
HEAPWASM_FILE	= bigloo.wheap
# Default object file locations
OBJDIR_SYS	= obj_s
OBJDIR_USR	= obj_s
# This two variables control how we are booting (from Scheme or C)
SCMOBJSUFFIX	= %.o
COBJSUFFIX	=
# Default class file locations
CLASSDIR_SYS	= class_s
CLASSDIR_USR	= class_s
# The class suffix
SCMCLASSSUFFIX	= %.class
# The package association file
PBASE		= bigloo.runtime
# The qualifier of the build library
LIBSUFFIX	=
# Wasm types
WASMTYPES	= Wlib/types.wat
# Wasm flags
WFLAGS		= 
# Wasm merge tool
WASMMERE	= $(BOOTDIR)/tools
# Wasm preallocate ints length
WASM_BINTS_PREALLOC_SIZE = 100
WASM_BINTS_PREALLOC_START = -10
WASM_BINTS_PREALLOC = bigloo -eval "(begin (print (cons* (quote array.new_fixed) (quote @DOLLAR@bints-table) $(WASM_BINTS_PREALLOC_SIZE) (map (lambda (i) (list (quote struct.new) (quote @DOLLAR@bint) (list (quote i64.const) i))) (iota $(WASM_BINTS_PREALLOC_SIZE) $(WASM_BINTS_PREALLOC_START))))) (exit 0))"

#*---------------------------------------------------------------------*/
#*    Compilation flags                                                */
#*---------------------------------------------------------------------*/
#*--- General flags ---------------------------------------------------*/
# The Bigloo compilation option used the produce an heap file
BHEAPFLAGS	= $(BOOTFLAGS) $(BFEATUREFLAGS) \
                  -unsafe -q -mkheap -mklib -s -no-hello
BHEAPFLAGSJVM	= $(BHEAPFLAGS) 
BHEAPFLAGSWASM	= $(BOOTFLAGS) \
                  -unsafe -q -mkheap -mklib -s -no-hello
#*--- Bigloo, common --------------------------------------------------*/
BCOMMONFLAGS	= $(BFLAGS) $(BOOTFLAGS) +rm
#*--- Bigloo, safe ----------------------------------------------------*/
BSAFEFLAGS	= -srfi bigloo-safe $(BCOMMONFLAGS) \
                  -O3 -mklib -cc $(CC) -fsharing \
                  -q -unsafev -eval '(set! *indent* 4)'
CSAFEFLAGS	= -I$(BOOTLIBDIR) $(CFLAGS) $(CPICFLAGS) $(CBUILDFLAGS)
#*--- Bigloo JVM flags ------------------------------------------------*/
BGLJFLAGS	= $(BFEATUREFLAGS) -srfi enable-gmp -jfile .jfile
BJSAFEFLAGS	= $(BFEATUREFLAGS) -srfi enable-gmp -jvm-purify
#*--- Bigloo, safe, debug ---------------------------------------------*/
BGSAFEFLAGS	= $(BSAFEFLAGS) -g 
#*--- Bigloo, unsafe, optimize ----------------------------------------*/
BUNSAFEO0FLAGS	= $(BCOMMONFLAGS) \
                  -mklib -cc $(CC) -fsharing -q -no-hello \
                  -unsafe -safee
BUNSAFEFLAGS	= -srfi bigloo-unsafe $(BUNSAFEO0FLAGS) -O4
CUNSAFEFLAGS	= -I$(BOOTLIBDIR) $(CFLAGS) $(CPICFLAGS) $(CBUILDFLAGS)
CSAFEFLAGS	= $(CUNSAFEFLAGS) -g
#*--- Bigloo, unsafe, profiled ----------------------------------------*/
BUNSAFEPFLAGS	= $(BCOMMONFLAGS) \
                  -O4 -mklib -unsafe -safee -cc $(CC) -fsharing -q -no-hello \
                  -p -srfi bigloo-profile
CUNSAFEPFLAGS	= -I$(BOOTLIBDIR) $(CFLAGS_P) $(CPICFLAGS) -DPROFILE
#*--- Bigloo, safe, debug, heap ---------------------------------------*/
BSAFEDBGFLAGS	= $(BSAFEFLAGS) -gheap
#*--- Bigloo, unsafe, heap --------------------------------------------*/
BUNSAFEDBGFLAGS	= $(BUNSAFEFLAGS) -gheap
CDBGFLAGS	= -I$(BOOTLIBDIR) $(CPICFLAGS) $(CUNSAFEFLAGS) $(BACKPTRFLAGS) -DBIGLOO_TRACE

#*---------------------------------------------------------------------*/
#*    The assembler objects.                                           */
#*---------------------------------------------------------------------*/
_OBJ_MLIB	= mach_dep

OBJ_MLIB	= $(_OBJ_MLIB:%=Mlib/%.o)
O_OBJ_MLIB      = $(OBJ_MLIB:%=$(O)/%)

#*---------------------------------------------------------------------*/
#*    C objects                                                        */
#*---------------------------------------------------------------------*/
CLIB		= cinit_obj cmain cerror cports cstring csymbol cvector \
                  cwriter capply cstruct csystem ccontrol ceval chash \
                  ctrace callcc cbinary cforeign cdsssl \
                  cucs2 cunicode cprocess csocket crgc ccustom cdlopen \
                  cdate csleep cthread cmmap csrfi4 cweakptr cbignum cregexp \
                  cobject ci18n cresolv csaw cselect coverflow

C_CLIB		= $(CLIB:%=Clib/%.c)

#*---------------------------------------------------------------------*/
#*    WAT objects                                                      */
#*---------------------------------------------------------------------*/
WLIB		= wcnst wvector javascript runtime wprocedure wwriter wfixnum \
                  wstring wsymbols wrgc wports wintegers wbignum wsystem \
                  wreal wdate wobject wcustom wforeign wweakptr wregexp \
                  wmmap wdenv wbinary

W_WLIB		= $(WLIB:%=Wlib/%.wat)


#*---------------------------------------------------------------------*/
#*    Java objects                                                     */
#*---------------------------------------------------------------------*/
JDK_LIBS	= JDK

ifeq ($(JAVA_VERSION), 20)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 19)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 18)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 17)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 16)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 15)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 14)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 13)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 12)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 11)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 10)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 1.9)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 1.8)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 1.7)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 1.6)
  JDK_LIBS += JDK16
else
ifeq ($(JAVA_VERSION), 1.5)
  JDK_LIBS += JDK15
else
ifeq ($(JAVA_VERSION), 1.4)
  JDK_LIBS += JDK14
else
  JDK_LIBS += JDK13
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif

JLIB		= foreign buffer configure stackwriter\
                  bbool output_string_port output_procedure_port \
		  bchar input_console_port pair \
		  bexception input_file_port  input_mmap_port\
                  procedure \
		  bignum bint input_port real \
		  bllong belong input_string_port input_pipe_port rest \
		  key struct \
		  callcc keyword symbol \
		  cobj cell magic \
		  cnst nil \
		  numeral \
		  custom opaque obj unspecified \
		  eof object generic \
		  exit optional \
		  extended_pair binary_port \
                  output_port output_buffered_port output_pipe_port \
		  socket client_socket server_socket input_socket_port \
                  datagram_socket output_datagram_port input_datagram_port \
                  datagram_client_socket datagram_server_socket \
                  process flusher stack_trace dlopen \
                  bucs2 ucs2string bgldynamic date os \
                  input_procedure_port input_gzip_port input_resource_port \
                  mutex condvar mmap mmaps semaphore \
                  hvector \
                  s8vector u8vector s16vector u16vector \
                  s32vector u32vector s64vector u64vector \
                  f32vector f64vector \
                  weakptr regexp bclass bint8 bint16 bint32 bint64 \
	          $(JDK_LIBS)

JLIB_SRC	= $(JLIB:%=%.java)
JAVA_JLIB	= $(JLIB:%=Jlib/%.java)

#*---------------------------------------------------------------------*/
#*    Scheme IEEE objects                                              */
#*---------------------------------------------------------------------*/
IEEE  		= boolean equiv pairlist char string control number \
                  fixnum flonum port vector symbol input output \
                  control5 dtoa

SCM_IEEE	= $(IEEE:%=Ieee/%.scm)
C_SCM_IEEE	= $(IEEE:%=Ieee/%.c)

#*---------------------------------------------------------------------*/
#*    Scheme R5rs objects                                              */
#*---------------------------------------------------------------------*/
R5RS  		= syntax

SCM_R5RS	= $(R5RS:%=R5rs/%.scm)
C_SCM_R5RS	= $(R5RS:%=R5rs/%.c)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
LLIB		= bexit type bigloo object error struct os hash weakhash \
                  tvector bit binary foreign dsssl ucs2 weakptr \
                  unicode process socket custom date \
                  param thread trace bconfigure mmap srfi4 module i18n \
                  semaphore

SCM_LLIB	= $(LLIB:%=Llib/%.scm)
C_SCM_LLIB	= $(LLIB:%=Llib/%.c)

#*---------------------------------------------------------------------*/
#*    The interpreter objects                                          */
#*---------------------------------------------------------------------*/
EVAL		= eval expand expanders expdlet expdbool expddefine \
                  expdcase expddo expdtry expdstruct evmeaning everror \
                  evcompile evenv progn macro expdquote evprimop \
                  expdsrfi0 expdrecord expdargs expdtrace library \
                  evobject evmodule evutils \
                  evaluate evaluate_types evaluate_avar evaluate_fsize \
                  evaluate_uncomp evaluate_use evaluate_comp

SCM_EVAL	= $(EVAL:%=Eval/%.scm)
C_SCM_EVAL	= $(EVAL:%=Eval/%.c)

#*---------------------------------------------------------------------*/
#*    The Pattern matching compiler objects                            */
#*---------------------------------------------------------------------*/
MATCH		= compiler descr mexpand normalize s2cfun

SCM_MATCH	= $(MATCH:%=Match/%.scm)
C_SCM_MATCH	= $(MATCH:%=Match/%.c)

#*---------------------------------------------------------------------*/
#*    The Regular grammar generator objects                            */
#*---------------------------------------------------------------------*/
RGC		= rgccompile rgcdfa rgcrules rgctree rgcconfig \
                  rgcexpand rgcset rgcposix rgc

SCM_RGC		= $(RGC:%=Rgc/%.scm)
C_SCM_RGC	= $(RGC:%=Rgc/%.c)

#*---------------------------------------------------------------------*/
#*    The Pretty-printer objects                                       */
#*---------------------------------------------------------------------*/
PP		= pp circle

SCM_PP		= $(PP:%=Pp/%.scm)
C_SCM_PP	= $(PP:%=Pp/%.c)

#*---------------------------------------------------------------------*/
#*    The reader                                                       */
#*---------------------------------------------------------------------*/
READ		= reader

SCM_READ	= $(READ:%=Read/%.scm)
C_SCM_READ	= $(READ:%=Read/%.c)

#*---------------------------------------------------------------------*/
#*    The unsafe                                                       */
#*---------------------------------------------------------------------*/
UNSAFE		= intext hmac md5 crc16 sha1 sha2 gunzip tar kmp \
                  base64 bignumber aes rsa url http crc uuid ftp \
                  bm ssr regexp

SCM_UNSAFE	= $(UNSAFE:%=Unsafe/%.scm)
C_SCM_UNSAFE	= $(UNSAFE:%=Unsafe/%.c)

#*---------------------------------------------------------------------*/
#*    The Lalr compiler objects                                        */
#*---------------------------------------------------------------------*/
LALR		= driver gen global lalr rewrite util

SCM_LALR	= $(LALR:%=Lalr/%.scm)
C_SCM_LALR	= $(LALR:%=Lalr/%.c)

#*---------------------------------------------------------------------*/
#*    All source files                                                 */
#*---------------------------------------------------------------------*/
SCM_USR_SRC	= $(SCM_IEEE) $(SCM_LLIB) $(SCM_R5RS)
SCM_SYS_SRC	= $(SCM_READ) $(SCM_RGC) $(SCM_PP) \
                  $(SCM_EVAL) $(SCM_MATCH) $(SCM_LALR) $(SCM_UNSAFE)

#*---------------------------------------------------------------------*/
#*    Src files                                                        */
#*---------------------------------------------------------------------*/
SRC_FILES	= $(SCM_USR_SRC) $(SCM_SYS_SRC) $(SCM_UNSAFE_SRC) \
                  $(C_CLIB) \
                  $(JAVA_JLIB:%/configure.java=) \
                  Mlib/mach_dep.c \
                  Mlib/sparc_sunos4_mach_dep.s Mlib/sparc_sunos5_mach_dep.s \
                  Ilib/realcallback.il

#*---------------------------------------------------------------------*/
#*    Include files                                                    */
#*---------------------------------------------------------------------*/
INCLUDE_FILE	= Include/bigloo.h Include/bigloo_saw.h \
                  Include/bigloo_pair.h Include/bigloo_real.h \
                  Include/bigloo_vector.h Include/bigloo_string.h \
                  Include/bigloo_struct.h Include/bigloo_cell.h \
                  Include/bigloo_int.h Include/bigloo_semaphore.h \
                  Include/bigloo_object.h Include/bigloo_bignum.h \
                  Include/bigloo_exit.h

#*---------------------------------------------------------------------*/
#*    Object files                                                     */
#*    -------------------------------------------------------------    */
#*    The file non-inline_alloc.c is added by configure when non       */
#*    custom GC is selected.                                           */
#*---------------------------------------------------------------------*/
OBJECTS		= $(C_CLIB:%.c=objs/$(OBJDIR_USR)/%.o) \
                  objs/$(OBJDIR_SYS)/Mlib/mach_dep.o \
                  $(SCM_SYS_SRC:%.scm=objs/$(OBJDIR_SYS)/%.o) \
                  $(SCM_USR_SRC:%.scm=objs/$(OBJDIR_USR)/%.o)

GCSTDOBJECTS	= $(GCSTDOBJ:%.o=objs/$(OBJDIR_SYS)/%.o)

#*---------------------------------------------------------------------*/
#*    JVM class files                                                  */
#*---------------------------------------------------------------------*/
CLASSES		= $(SCM_SYS_SRC:%.scm=objs/$(CLASSDIR_SYS)/bigloo/runtime/%.class) \
                  $(SCM_USR_SRC:%.scm=objs/$(CLASSDIR_USR)/bigloo/runtime/%.class)

#*---------------------------------------------------------------------*/
#*    WASM module files                                                */
#*---------------------------------------------------------------------*/
# WASMS		= $(SCM_SYS_SRC:%.scm=objs/$(WASMDIR_SYS)/%.wasm) \
                 $(SCM_USR_SRC:%.scm=objs/$(WASMDIR_USR)/%.wasm)
WASMS = \
	objs/$(WASMDIR_SYS)/Unsafe/base64.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/bm.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/crc.wasm objs/$(WASMDIR_SYS)/Unsafe/crc16.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/ftp.wasm objs/$(WASMDIR_SYS)/Unsafe/gunzip.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/hmac.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/intext.wasm objs/$(WASMDIR_SYS)/Unsafe/kmp.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/md5.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/regexp.wasm objs/$(WASMDIR_SYS)/Unsafe/rsa.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/sha1.wasm objs/$(WASMDIR_SYS)/Unsafe/sha2.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/ssr.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/url.wasm objs/$(WASMDIR_SYS)/Unsafe/uuid.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/aes.wasm \
	objs/$(WASMDIR_SYS)/Unsafe/http.wasm \
        objs/$(WASMDIR_SYS)/Unsafe/tar.wasm \
        objs/$(WASMDIR_SYS)/Unsafe/bignumber.wasm \
	$(LLIB:%=objs/$(WASMDIR_SYS)/Llib/%.wasm) \
	$(IEEE:%=objs/$(WASMDIR_SYS)/Ieee/%.wasm) \
	$(EVAL:%=objs/$(WASMDIR_SYS)/Eval/%.wasm) \
	$(RGC:%=objs/$(WASMDIR_SYS)/Rgc/%.wasm) \
	$(PP:%=objs/$(WASMDIR_SYS)/Pp/%.wasm) \
	$(READ:%=objs/$(WASMDIR_SYS)/Read/%.wasm) \
	$(R5RS:%=objs/$(WASMDIR_SYS)/R5rs/%.wasm) \
	$(MATCH:%=objs/$(WASMDIR_SYS)/Match/%.wasm) \
	$(LALR:%=objs/$(WASMDIR_SYS)/Lalr/%.wasm) \
	# objs/$(WASMDIR_SYS)/Unsafe/pregexp.wasm \
	# objs/$(WASMDIR_SYS)/Unsafe/pcre.wasm objs/$(WASMDIR_SYS)/Unsafe/regex.wasm
#*---------------------------------------------------------------------*/
#*    C Object files for the distribution                              */
#*---------------------------------------------------------------------*/
C_OBJECTS	= $(SCM_SYS_SRC:%.scm=objs/$(OBJDIR_SYS)/%.c) \
                  $(SCM_USR_SRC:%.scm=objs/$(OBJDIR_USR)/%.c)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION_SI	= $(SRC_FILES) \
                  Makefile \
                  Llib/make_lib.scm \
		  Lalr/lalr.sch \
		  Rgc/rgc-node.sch \
		  Eval/byte-code.sch \
		  Llib/hash.sch \
		  Llib/object.sch \
		  Mlib/mach_dep.c \
                  Clib/inline_alloc.c \
                  Clib/non_inline_alloc.c \
                  Clib/gc_intf.c \
                  Clib/inline_alloc_bump.h \
                  Clib/inline_alloc.h \
                  Clib/inline_alloc_thread.h \
                  Clib/cregex.h Clib/cpcre.h \
                  Clib/cregex.h Clib/cpcre2.h
POPULATION	= $(POPULATION_SI) $(INCLUDE_FILE) \
                  Jlib/JDK13.java \
                  Jlib/JDK14.java \
                  Jlib/JDK15.java \
                  Jlib/JDK16.java

#*---------------------------------------------------------------------*/
#*    .SUFFIXES                                                        */
#*---------------------------------------------------------------------*/
.SUFFIXES:

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: all all-c all-jvm

all: all-c all-jvm all-wasm

all-c: heap-c libs-c
all-jvm: heap-jvm libs-jvm
all-wasm: heap-wasm libs-wasm

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	$(MAKE) DISTRIB=true dodistrib

dodistrib: .afile
	@ if [ `pwd` = $(BOOTDIR)/runtime ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) objs-sans-dirs
	@ $(MAKE) distlibs
	@ $(MAKE) .jfile

#*---------------------------------------------------------------------*/
#*    The includes                                                     */
#*    -------------------------------------------------------------    */
#*    This entry point is used to force a regeneration of the          */
#*    include files after a `make clean'.                              */
#*---------------------------------------------------------------------*/
.PHONY: includes

includes: $(BOOTLIBDIR)/bigloo.h \
          $(BOOTLIBDIR)/bigloo_gc.h \
          $(BOOTLIBDIR)/bigloo_config.h

#*---------------------------------------------------------------------*/
#*    The heap construction                                            */
#*---------------------------------------------------------------------*/
.PHONY: heap heap-c heap-jvm heap-wasm

heap: heap-c
heap-c: .afile includes
	$(RM) -f $(BOOTLIBDIR)/$(HEAPC_FILE)
	$(BIGLOO) $(BHEAPFLAGS) $(EFLAGS) -L $(BOOTLIBDIR) Llib/make_lib.scm -heap $(HEAPC_FILE)
	@ echo "\e[1;35m$(HEAPC_FILE)\e[0m done."

heap-jvm: .afile .jfile includes lib-jvm_u
	$(RM) -f $(BOOTLIBDIR)/$(HEAPJVM_FILE)
	$(BIGLOO) -target jvm -jfile .jfile -L $(BOOTLIBDIR) $(BHEAPFLAGSJVM) \
                  $(EFLAGS) Llib/make_lib.scm -heap $(HEAPJVM_FILE)
	@ echo "\e[1;35m$(HEAPJVM_FILE)\e[0m done."

heap-wasm: .afile includes
	$(RM) -f $(BOOTLIBDIR)/$(HEAPWASM_FILE)
	$(BIGLOO) -wasm -target wasm -L $(BOOTLIBDIR) $(BHEAPFLAGSWASM) \
                  $(EFLAGS) Llib/make_lib.scm -heap $(HEAPWASM_FILE)
	@ echo "\e[1;35m$(HEAPWASM_FILE)\e[0m done."

#*---------------------------------------------------------------------*/
#*    Bigloo C libraries compilation                                   */
#*---------------------------------------------------------------------*/
.PHONY: lib lib_s lib_u lib_p lib-c lib-c_u lib-c_p libs libs-c libs-saw

libs:
	@ $(MAKE) lib_u
	@ $(MAKE) lib_s

libs-c:
	@ $(MAKE) lib-c_u
	@ $(MAKE) lib-c_s

libs-saw:
	@ $(MAKE) lib-saw_u
	@ $(MAKE) lib-saw_s

lib: lib_s
lib_s: lib-c_s
lib-c_s: includes
	@ $(RM) -f $(BOOTLIBDIR)/libbigloo_s-$(RELEASE).a.old
	@ $(RM) -f $(BOOTLIBDIR)/st*
	@ if [ -f $(BOOTLIBDIR)/libbigloo_s-$(RELEASE).a ]; then \
            mv $(BOOTLIBDIR)/libbigloo_s-$(RELEASE).a $(BOOTLIBDIR)/libbigloo_s-$(RELEASE).a.old; \
          fi
	$(MAKE) one-lib-c \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BSAFEFLAGS)" \
                OBJDIR_USR=obj_s OBJDIR_SYS=obj_u \
                LIBSUFFIX=_s

lib-saw: lib-saw_s
lib-saw_s: includes
	$(MAKE) one-lib-c \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BSAFEFLAGS)" \
                OBJDIR_USR=saw_s OBJDIR_SYS=saw_u \
                LIBSUFFIX=_saw_s

lib_u: lib-c_u
lib-c_u: includes
	$(MAKE) one-lib-c \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BUNSAFEFLAGS)" \
                OBJDIR_USR=obj_u OBJDIR_SYS=obj_u \
                LIBSUFFIX=_u

lib-saw_u: includes
	$(MAKE) one-lib-c \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BUNSAFEFLAGS)" \
                OBJDIR_USR=saw_u OBJDIR_SYS=saw_u \
                LIBSUFFIX=_saw_u

lib_p: lib-c_p
lib-c_p: includes
	$(MAKE) one-lib-c \
                BFLAGS_SYS="$(BUNSAFEPFLAGS)" \
                BFLAGS_USR="$(BUNSAFEPFLAGS)" \
                OBJDIR_USR=obj_p OBJDIR_SYS=obj_p \
                LIBSUFFIX=_p

one-lib-c: $(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)-$(RELEASE).a
	$(MAKE) one-share-lib-c LIBSUFFIXTHREAD=
	$(MAKE) one-share-lib-c LIBSUFFIXTHREAD=_mt

$(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)-$(RELEASE).a: $(OBJECTS) $(GCSTDOBJECTS)
	@ $(RM) -f $(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)-$(RELEASE).a
	$(AR) $(ARFLAGS) \
                $(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)-$(RELEASE).a $^
	$(RANLIB) $(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)-$(RELEASE).a
	@ echo "\e[1;36m$(@)\e[0m done."

one-share-lib-c: $(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)$(LIBSUFFIXTHREAD)-$(RELEASE).$(SHAREDSUFFIX)

$(BOOTLIBDIR)/lib$(LIBRARYNAME)$(LIBSUFFIX)$(LIBSUFFIXTHREAD)-$(RELEASE).$(SHAREDSUFFIX): $(OBJECTS) $(GCSTDOBJECTS)
	$(MAKE) bigloo-shared-lib \
             LIBDEST=$@ \
             LIBPATH=$(BOOTLIBDIR) \
             LDOPTS="$(LDOPTS) $(LDBUILDOPTS)" \
             BIGLOOLIB= \
             SONAME=lib$(LIBRARYNAME)$(LIBSUFFIX)$(LIBSUFFIXTHREAD)-$(RELEASE).$(SHAREDSUFFIX)
	@ echo "\e[1;37m$(@)\e[0m done."

#*---------------------------------------------------------------------*/
#*    Bigloo JVM libraries compilation                                 */
#*---------------------------------------------------------------------*/
.PHONY: lib-jvm lib-jvm_s lib-jvm_u lib-jvm_p libs-jvm

libs-jvm: .jfile
	@ $(MAKE) lib-jvm_u
	@ $(MAKE) lib-jvm_s

lib-jvm: lib-jvm_s
lib-jvm_s: .jfile includes
	$(MAKE) one-lib-jvm \
                BFLAGS_SYS="$(BUNSAFEFLAGS) $(BJSAFEFLAGS)" \
                BFLAGS_USR="$(BSAFEFLAGS) $(BJSAFEFLAGS)" \
                CLASSDIR_USR=class_s CLASSDIR_SYS=class_s \
                LIBSUFFIX=_s


lib-jvm_u: .jfile includes
	$(MAKE) one-lib-jvm \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BUNSAFEFLAGS)" \
                CLASSDIR_USR=class_u CLASSDIR_SYS=class_u \
                LIBSUFFIX=_u

lib-jvm_p: .jfile includes
	$(MAKE) one-lib-jvm \
                BFLAGS_SYS="$(BUNSAFEPFLAGS)" \
                BFLAGS_USR="$(BUNSAFEPFLAGS)" \
                CLASSDIR_USR=class_p CLASSDIR_SYS=class_p \
                LIBSUFFIX=_p

one-lib-jvm: $(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).zip

$(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).zip: $(CLASSES) class$(LIBSUFFIX)/jlib Jlib/configure.java
	@$(RM) -f $(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).zip
	@(cd objs/class$(LIBSUFFIX) && \
	 $(ZIP) -q $(ZFLAGS) $(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).zip \
                 $(SCM_SYS_SRC:%.scm=bigloo/runtime/%.class) \
                 $(SCM_USR_SRC:%.scm=bigloo/runtime/%.class) \
                 bigloo/runtime/*/_KBGl_*.class \
                 $(JAVA_JLIB:Jlib/%.java=bigloo/%.class) \
                 $(JAVA_JLIB:Jlib/%.java=bigloo/%[$$]*.class)) || exit 1
	@ echo "\e[1;38m$(@)\e[0m done."

#*---------------------------------------------------------------------*/
#*    Bigloo JAS libraries compilation                                 */
#*---------------------------------------------------------------------*/
.PHONY: jlib

class_s/jlib: $(CLASSES)
	(cd Jlib && \
         $(JAVAC) -classpath ../objs/class_s -d ../objs/class_s \
                  $(JCFLAGS) $(JLIB_SRC))

class_u/jlib: $(CLASSES)
	(cd Jlib && \
         $(JAVAC) -classpath ../objs/class_u -d ../objs/class_u \
                  $(JCFLAGS) $(JLIB_SRC))

class_p/jlib: $(CLASSES)
	(cd Jlib && \
         $(JAVAC) -classpath ../objs/class_p -d ../objs/class_p \
                  $(JCFLAGS) $(JLIB_SRC))

#*---------------------------------------------------------------------*/
#*    Bigloo WASM libraries compilation                                */
#*---------------------------------------------------------------------*/
.PHONY: lib-wasm lib-wasm_s lib-wasm_u libs-wasm

libs-wasm:
	@ $(MAKE) lib-wasm_u
	@ $(MAKE) lib-wasm_s

lib-wasm: lib-wasm_s
lib-wasm_s: includes
	$(MAKE) one-lib-wasm \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BSAFEFLAGS)" \
                WASMDIR_SYS=wasm_u WASMDIR_USR=wasm_s \
                LIBSUFFIX=_s

lib-wasm_u: includes
	$(MAKE) one-lib-wasm \
                BFLAGS_SYS="$(BUNSAFEFLAGS)" \
                BFLAGS_USR="$(BUNSAFEFLAGS)" \
                WASMDIR_SYS=wasm_u WASMDIR_USR=wasm_u \
                LIBSUFFIX=_u

one-lib-wasm: $(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).wat

WASM_OPT ?= wasm-opt
# WASM_OPT_FLAGS=-O3 --gufa -O3 -g
WASM_OPT_FLAGS=-O0 -g
WASM_AS ?= wasm-as
WASM_AS_FLAGS=-all -g
WASM_MERGE=wasm-merge
WASM_MERGE_FLAGS=-all -g --rename-export-conflicts

.PHONY: touch-wasm
touch-wasm:
	@ touch $(WASMS:objs//%.wasm=%.scm)
	@ touch Wlib/runtime.wat.in

$(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).wasm: \
  $(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).wat
	$(WASM_OPT) -all $(WASM_OPT_FLAGS) $< -o $@

$(BOOTLIBDIR)/$(LIBRARYNAME)$(LIBSUFFIX).wat: \
  objs/wasm$(LIBSUFFIX)/$(LIBRARYNAME)$(LIBSUFFIX).wat \
  objs/wasm$(LIBSUFFIX)/dummy_runtime.wat \
  $(WASMTYPES) $(W_WLIB) $(WASMS:%.wasm=%.wat)
	$(BIGLOO) -wasm -y -w $(WASMTYPES) \
		objs/wasm$(LIBSUFFIX)/$(LIBRARYNAME)$(LIBSUFFIX).wat \
		objs/wasm$(LIBSUFFIX)/dummy_runtime.wat \
		-o $@
	@ echo "\e[1;33m$(@)\e[0m done"

objs/wasm$(LIBSUFFIX)/$(LIBRARYNAME)$(LIBSUFFIX).wat: \
  $(WASMTYPES) $(W_WLIB) $(WASMS:%.wasm=%.wat)
	$(BIGLOO) -wasm -y -w $^ -o $@
	@ echo "\e[1;33m$(@)\e[0m done"

objs/wasm_u/%.wat: %.scm $(BOOTLIBDIR)/types.wat
	$(BIGLOO) -wasm $(WFLAGS) $(BFLAGS_SYS) $(EFLAGS) -c $< -o $@

objs/wasm_s/%.wat: %.scm $(BOOTLIBDIR)/types.wat
	$(BIGLOO) -wasm $(WFLAGS) $(BFLAGS_USR) $(EFLAGS) -c $< -o $@

objs/wasm$(LIBSUFFIX)/dummy_runtime.wat: \
  objs/wasm$(LIBSUFFIX)/$(LIBRARYNAME)$(LIBSUFFIX).wat
	$(BIGLOO) -i ../tools/gen_dummy_wasm_imports.scm $(WASMTYPES) $< > $@ || (rm -f $@; exit 1)

$(BOOTLIBDIR)/types.wat: $(WASMTYPES)
	cp $< $@

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=runtime/%)

#*---------------------------------------------------------------------*/
#*    touchall                                                         */
#*---------------------------------------------------------------------*/
touchall:
	touch $(POPULATION_SI)

#*---------------------------------------------------------------------*/
#*    Soure file distributions                                         */
#*---------------------------------------------------------------------*/
distlibs:
	@ $(MAKE) distlib_u
	@ $(MAKE) distlib

distlib_u:
	$(MAKE) one-distlib \
		BIGLOO=$(BIGLOO) \
                BFEATUREFLAGS= \
                BFLAGS_SYS="$(BUNSAFEFLAGS) $(BDISTRIBFLAGS)" \
                BFLAGS_USR="$(BUNSAFEFLAGS) $(BDISTRIBFLAGS)" \
                OBJDIR_USR=obj_u OBJDIR_SYS=obj_u


distlib:
	$(MAKE) one-distlib \
		BIGLOO=$(BIGLOO) \
                BFEATUREFLAGS= \
                BFLAGS_SYS="$(BUNSAFEFLAGS) $(BDISTRIBFLAGS)" \
                BFLAGS_USR="$(BSAFEFLAGS) $(BDISTRIBFLAGS)" \
                OBJDIR_USR=obj_s OBJDIR_SYS=obj_u

one-distlib: $(C_OBJECTS)

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*    -------------------------------------------------------------    */
#*    Boot the library on a bare system.                               */
#*---------------------------------------------------------------------*/
boot: dirs includes
	touch .afile
	@ $(MAKE) boot-libs CEFLAGS=-DBGL_BOOT=1 EFLAGS="-srfi boot"

boot-libs:
	@ $(MAKE) lib-c_u SCMOBJSUFFIX="" COBJSUFFIX=%.o
	@ $(MAKE) lib-c_s SCMOBJSUFFIX="" COBJSUFFIX=%.o

boot-jvm:
	@ $(MAKE) heap-jvm
	@ $(MAKE) lib-jvm_s
	@ $(MAKE) lib-jvm_u

boot-wasm:
	@ $(MAKE) heap-wasm
	@ $(MAKE) lib-wasm_s
	@ $(MAKE) lib-wasm_u

#*---------------------------------------------------------------------*/
#*    hostboot                                                         */
#*    -------------------------------------------------------------    */
#*    Boot a new Bigloo using the Scheme files (instead of the C       */
#*    pre-compiled files).                                             */
#*---------------------------------------------------------------------*/
hostboot:
	$(RM) -rf objs
	@ $(MAKE) .afile
	@ $(MAKE) objs includes dirs
	@ $(MAKE) lib-c_s
	@ $(MAKE) heap-c

#*---------------------------------------------------------------------*/
#*    compile-bee                                                      */
#*---------------------------------------------------------------------*/
compile-bee: lib_p

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
.PHONY: install install-bee install-c install-jvm install-wasm \
        uninstall uninstall-bee uninstall-jvm uninstall-wasm

install:
	@ $(MAKE) install-c
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) install-jvm; \
          fi
	@ if [ "$(WASMBACKEND)" = "yes" ]; then \
             $(MAKE) install-wasm; \
          fi

install-c:
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
           for i in $(INCLUDE_FILE:Include/%=%) bigloo_gc.h bigloo_config.h; do \
	      cp $(BOOTLIBDIR)/$$i $(DESTDIR)$(LIBDIR)/$(FILDIR)/$$i && \
              chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/$$i; \
           done; \
	   cp $(BOOTLIBDIR)/bigloo.heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.heap && \
           chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.heap; \
	fi
	$(MAKE) install-lib LIB=libbigloo_s-$(RELEASE)
	$(MAKE) install-lib LIB=libbigloo_u-$(RELEASE)
	if [ "$(GCTHREADSUPPORT) " = "yes " ]; then \
	  $(MAKE) install-lib LIB=libbigloo_s_mt-$(RELEASE); \
	  $(MAKE) install-lib LIB=libbigloo_u_mt-$(RELEASE); \
        fi

install-saw-c:
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
           for i in $(INCLUDE_FILE:Include/%=%) bigloo_gc.h bigloo_config.h; do \
	      cp $(BOOTLIBDIR)/$$i $(DESTDIR)$(LIBDIR)/$(FILDIR)/$$i && \
              chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/$$i; \
           done; \
	   cp $(BOOTLIBDIR)/bigloo.heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.heap && \
           chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.heap; \
	fi
	$(MAKE) install-lib LIB=libbigloo_saw_s-$(RELEASE)
	$(MAKE) install-lib LIB=libbigloo_saw_u-$(RELEASE)

install-jvm:
	if [ $(ZIPDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/bigloo.jheap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.jheap && \
             chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.jheap && \
	  cp $(BOOTLIBDIR)/bigloo_s.zip $(ZIPDIR)/bigloo_s.zip && \
             chmod $(MODFILE) $(ZIPDIR)/bigloo_s.zip && \
	  cp $(BOOTLIBDIR)/bigloo_u.zip $(ZIPDIR)/bigloo_u.zip && \
             chmod $(MODFILE) $(ZIPDIR)/bigloo_u.zip; \
        fi

install-wasm:
	cp $(WASMTYPES) $(DESTDIR)$(LIBDIR)/$(FILDIR)/types.wat
	cp Wlib/runtime.mjs $(DESTDIR)$(LIBDIR)/$(FILDIR)/runtime.mjs
	cp $(BOOTLIBDIR)/bigloo.wheap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.wheap
	chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.wheap
	cp $(BOOTLIBDIR)/bigloo_s.wat $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_s.wat
	chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_s.wat
	cp $(BOOTLIBDIR)/bigloo_u.wat $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_u.wat
	chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_u.wat

install-bee:
	$(MAKE) install-lib LIB=libbigloo_p-$(RELEASE)

uninstall: uninstall-bee uninstall-jvm uninstall-wasm
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
          for i in $(INCLUDE_FILE:Include/%=%) bigloo_gc.h bigloo_config.h; do \
             $(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/$$i; \
          done; \
          $(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.heap && \
	  $(MAKE) uninstall-lib LIB=libbigloo_s-$(RELEASE) && \
	  $(MAKE) uninstall-lib LIB=libbigloo_u-$(RELEASE) && \
	  $(MAKE) uninstall-lib LIB=libbigloogc-$(RELEASE) && \
          $(RM) -rf $(DESTDIR)$(LIBDIR)/$(FILDIR); \
	  if [ "$(GCTHREADSUPPORT) " = "yes " ]; then \
	    $(MAKE) uninstall-lib LIB=libbigloo_s_mt-$(RELEASE); \
	    $(MAKE) uninstall-lib LIB=libbigloo_u_mt-$(RELEASE); \
          fi; \
        fi

uninstall-jvm:
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.jheap
	-$(RM) -f $(ZIPDIR)/bigloo_s.zip
	-$(RM) -f $(ZIPDIR)/bigloo_u.zip

uninstall-wasm:
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/types.wat
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/runtime.mjs
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo.wheap
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_s.wat
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_u.wat

uninstall-bee:
	- $(MAKE) uninstall-lib LIB=libbigloo-$(RELEASE)_p

#*---------------------------------------------------------------------*/
#*    cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean-c clean-jvm clean cleanall distclean

clean-quick:
	$(MAKE) cleanobj OBJDIR_USR=obj_s OBJDIR_SYS=obj_u
	$(MAKE) cleanobj OBJDIR_USR=obj_u OBJDIR_SYS=obj_u
	$(MAKE) cleanobj OBJDIR_USR=obj_p OBJDIR_SYS=obj_p

clean-c:
	$(RM) -f $(BOOTLIBDIR)/bigloo.h
	$(RM) -f $(BOOTLIBDIR)/bigloo_gc.h
	$(RM) -f $(BOOTLIBDIR)/bigloo_config.h
	$(MAKE) clean-quick
	$(RM) -f $(BOOTLIBDIR)/$(HEAPC_FILE)
	$(RM) -f $(BOOTLIBDIR)/lib$(LIBRARYNAME)*.*

clean-jvm:
	$(RM) -f $(BOOTLIBDIR)/$(HEAPJVM_FILE)
	$(RM) -rf objs/class*/bigloo/*.class
	$(RM) -rf objs/class*/bigloo/runtime/*/*.class
	$(MAKE) cleanclass CLASSDIR_USR=class_s CLASSDIR_SYS=class_u
	$(MAKE) cleanclass CLASSDIR_USR=class_u CLASSDIR_SYS=class_u
	$(MAKE) cleanclass CLASSDIR_USR=class_p CLASSDIR_SYS=class_p
	$(RM) -f $(BOOTLIBDIR)/bigloo_u.zip
	$(RM) -f $(BOOTLIBDIR)/bigloo_s.zip

cleanobj:
	$(RM) -f $(OBJECTS)

cleanclass:
	$(RM) -f $(CLASSES) $(JAS)

clean: clean-c clean-jvm

cleanall: clean-c clean
	@ if [ "`pwd` " = "$$HOME/prgm/project/bigloo/runtime " ]; then \
             echo "*** ERROR:Illegal dir to make a cleanall `pwd`"; \
             exit 1; \
          fi

distclean: cleanall
	$(RM) -rf objs/class*
	$(RM) -f Jlib/configure.java
	$(MAKE) dirs

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
objs/obj_u/$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_SYS) $(EFLAGS) -c $< -o $@

objs/obj_s/$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_USR) $(EFLAGS) -c $< -o $@

objs/obj_p/$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_USR) $(EFLAGS) -c $< -o $@

objs/saw_u/$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_SYS) $(SAWFLAGS) $(EFLAGS) -c $< -o $@

objs/saw_s/$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_USR) $(SAWFLAGS) $(EFLAGS) -c $< -o $@

objs/obj_u/Eval/evaluate_comp$(SCMOBJSUFFIX): Eval/evaluate_comp.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_SYS) $(EFLAGS) -fno-arithmetic-overflow -c $< -o $@

objs/saw_u/Eval/evaluate_comp$(SCMOBJSUFFIX): Eval/evaluate_comp.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_SYS) $(SAWFLAGS) $(EFLAGS) -fno-arithmetic-overflow -c $< -o $@

#*---------------------------------------------------------------------*/
#*     .c --> .o                                                       */
#*---------------------------------------------------------------------*/
objs/obj_u/$(COBJSUFFIX): objs/obj_u/%.c
	$(CC) $(CEFLAGS) $(CUNSAFEFLAGS) $(GCINCLUDE) -c -o $(*D)-$(@F) $< && \
        mv $(*D)-$(@F) $@

objs/obj_s/$(COBJSUFFIX): objs/obj_s/%.c
	$(CC) $(CEFLAGS) $(CSAFEFLAGS) $(GCINCLUDE) -c -o $(*D)-$(@F) $< && \
        mv $(*D)-$(@F) $@

objs/saw_u/$(COBJSUFFIX): objs/saw_u/%.c
	$(CC) $(CSAWFLAGS) $(CEFLAGS) $(CUNSAFEFLAGS) $(GCINCLUDE) -c -o $(*D)-$(@F) $< && \
        mv $(*D)-$(@F) $@

objs/saw_s/$(COBJSUFFIX): objs/saw_s/%.c
	$(CC) $(CSAWFLAGS) $(CEFLAGS) $(CUNSAFEFLAGS) $(GCINCLUDE) -c -o $(*D)-$(@F) $< && \
        mv $(*D)-$(@F) $@

#*---------------------------------------------------------------------*/
#*     .c --> .o                                                       */
#*---------------------------------------------------------------------*/
objs/obj_u/Clib/%.o: Clib/%.c
	$(CC) $(CEFLAGS) $(CUNSAFEFLAGS) $(GCINCLUDE) -c Clib/$*.c -o Clib-$(@F) && \
        mv Clib-$(@F) $@
objs/obj_s/Clib/%.o: Clib/%.c
	$(CC) $(CEFLAGS) $(CSAFEFLAGS) $(GCINCLUDE) -c Clib/$*.c -o Clib-$(@F) && \
        mv Clib-$(@F) $@

objs/obj_p/Clib/%.o: Clib/%.c
	$(CC) $(CEFLAGS) $(CUNSAFEPFLAGS) $(GCINCLUDE) -c Clib/$*.c -o Clib-$(@F) && \
        mv Clib-$(@F) $@

objs/saw_u/Clib/%.o: Clib/%.c
	$(CC) $(CSAWFLAGS) $(CEFLAGS) $(CUNSAFEFLAGS) $(GCINCLUDE) -c Clib/$*.c -o Clib-$(@F) && \
        mv Clib-$(@F) $@
objs/saw_s/Clib/%.o: Clib/%.c
	$(CC) $(CSAWFLAGS) $(CEFLAGS) $(CSAFEFLAGS) $(GCINCLUDE) -c Clib/$*.c -o Clib-$(@F) && \
        mv Clib-$(@F) $@

objs/obj_u/Eval/evprimop.o: objs/obj_u/Eval/evprimop.c
	$(CC) -I$(BOOTLIBDIR) $(CUNSAFEFLAGS) $(CEFLAGS) -c $< -o $@

objs/saw_u/Eval/evprimop.o: objs/saw_u/Eval/evprimop.c
	$(CC) -I$(BOOTLIBDIR) $(CUNSAFEFLAGS) $(CSAWFLAGS) $(CEFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*     .scm --> .c                                                     */
#*---------------------------------------------------------------------*/
objs/obj_u/%.c: %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_SYS) $(EFLAGS) -c $< -indent -o $@

objs/obj_s/%.c: %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_USR) $(EFLAGS) -c $< -indent -o $@

objs/obj_u/Unsafe/bignumber.c: Unsafe/bignumber.scm
	$(BIGLOO) $(BFLAGS_USR) $(EFLAGS) -c $< -indent -o $@

objs/obj_u/Eval/evprimop.c: Eval/evprimop.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BUNSAFEFLAGS) $(EFLAGS) -cgen $< -o $@

objs/saw_u/%.c: %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_SYS) $(SAWFLAGS) $(EFLAGS) -c $< -indent -o $@

objs/saw_s/%.c: %.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BFLAGS_USR) $(SAWFLAGS) $(EFLAGS) -c $< -indent -o $@

objs/saw_u/Unsafe/bignumber.c: Unsafe/bignumber.scm
	$(BIGLOO) $(BFLAGS_USR) $(SAWFLAGS) $(EFLAGS) -c $< -indent -o $@

objs/saw_u/Eval/evprimop.c: Eval/evprimop.scm
	$(BIGLOO) $(BFEATUREFLAGS) $(BUNSAFEFLAGS) $(SAWFLAGS) $(EFLAGS) -cgen $< -o $@

objs/obj_u/Eval/evaluate_comp.c: Eval/evaluate_comp.scm
	$(BIGLOO) $(BFLAGS_USR) $(EFLAGS) -c $< -indent -fno-arithmetic-overflow -o $@

#*---------------------------------------------------------------------*/
#*    mach_dep.o                                                       */
#*---------------------------------------------------------------------*/
objs/$(OBJDIR_SYS)/Mlib/mach_dep.o: $(NATIVE_MACHDEP)

sparc_sunos5_mach_dep: Mlib/sparc_sunos5_mach_dep.s
	$(AS) -o objs/$(OBJDIR_SYS)/Mlib/mach_dep.o Mlib/sparc_sunos5_mach_dep.s

sparc_sunos4_mach_dep: Mlib/sparc_sunos4_mach_dep.s
	$(AS) -o objs/$(OBJDIR_SYS)/Mlib/mach_dep.o Mlib/sparc_sunos4_mach_dep.s

generic_mach_dep: Mlib/mach_dep.c
	$(CC) $(CUNSAFEFLAGS) $(CEFLAGS) -c -o mach_dep.o $< && \
        mv mach_dep.o objs/$(OBJDIR_SYS)/Mlib/mach_dep.o

#*---------------------------------------------------------------------*/
#*     .scm --> .class                                                 */
#*---------------------------------------------------------------------*/
objs/class_u/bigloo/runtime/$(SCMCLASSSUFFIX): %.scm
	$(BIGLOO) -jvm $(BGLJFLAGS) $(BFLAGS_SYS) $(EFLAGS) -c $< -o $@

objs/class_s/bigloo/runtime/$(SCMCLASSSUFFIX): %.scm
	$(BIGLOO) -jvm $(BGLJFLAGS) $(BFLAGS_USR) $(EFLAGS) -c $< -o $@

objs/class_p/bigloo/runtime/$(SCMCLASSSUFFIX): %.scm
	$(BIGLOO) -jvm $(BGLJFLAGS) $(BFLAGS_USR) $(EFLAGS) -c $< -o $@

objs/class_s/bigloo/runtime/Eval/evaluate_comp.class: Eval/evaluate_comp.scm
	$(BIGLOO) -jvm $(BGLJFLAGS) $(BFLAGS_SYS) $(EFLAGS) -fno-arithmetic-overflow -c $< -o $@

objs/class_u/bigloo/runtime/Eval/evaluate_comp.class: Eval/evaluate_comp.scm
	$(BIGLOO) -jvm $(BGLJFLAGS) $(BFLAGS_SYS) $(EFLAGS) -fno-arithmetic-overflow -c $< -o $@


#*---------------------------------------------------------------------*/
#*    objs                                                             */
#*    -------------------------------------------------------------    */
#*    This entry point creates all the directory where the object      */
#*    files and the Jvm classes are stored.                            */
#*---------------------------------------------------------------------*/
.PHONY: objs objs-sans-dirs dirs

objs-sans-dirs: Jlib/configure.java
	mkdir -p objs/obj_s/Clib
	mkdir -p objs/obj_s/Ieee
	mkdir -p objs/obj_s/Llib
	mkdir -p objs/obj_s/R5rs
	(dirs="Clib Ieee Llib Mlib Ilib R5rs Rgc Eval Lalr Match Pp Read Unsafe"; \
         objs="obj_u obj_p saw_u saw_s wasm_s wasm_u"; \
         for o in $$objs; do \
           mkdir -p objs/$$o; \
           for d in $$dirs; do \
             mkdir -p objs/$$o/$$d; \
             touch objs/$$o/$$d/.keep; \
           done; \
         done)
	mkdir -p objs/class_s
	mkdir -p objs/class_s/bigloo
	mkdir -p objs/class_s/bigloo/runtime
	mkdir -p objs/class_u
	mkdir -p objs/class_u/bigloo
	mkdir -p objs/class_u/bigloo/runtime

dirs:
	(dirs="Jlib Ieee Llib Mlib R5rs Rgc Eval Lalr Match Pp Read Unsafe"; \
         for o in class_u class_s class_p; do \
           echo objs/$$o/bigloo/runtime; \
           mkdir -p objs/$$o/bigloo/runtime; \
           for d in $$dirs; do \
             echo objs/$$o/bigloo/runtime/$$d; \
             mkdir -p objs/$$o/bigloo/runtime/$$d; \
           done; \
         done; \
         dirs="Clib Ilib Ieee Llib Mlib R5rs Rgc Eval Lalr Match Pp Read Unsafe"; \
         for o in obj_u saw_s saw_u wasm_s wasm_u; do \
           echo objs/$$o; \
           mkdir -p objs/$$o; \
           for d in $$dirs; do \
             echo objs/$$o/$$d; \
             mkdir -p objs/$$o/$$d; \
           done; \
         done; \
         dirs="Clib Ieee Llib R5rs"; \
         for o in obj_s obj_p saw_s saw_u wasm_s wasm_u; do \
           echo objs/$$o; \
           mkdir -p objs/$$o; \
           for d in $$dirs; do \
             echo objs/$$o/$$d; \
             mkdir -p objs/$$o/$$d; \
           done; \
         done)

objs: objs-sans-dirs dirs

#*---------------------------------------------------------------------*/
#*    $(BOOTLIBDIR)/bigloo.h                                           */
#*---------------------------------------------------------------------*/
$(BOOTLIBDIR)/bigloo.h: Include/bigloo.h
	@ if [ "$(DISTRIB) " != "true " ]; then \
	    echo "*** ERROR: $@ is out of date (older than $?)"; \
	    echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""; \
	    exit 1; \
          fi


#*---------------------------------------------------------------------*/
#*    $(BOOTLIBDIR)/bigloo_config.h                                    */
#*---------------------------------------------------------------------*/
$(BOOTLIBDIR)/bigloo_config.h: $(BOOTDIR)/configure $(BOOTDIR)/Makefile
	@ if [ "$(DISTRIB) " != "true " ]; then \
	    echo "*** ERROR: $@ is out of date (older than $?)"; \
	    echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""; \
	    exit 1; \
          fi

#*---------------------------------------------------------------------*/
#*    Jlib/configure.java                                              */
#*---------------------------------------------------------------------*/
Jlib/configure.java: $(BOOTDIR)/configure $(BOOTDIR)/Makefile
	@ if [ "$(DISTRIB) " != "true " ]; then \
	    echo "*** ERRORRR: $@ is out of date (older than $?)"; \
	    echo "You need to re-configure \"(cd ..; ./configure --bootconfig)\""; \
	    exit 1; \
          fi

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude

ude: .afile .jfile

#*---------------------------------------------------------------------*/
#*    .afile                                                           */
#*---------------------------------------------------------------------*/
afile: .afile
.afile:
	$(BGLAFILE) -o .afile $(SCM_USR_SRC) $(SCM_SYS_SRC)

#*---------------------------------------------------------------------*/
#*    .jfile                                                           */
#*---------------------------------------------------------------------*/
jfile: .jfile
.jfile:
	$(BGLJFILE) -pbase $(PBASE) $(SCM_USR_SRC) $(SCM_SYS_SRC) > .jfile

#*---------------------------------------------------------------------*/
#*    wasm dependencies                                                */
#*---------------------------------------------------------------------*/
objs/wasm_s/Ieee/fixnum.wat: Ieee/fixnum-generic.sch
objs/wasm_u/Ieee/fixnum.wat: Ieee/fixnum-generic.sch
objs/wasm_s/Ieee/output.wat: Ieee/output-generic.sch
objs/wasm_u/Ieee/output.wat: Ieee/output-generic.sch
objs/wasm_s/Ieee/string.wat: Ieee/string-generic.sch
objs/wasm_u/Ieee/string.wat: Ieee/string-generic.sch
objs/wasm_s/Ieee/vector.wat: Ieee/vector-generic.sch
objs/wasm_u/Ieee/vector.wat: Ieee/vector-generic.sch
objs/wasm_s/Ieee/symbol.wat: Ieee/symbol-generic.sch
objs/wasm_u/Ieee/symbol.wat: Ieee/symbol-generic.sch
objs/wasm_s/Unsafe/bignumber.wat: Unsafe/bignumber-gmp.sch
objs/wasm_u/Unsafe/bignumber.wat: Unsafe/bignumber-gmp.sch
objs/wasm_s/Rgc/rgc.wat: Rgce/rgc-generic.sch
objs/wasm_u/Rgc/rgc.wat: Rgc/rgc-generic.sch
objs/wasm_u/Llib/unicode.wat: Llib/unicode-generic.sch

Wlib/wfixnum.wat: Wlib/wfixnum$(WASM_FIXNUM).wat.in $(BOOTLIBDIR)/bigloo_config.h
	preallocs=`$(WASM_BINTS_PREALLOC)`; \
	cat $< \
           | sed -e "s/-[*]- eval: (bee-mode) -[*]-/!!! GENERATED FILE, DON'T EDIT !!!/" \
           | sed -e "s/@BINTS_PREALLOC_SIZE@/$(WASM_BINTS_PREALLOC_SIZE)/g" \
           | sed -e "s/@BINTS_PREALLOC_START@/$(WASM_BINTS_PREALLOC_START)/g" \
           | sed -e "s/@BINTS_PREALLOC@/$$preallocs/" \
           | sed -e "s/@DOLLAR@/$$/g" \
           > $@

Wlib/wcnst.wat: Wlib/wcnst$(WASM_FIXNUM).wat.in $(BOOTLIBDIR)/bigloo_config.h
	cat $< \
           | sed -e "s/-[*]- eval: (bee-mode) -[*]-/!!! GENERATED FILE, DON'T EDIT !!!/" \
           > $@

Wlib/types.wat: Wlib/types.wat.in
	$(BIGLOO) -i ../tools/gen_wasm_types.scm $< > $@ || (rm -f $@; exit 1)

Wlib/types.wat: Wlib/wports.wat Wlib/wstring.wat Wlib/wfixnum.wat \
  Wlib/wintegers.wat Wlib/wbignum.wat Wlib/wcnst.wat Wlib/wdate.wat \
  Wlib/wvector.wat Wlib/wmmap.wat Wlib/wdenv.wat Wlib/wprocedure.wat \
  Wlib/wbinary.wat

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
objs/obj_s/Llib/bconfigure.o: $(BOOTLIBDIR)/bigloo_config.h
objs/obj_u/Llib/bconfigure.o: $(BOOTLIBDIR)/bigloo_config.h
objs/obj_s/Llib/evobject.o: Llib/object.sch
objs/obj_u/Llib/evobject.o: Llib/object.sch
objs/obj_u/Unsafe/regexp.o: Unsafe/pcre.scm Unsafe/pregexp.scm Unsafe/regexp.scm
objs/obj_s/Unsafe/regexp.o: Unsafe/pcre.scm Unsafe/pregexp.scm Unsafe/regexp.scm
objs/saw_s/Llib/bconfigure.o: $(BOOTLIBDIR)/bigloo_config.h
objs/saw_u/Llib/bconfigure.o: $(BOOTLIBDIR)/bigloo_config.h
objs/saw_s/Llib/evobject.o: Llib/object.sch
objs/saw_u/Llib/evobject.o: Llib/object.sch
objs/obj_s/Clib/cregexp.o: Clib/cpcre.h Clib/cpcre2.h Clib/cregex.h
objs/obj_u/Clib/cregexp.o: Clib/cpcre.h Clib/cpcre2.h Clib/cregex.h
