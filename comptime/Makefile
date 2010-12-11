#*=====================================================================*/
#*    serrano/prgm/project/bigloo/comptime/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 08:29:03 1998                          */
#*    Last change :  Sat Dec 11 08:20:33 2010 (serrano)                */
#*    Copyright   :  1998-2010 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the compiler part of Bigloo (bootstrap).   */
#*    This Makefile has to be processed by Gnu make.                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
BIGLOO		= $(BGLBUILDBIGLOO)
# the produced executable
BIGLOO_DEST	= $(BOOTBINDIR)/bigloo
# the .afile
AFILE           = .afile
# the .jfile
JFILE           = .jfile
# the .etags
ETAGS           = .etags
# the shell to be used
SHELL           = /bin/sh
# another produced executable
NEW_SUFFIX	= new
# another target.
DEFAULT		= plain
# These variables control how we are booting (from Scheme or C)
SCMOBJSUFFIX	= %.o
SCMCLASSSUFFIX	= %.class
DOTNETOBJSUFFIX	= %.obj
COBJSUFFIX	= 
CSRCSUFFIX	= 

#*---------------------------------------------------------------------*/
#*    Bigloo compilation flags                                         */
#*---------------------------------------------------------------------*/
LIBDIRFLAGS	= -lib-dir $(BOOTLIBDIR)
# Commmon flags
COMMONFLAGS	= -q \
                  -afile $(AFILE) \
                  -fsharing \
                  -fno-reflection \
                  -cc $(CC) \
                  $(LIBDIRFLAGS) \
                  -unsafev \
                  -gno-error-localization \
                  -no-hello
# Optimized _and_ debugged compilation
OGBFLAGS        = $(BOOTFLAGS) $(COMMONFLAGS) -Wall -O2 +rm -g
# Bootstrap flag
BFLAGS		= $(OGBFLAGS)       
EFLAGS		=
# Distribution flag
BDISTFLAGS	= $(COMMONFLAGS) -O2 -rm -unsafe -s
# Distribution Jvm flag
BJVMFLAGS	= $(BDISTFLAGS) -lib-dir $(BOOTLIBDIR)
# Distribution Dotnet flag
BDOTNETFLAGS	= $(BDISTFLAGS) -lib-dir $(BOOTLIBDIR)
# Link flag
LDBOOTFLAGS     = -static-bigloo $(LDSTRIPFLAGS)
# C compiler flag
CADDFLAGS	= -I$(BOOTLIBDIR)

#*---------------------------------------------------------------------*/
#*    Afile, Jfile and Bdepend tools                                   */
#*---------------------------------------------------------------------*/
AFILE_BIN	= $(BGLBUILDBINDIR)/$(AFILE_EXE)
JFILE_BIN	= $(BGLBUILDBINDIR)/$(JFILE_EXE)
BTAGS_BIN	= $(BGLBUILDBINDIR)/$(BTAGS_EXE)
DEPEND_BIN	= $(BGLBUILDBINDIR)/$(BDEPEND_EXE)

#*---------------------------------------------------------------------*/
#*    License                                                          */
#*---------------------------------------------------------------------*/
LICENSE		= ../LICENSE

#*---------------------------------------------------------------------*/
#*    The compiler source files ...                                    */
#*---------------------------------------------------------------------*/
#*--- init ------------------------------------------------------------*/
INIT_FILE_NOMAIN= parse_args setrc extend lib_dir
INIT_FILE	= $(INIT_FILE_NOMAIN) main

INIT_OBJ_NOMAIN	= $(INIT_FILE_NOMAIN:%=Init/%.o)
INIT_OBJ	= $(INIT_FILE:%=Init/%.o)

#*--- tools -----------------------------------------------------------*/
TOOLS_FILE	= args location progn trace speek error shape misc date \
                  dsssl license

TOOLS_OBJ	= $(TOOLS_FILE:%=Tools/%.o)

#*--- engine ----------------------------------------------------------*/
ENGINE_FILE	= compiler engine param interp link signals pass \
                  configure heap

ENGINE_OBJ	= $(ENGINE_FILE:%=Engine/%.o)

#*--- heap ------------------------------------------------------------*/
HEAP_FILE	= restore make

HEAP_OBJ	= $(HEAP_FILE:%=Heap/%.o)

#*--- write -----------------------------------------------------------*/
WRITE_FILE	= version expanded scheme ast

WRITE_OBJ	= $(WRITE_FILE:%=Write/%.o)

#*--- read ------------------------------------------------------------*/
READ_FILE	= access include src inline load reader jvm

READ_OBJ	= $(READ_FILE:%=Read/%.o)

#*--- module ----------------------------------------------------------*/
MODULE_FILE	= foreign impuse main prototype with class include \
                  module statexp eval load pragma type library checksum \
                  option alibrary java

MODULE_OBJ	= $(MODULE_FILE:%=Module/%.o)

#*--- ast -------------------------------------------------------------*/
AST_FILE	= build venv glo_decl node unit dump glo_def pragma \
                  var find_gdefs ident remove local sexp labels let \
                  exit substitute alphatize occur toccur app apply lvtype \
                  hrtype shrinkify check_init private check_sharing init \

AST_OBJ		= $(AST_FILE:%=Ast/%.o)

#*--- beta ------------------------------------------------------------*/
BETA_FILE	= walk

BETA_OBJ	= $(BETA_FILE:%=Beta/%.o)

#*--- cc --------------------------------------------------------------*/
CC_FILE		= exec indent cc ld

CC_OBJ		= $(CC_FILE:%=Cc/%.o)

#*--- foreign ---------------------------------------------------------*/
FOREIGN_FILE	= access cenum cptr ctype calias cfun cstruct copaque \
                  library jtype
 
FOREIGN_OBJ	= $(FOREIGN_FILE:%=Foreign/%.o)

#*--- Type ------------------------------------------------------------*/
TYPE_FILE	= cache tenv type coercion tools typeof pptype misc

TYPE_OBJ	= $(TYPE_FILE:%=Type/%.o)

#*--- expand ----------------------------------------------------------*/
EXPAND_FILE	= eps assert case define expander garith iarith farith \
                  if initial lambda let map struct exit object \
                  mvalue

EXPAND_OBJ	= $(EXPAND_FILE:%=Expand/%.o)

#*--- inline ----------------------------------------------------------*/
INLINE_FILE	= walk inline app size simple recursion variant loop


INLINE_OBJ	= $(INLINE_FILE:%=Inline/%.o)

#*--- coerce ----------------------------------------------------------*/
COERCE_FILE	= walk pproto coerce app funcall apply convert

COERCE_OBJ	= $(COERCE_FILE:%=Coerce/%.o)

#*--- Dataflow --------------------------------------------------------*/
DATAFLOW_FILE	= walk

DATAFLOW_OBJ	= $(DATAFLOW_FILE:%=Dataflow/%.o)

#*--- globalize -------------------------------------------------------*/
GLOBALIZE_FILE	= walk ginfo escape gn node free kapture \
                  integration integration28c \
                  new_body loc2glo gloclo clocto globalize

GLOBALIZE_OBJ	= $(GLOBALIZE_FILE:%=Globalize/%.o)

#*--- cnst ------------------------------------------------------------*/
CNST_FILE	= walk cache alloc node initialize

CNST_OBJ	= $(CNST_FILE:%=Cnst/%.o)

#*--- cfa -------------------------------------------------------------*/
CFA_FILE	= app cinfo iterate set approx collect loose show walk \
                  setup vector cfa funcall procedure struct type \
                  closure tvector box specialize cinfo2 cinfo3 \
                  arithmetic ltype

CFA_OBJ		= $(CFA_FILE:%=Cfa/%.o)

#*--- Integrate -------------------------------------------------------*/
INTEGRATE_FILE	= walk definition iinfo a kk u g ctn cto kaptured \
                  let_fun loc2glo node free

INTEGRATE_OBJ	= $(INTEGRATE_FILE:%=Integrate/%.o)

#*--- Tailc -----------------------------------------------------------*/
TAILC_FILE	= walk

TAILC_OBJ	= $(TAILC_FILE:%=Tailc/%.o)

#*--- Cgen ------------------------------------------------------------*/
CGEN_FILE	= compile cop emit_cop cgen capp

CGEN_OBJ	= $(CGEN_FILE:%=Cgen/%.o)

#*--- Jas -------------------------------------------------------------*/
JAS_FILE        = lib classfile opcode peep wide labels stack profile produce as

JAS_OBJ		= $(JAS_FILE:%=Jas/%.o)

#*--- Jvm -------------------------------------------------------------*/
JVM_FILE	= extern env instr inline peephole lisp prelude compress \
                  init constr closure \
                  compiler tail effect predicate expr \
                  compile jdump jld vmemq

JVM_OBJ		= $(JVM_FILE:%=Jvm/%.o)

#*--- BackEnd ---------------------------------------------------------*/
BACKEND_FILE    = backend walk init lib cplib cvm bvm \
                  c c_main c_proto c_emit  \
                  jvm jvm_class \
                  dotnet dotnet_class

BACKEND_OBJ	= $(BACKEND_FILE:%=BackEnd/%.o)

#*--- SawMill ---------------------------------------------------------*/
SAW_FILE        = lib defs rtl node2rtl collapse remove InlineReturn \
		  blockorder gotos cast expr woodcutter \
		  procedures elsewhere regalloc regset

SAW_OBJ		= $(SAW_FILE:%=SawMill/%.o)

#*--- SawC ------------------------------------------------------------*/
SAW_C_FILE      = compile code

SAW_C_OBJ	= $(SAW_C_FILE:%=SawC/%.o)

#*--- SawMsil ---------------------------------------------------------*/
SAW_MSIL_FILE	= compile type code inline maxstack funcall names out dnld

SAW_MSIL_OBJ	= $(SAW_MSIL_FILE:%=SawMsil/%.o)

#*--- SawJvm ----------------------------------------------------------*/
SAW_JVM_FILE	= compile type code inline funcall names out jld

SAW_JVM_OBJ	= $(SAW_JVM_FILE:%=SawJvm/%.o)

#*--- Hgen ------------------------------------------------------------*/
HGEN_FILE	= walk

HGEN_OBJ	= $(HGEN_FILE:%=Hgen/%.o)

#*--- tvector ---------------------------------------------------------*/
TVECTOR_FILE	= tvector access cnst

TVECTOR_OBJ	= $(TVECTOR_FILE:%=Tvector/%.o)

#*--- trace -----------------------------------------------------------*/
TRACE_FILE	= walk

TRACE_OBJ	= $(TRACE_FILE:%=Trace/%.o)

#*--- fail ------------------------------------------------------------*/
FAIL_FILE	= walk

FAIL_OBJ	= $(FAIL_FILE:%=Fail/%.o)

#*--- callcc ----------------------------------------------------------*/
CALLCC_FILE	= walk

CALLCC_OBJ	= $(CALLCC_FILE:%=Callcc/%.o)

#*--- reduce ----------------------------------------------------------*/
REDUCE_FILE	= walk cond typec ftypec copy cse same 1occ sbeta

REDUCE_OBJ	= $(REDUCE_FILE:%=Reduce/%.o)

#*--- effect ----------------------------------------------------------*/
EFFECT_FILE	= effect walk cgraph spread feffect

EFFECT_OBJ	= $(EFFECT_FILE:%=Effect/%.o)

#*--- object ----------------------------------------------------------*/
OBJECT_FILE	= class generic method getter \
                  plain_access wide_access java_access \
                  struct slots tools coercion predicate creator nil

OBJECT_OBJ	= $(OBJECT_FILE:%=Object/%.o)

#*--- bdb -------------------------------------------------------------*/
BDB_FILE	= setting spread_obj bdb_initialize walk bdb_emit

BDB_OBJ		= $(BDB_FILE:%=Bdb/%.o)

#*--- prof ------------------------------------------------------------*/
PROF_FILE	= unit prof_emit

PROF_OBJ	= $(PROF_FILE:%=Prof/%.o)

#*--- bound -----------------------------------------------------------*/
ABOUND_FILE	= walk

ABOUND_OBJ	= $(ABOUND_FILE:%=Abound/%.o)

#*--- user ------------------------------------------------------------*/
USER_FILE	= user

USER_OBJ	= $(USER_FILE:%=User/%.o)

#*---------------------------------------------------------------------*/
#*    The object files                                                 */
#*---------------------------------------------------------------------*/
O_OBJECTS_NOIJ  = $(ENGINE_OBJ) $(WRITE_OBJ) \
                  $(READ_OBJ) $(AST_OBJ) $(BETA_OBJ) $(CC_OBJ) $(FOREIGN_OBJ) \
                  $(HEAP_OBJ) $(MODULE_OBJ) $(TYPE_OBJ) $(EXPAND_OBJ) \
                  $(INLINE_OBJ) $(BIVALUE_OBJ) $(USER_OBJ) $(HGEN_OBJ) \
                  $(COERCE_OBJ) $(DATAFLOW_OBJ) $(GLOBALIZE_OBJ) $(CNST_OBJ) \
                  $(CFA_OBJ) $(INTEGRATE_OBJ) $(TAILC_OBJ) $(CGEN_OBJ) \
                  $(TVECTOR_OBJ) $(TSTRUCT_OBJ) $(TRACE_OBJ) $(CALLCC_OBJ) \
                  $(EVAL_OBJ) $(LIFEXT_OBJ) $(FAIL_OBJ) $(REDUCE_OBJ) \
                  $(EFFECT_OBJ) $(OBJECT_OBJ) $(TOOLS_OBJ) $(BDB_OBJ) \
                  $(PROF_OBJ) $(ABOUND_OBJ) $(JAS_OBJ) $(BACKEND_OBJ) \
                  $(SAW_OBJ) $(SAW_C_OBJ) $(SAW_JVM_OBJ) $(SAW_MSIL_OBJ)

O_OBJECTS_NOINIT=$(O_OBJECTS_NOIJ)

O_OBJECTS       = $(O_OBJECTS_NOINIT) $(INIT_OBJ)
CLASS_OBJECTS	= $(O_OBJECTS:%.o=%.class)
DOTNET_OBJECTS	= $(O_OBJECTS:%.o=%.obj)

# This entry is used by QNC, for building bigloo++. Mainly Christian
# defines is own main entry point. This list does not contains the
# init/main.o file
O_OBJECTS_NOMAIN= $(O_OBJECTS_NOINIT) $(INIT_OBJ_NOMAIN)

C_OBJECTS	= $(O_OBJECTS:%.o=%.c)

SOURCE_FILES	= $(O_OBJECTS:%.o=%.scm)

POPULATION	= $(SOURCE_FILES) \
		  Ast/node.sch \
		  Ast/unit.sch \
		  Engine/pass.sch \
		  Expand/expander.sch \
		  Tools/location.sch \
		  Tools/fprint.sch \
		  Tools/verbose.sch \
		  Tools/trace.sch \
		  Tvector/tvector.sch \
		  Type/coercer.sch \
                  Init/pass-args-parse.sch \
		  Module/libinfo.sch \
                  Makefile

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:

#*---------------------------------------------------------------------*/
#*    Bootstrap (one round) the compiler                               */
#*---------------------------------------------------------------------*/
bigloo:
	$(MAKE) bigloo.$(DEFAULT)

#*---------------------------------------------------------------------*/
#*    jvm                                                              */
#*---------------------------------------------------------------------*/
jvm: $(CLASS_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(EFLAGS) -jvm -jvm-jar -jvm-java $(JAVA) \
             -jvm-shell $(JSHELL) \
             -jvm-classpath $(CLASSPATH) -jvm-jarpath $(JARPATH) \
             $(BDISTFLAGS) -o $(BIGLOO_DEST).jvm $(CLASS_OBJECTS)
	@ echo "$(BIGLOO_DEST).jvm done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    dotnet                                                           */
#*---------------------------------------------------------------------*/
dotnet: $(DOTNET_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(EFLAGS) -dotnet $(BDISTFLAGS) \
              -o $(BIGLOO_DEST)-dotnet$(SCRIPTEXTENSION) $(DOTNET_OBJECTS)
	@ echo "$(BIGLOO_DEST)-dotnet$(SCRIPTEXTENSION) done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/comptime ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	$(MAKE) $(AFILE)
	$(MAKE) $(JFILE)
	$(MAKE) dist-updates
	$(MAKE) dist-bigloo.exe CSRCSUFFIX="%.c"

#*---------------------------------------------------------------------*/
#*    bigloo.plain and bigloo.new                                      */
#*    -------------------------------------------------------------    */
#*    Because some OS does not support executable to be changed while  */
#*    running we cannot directly override the file used to compile.    */
#*    That is, if we are compiling Bigloo with a file called           */
#*    bin/bigloo we cannot directly produce a new version called       */
#*    bin/bigloo. Instead, we generate a file called bin/bigloo.plain  */
#*    and then, we rename this file into bin/bigloo.                   */
#*    -------------------------------------------------------------    */
#*    On a bootstrap, we _always_ change the compiler date. That's     */
#*    why, Tools/date.scm is erased and rebuild.                       */
#*---------------------------------------------------------------------*/
.PHONY: plain new newnew

plain: bigloo.plain
new: bigloo.new
so: bigloo.$(SHAREDSUFFIX)

bigloo.plain: Tools/license.scm
	$(MAKE) $(AFILE)
	$(MAKE) $(JFILE)
	- $(RM) -f Tools/date.scm
	$(MAKE) Tools/date.scm
	$(MAKE) $(BIGLOO_DEST)$(EXE_SUFFIX)

bigloo.new:
	$(MAKE) $(BIGLOO_DEST).$(NEW_SUFFIX)

renew:
	$(RM) -f $(BIGLOO_DEST).$(NEW_SUFFIX)
	$(MAKE) new

bigloo.$(SHAREDSUFFIX):
	$(MAKE) $(BIGLOO_DEST).$(SHAREDSUFFIX)

#*---------------------------------------------------------------------*/
#*    bigloo                                                           */
#*    -------------------------------------------------------------    */
#*    For bigloo.dest, we have to use a temporary target and then      */
#*    rename into the final target. Otherwise we may have trouble      */
#*    during the linkg if BIGLOO and BIGLOO_DEST are the same          */
#*    file.                                                            */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST)$(EXE_SUFFIX): $(AFILE) $(JFILE) $(ETAGS) $(O_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) \
	          -copt "$(EXTRA_LD_OPT)" \
	          -o $(BIGLOO_DEST).aux \
	          $(O_OBJECTS)
	@ if [ $(HOSTOS) = "cygwin" ]; then \
	    echo "Waiting a few seconds for the lock on $@ to be released..."; \
	    sleep 5s; \
	  fi
	mv $(BIGLOO_DEST).aux $@
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    bigloo.new                                                       */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST).$(NEW_SUFFIX): $(AFILE) $(JFILE) $(ETAGS) $(O_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) -o $@ $(O_OBJECTS)
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    bigloo.$(SHAREDSUFFIX)                                           */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST).$(SHAREDSUFFIX): $(O_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) -o $@ $(O_OBJECTS) \
                    -ldopt $(CSTRIPFLAGS)
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    mainnewo (for Bigloo++ regeneration)                             */
#*---------------------------------------------------------------------*/
mainnewo: $(O_LESS_MAIN_OBJECTS_OBJECTS)
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) -o $(BIGLOO_DEST)$(EXE_SUFFIX) $(ADDITIONAL_BFLAGS) \
	    $(INIT_MAIN_SCM) $(O_LESS_MAIN_OBJECTS) $(ADDITIONAL_LIBRARIES)

#*---------------------------------------------------------------------*/
#*    dist-bigloo.exe                                                  */
#*---------------------------------------------------------------------*/
dist-bigloo.exe: $(C_OBJECTS)
	@ echo "Comptime (distribution) done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    Tools/date.scm                                                   */
#*    -------------------------------------------------------------    */
#*    We produce a legal Scheme file, that implements a function,      */
#*    returning the date of the compiler bootstrap.                    */
#*---------------------------------------------------------------------*/
Tools/date.scm:
	@- $(RM) -f $@
	@ echo ";; Automatically generated file (don't edit)" > $@
	@ echo "(module tools_date (export (bigloo-date)))" >> $@
	@ echo "" >> $@
	@ echo "(define (bigloo-date) " \" `date` \"")" >> $@

#*---------------------------------------------------------------------*/
#*    Tools/license.scm                                                */
#*    -------------------------------------------------------------    */
#*    We produce the license module from the official Bigloo license.  */
#*---------------------------------------------------------------------*/
Tools/license.scm: $(LICENSE)
	@- $(RM) -f $@
	@ echo ";; Automatically generated file (don't edit)" > $@
	@ echo "(module tools_license (export (bigloo-license)))" >> $@
	@ echo "" >> $@
	@ echo "(define (bigloo-license)" >> $@
	@ echo " \" `cat ../LICENSE` \")" >> $@

#*---------------------------------------------------------------------*/
#*    afile                                                            */
#*    -------------------------------------------------------------    */
#*    This entry point produce the .afile file, the .etags file for    */
#*    emacs and update the Makefile for dependences.                   */
#*---------------------------------------------------------------------*/
.PHONY: ude

ude: Tools/date.scm Tools/license.scm $(AFILE) $(JFILE) $(ETAGS)
	@ echo "depend:"
	$(MAKE) dep

$(AFILE): $(SOURCE_FILES)
	@ echo "$(AFILE):"
	$(AFILE_BIN) -o $(AFILE) $(SOURCE_FILES)

$(JFILE): $(AFILE)
	@ echo "$(JFILE):"
	$(JFILE_BIN) -o $(JFILE) $(SOURCE_FILES)

$(ETAGS): $(AFILE)
	@ echo ".etags:"
	$(BTAGS_BIN) --define-var doc-define -o $(ETAGS) $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*    qnctags                                                          */
#*---------------------------------------------------------------------*/
.PHONY: qnctags
qnctags:
	etags `find . -name '*.scm' -print -o -name '*.[ch]' -print`

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: Tools/date.scm Tools/license.scm
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	$(DEPEND_BIN) -fno-mco $(SOURCE_FILES) >> /tmp/Makefile.aux
	mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    dist-updates                                                     */
#*    -------------------------------------------------------------    */
#*    When compiling a distribution we have to make some updates into  */
#*    the source files. Mostly we have to remove debug informations.   */
#*---------------------------------------------------------------------*/
dist-updates:
	echo "updating Tools/trace.sc[hm]"
	cat Tools/trace.sch | sed -e s/"#t"/"#f"/g > Tools/trace.schu; \
         $(RM) Tools/trace.sch; mv Tools/trace.schu Tools/trace.sch
	cat Tools/trace.scm | sed -e s/"#t"/"#f"/g > Tools/trace.scmu; \
         $(RM) Tools/trace.scm; mv Tools/trace.scmu Tools/trace.scm

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*    -------------------------------------------------------------    */
#*    This is the regular entry point for a standard Bigloo boot       */
#*    procedure (from the C files).                                    */
#*---------------------------------------------------------------------*/
.PHONY: boot doboot

boot: 
	touch $(AFILE)
	touch $(JFILE)
	$(MAKE) doboot SCMOBJSUFFIX="" COBJSUFFIX=%.o

doboot: $(O_OBJECTS)
	if [ "$(SHRD_COMP)" = "yes" -a -f $(BOOTLIBDIR)/lib$(LIBRARYNAME)_s-$(RELEASE).$(SHAREDSUFFIX) ]; then \
           gc=-l$(GCLIB)-$(RELEASE); \
           if [ "$(GCCUSTOM)" = "no" ]; then \
              if [ "$(GCLIBDIR) " = " " ]; then \
                gc=-lgc; \
              else \
                gc="-L$(GCLIBDIR) -lgc"; \
              fi; \
           fi; \
	   $(CC) $(EXTRA_LD_OPT) $(CFLAGS) -o $(BIGLOO_DEST)$(EXE_SUFFIX) $(O_OBJECTS) -L$(BOOTLIBDIR) -lbigloo_s-$(RELEASE) $$gc -lbigloo_s-$(RELEASE) $(LDCOMPLIBS) $(EXTRALIBS); \
	else \
           gc=$(BOOTLIBDIR)/lib$(GCLIB)-$(RELEASE).a; \
           if [ "$(GCCUSTOM)" = "no" ]; then \
              if [ "$(GCLIBDIR) " = " " ]; then \
                gc=-lgc; \
              else \
                gc="-L$(GCLIBDIR) -lgc"; \
              fi; \
           fi; \
	   $(CC) $(EXTRA_LD_OPT) $(CFLAGS) -o $(BIGLOO_DEST)$(EXE_SUFFIX) $(O_OBJECTS) $(BOOTLIBDIR)/libbigloo_s-$(RELEASE).a $$gc $(LDCOMPLIBS) $(EXTRALIBS); \
	fi
	-$(STRIP) $(BIGLOO_DEST)$(EXE_SUFFIX)
	@ echo "$(BIGLOO_DEST)$(EXE_SUFFIX) done..."

#*---------------------------------------------------------------------*/
#*    bigboot                                                          */
#*    -------------------------------------------------------------    */
#*    This is the regular entry point for a standard Bigloo boot       */
#*    procedure from the Scheme files, using an already (compatible)   */
#*    installed compiler.                                              */
#*---------------------------------------------------------------------*/
.PHONY: bigboot

bigboot: 
	$(MAKE) SCMOBJSUFFIX="%.o" LDBOOTFLAGS="-lib-version $(RELEASE) -libgc-version $(RELEASE) -static-bigloo -ldopt $(CSTRIPFLAGS)"
	-$(STRIP) $(BIGLOO_DEST)$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install install-compiler alias-compiler uninstall

install: install-compiler 
	$(MAKE) alias-compiler

install-compiler:
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(BOOTBINDIR)/bigloo$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bigloo$(RELEASE)$(EXE_SUFFIX) && \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bigloo$(RELEASE)$(EXE_SUFFIX); \
        fi

alias-compiler:
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
           (cd $(DESTDIR)$(BINDIR) && \
            $(RM) -f bigloo && \
            $(RM) -f bigloo$(EXE_SUFFIX) && \
            ln bigloo$(RELEASE)$(EXE_SUFFIX) bigloo$(EXE_SUFFIX)) || exit 1; \
        fi

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bigloo$(RELEASE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bigloo$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean-common:
	- $(RM) -f $(AFILE)
	- $(RM) -f $(JFILE)
	- $(RM) -f .etags
	find . \( -name '*[~%]'                     \
                       -o -name '.??*[~%]'          \
                       -o -name '#*#'               \
                       -o -name '?*#'               \
                       -o -name \*core \)           \
                     -type f -exec $(RM) {} \;   

clean-c:
	- $(RM) -f $(O_OBJECTS)

clean-jvm:
	- $(RM) -f $(CLASS_OBJECTS)
	- $(RM) -f $(BIGLOO_DEST).jvm
	- $(RM) -f $(BIGLOO_DEST).jar

clean-dotnet:
	- $(RM) -f $(DOTNET_OBJECTS)
	- $(RM) -f $(BIGLOO_DEST)-dotnet$(SCRIPTEXTENSION)
	- $(RM) -f $(BIGLOO_DEST)-dotnet$(EXE_SUFFIX)

clean: clean-common clean-c clean-jvm clean-dotnet
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@ if [ `pwd` = $(HOME)/prgm/project/bigloo/comptime ]; then \
             echo "*** ERROR:Illegal dir to make a cleanall `pwd`"; \
             exit 1; \
          fi
	- $(RM) -f $(C_OBJECTS)
	- $(RM) -f Tools/date.scm Tools/license.scm
	- $(RM) -f $(BIGLOO_DEST)$(EXE_SUFFIX)

distclean: 
	- $(RM) -f $(O_OBJECTS)
	- $(RM) -f $(BIGLOO_DEST)$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*     touchall ...                                                    */
#*---------------------------------------------------------------------*/
.PHONY: touchall
touchall:
	touch $(SOURCE_FILES)
	@ echo "touchall done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    getsources                                                       */
#*---------------------------------------------------------------------*/
getsources:
	@ echo $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .class                                                 */
#*---------------------------------------------------------------------*/
$(SCMCLASSSUFFIX): %.scm
	$(BIGLOO) -jvm $(BBFLAGS) $(BJVMFLAGS) $(EFLAGS) -c $*.scm -o $*.class

#*---------------------------------------------------------------------*/
#*     .scm --> .obj                                                   */
#*---------------------------------------------------------------------*/
$(DOTNETOBJSUFFIX): %.scm
	$(BIGLOO) -dotnet $(BBFLAGS) $(BDOTNETFLAGS) $(EFLAGS) -c $*.scm -o $*.obj

#*---------------------------------------------------------------------*/
#*     .c --> .o                                                       */
#*---------------------------------------------------------------------*/
$(COBJSUFFIX): %.c
	$(CC) $(CFLAGS) $(CADDFLAGS) $*.c -c -o $(*D)-$(@F) && \
        mv $(*D)-$(@F) $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .c                                                     */
#*---------------------------------------------------------------------*/
$(CSRCSUFFIX): %.scm
	$(BIGLOO) $(BBFLAGS) $(BDISTFLAGS) $(EFLAGS) -indent $*.scm

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop: Tools/date.scm Tools/license.scm
	@ echo $(POPULATION:%=comptime/%)

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
Engine/param.o: $(BOOTLIBDIR)/bigloo_config.h ../configure

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
./Inline/size.o ./Inline/size.class: Ast/node.sch 
./Prof/prof-emit.o ./Prof/prof-emit.class: Ast/node.sch Ast/unit.sch     \
      Tools/location.sch 
./Expand/exit.o ./Expand/exit.class: Expand/expander.sch Tools/trace.sch \
      Tools/location.sch 
./SawMill/regalloc.o ./SawMill/regalloc.class: Tools/trace.sch 
Globalize/node.o Globalize/node.class: Tools/trace.sch 
./Inline/loop.o ./Inline/loop.class: Tools/trace.sch 
./Expand/lambda.o ./Expand/lambda.class: Tools/trace.sch 
Ast/glo-def.o Ast/glo-def.class: Tools/trace.sch 
./Expand/eps.o ./Expand/eps.class: Expand/expander.sch Tools/trace.sch   \
      Engine/pass.sch Ast/unit.sch 
./Object/predicate.o ./Object/predicate.class: Tools/trace.sch 
./Globalize/walk.o ./Globalize/walk.class: Engine/pass.sch Ast/node.sch  \
      Tools/trace.sch 
Coerce/pproto.o Coerce/pproto.class: Tools/verbose.sch 
./Reduce/sbeta.o ./Reduce/sbeta.class: Tools/trace.sch 
Write/ast.o Write/ast.class: Engine/pass.sch Ast/node.sch 
./Integrate/loc2glo.o ./Integrate/loc2glo.class: Tools/trace.sch 
Effect/effect.o Effect/effect.class: Ast/node.sch 
./BackEnd/c-proto.o ./BackEnd/c-proto.class: Tvector/tvector.sch         \
      Tools/fprint.sch 
./Tailc/walk.o ./Tailc/walk.class: Engine/pass.sch 
./Reduce/cond.o ./Reduce/cond.class: Tools/trace.sch 
SawC/code.o SawC/code.class: Tools/trace.sch Tools/location.sch 
Module/impuse.o Module/impuse.class: Ast/unit.sch Tools/trace.sch 
./Globalize/loc2glo.o ./Globalize/loc2glo.class: Tools/trace.sch 
Module/type.o Module/type.class: Ast/unit.sch Tools/trace.sch 
Ast/build.o Ast/build.class: Engine/pass.sch Ast/node.sch 
./Globalize/integration28c.o ./Globalize/integration28c.class:           \
      Tools/trace.sch 
Foreign/calias.o Foreign/calias.class: Tools/trace.sch 
Ast/check-sharing.o Ast/check-sharing.class: Ast/unit.sch                \
      Engine/pass.sch 
./Globalize/gloclo.o ./Globalize/gloclo.class: Tools/trace.sch 
BackEnd/jvm.o BackEnd/jvm.class: Engine/pass.sch 
Globalize/ginfo.o Globalize/ginfo.class: Ast/node.sch 
./Globalize/gn.o ./Globalize/gn.class: Tools/trace.sch 
./Integrate/let-fun.o ./Integrate/let-fun.class: Tools/trace.sch 
./Reduce/1occ.o ./Reduce/1occ.class: Tools/trace.sch 
SawJvm/code.o SawJvm/code.class: Tools/location.sch 
Reduce/cse.o Reduce/cse.class: Tools/trace.sch 
Read/inline.o Read/inline.class: Tools/trace.sch Ast/unit.sch 
./Expand/let.o ./Expand/let.class: Ast/node.sch Tools/location.sch 
Tools/location.o Tools/location.class: Tools/location.sch 
Integrate/walk.o Integrate/walk.class: Engine/pass.sch 
Globalize/globalize.o Globalize/globalize.class: Tools/trace.sch         \
      Tools/verbose.sch 
Module/include.o Module/include.class: Ast/unit.sch 
./Ast/occur.o ./Ast/occur.class: Ast/node.sch 
./Init/extend.o ./Init/extend.class: Engine/pass.sch 
Object/java-access.o Object/java-access.class: Tools/trace.sch 
./Heap/restore.o ./Heap/restore.class: Engine/pass.sch 
./Ast/exit.o ./Ast/exit.class: Ast/node.sch Tools/trace.sch 
./Object/plain-access.o ./Object/plain-access.class: Tools/trace.sch 
./Coerce/convert.o ./Coerce/convert.class: Tools/trace.sch               \
      Tools/location.sch Type/coercer.sch 
./Cgen/cgen.o ./Cgen/cgen.class: Tools/trace.sch Tools/location.sch 
Bdb/spread-obj.o Bdb/spread-obj.class: Engine/pass.sch Ast/node.sch 
./Init/parse-args.o ./Init/parse-args.class: Tools/trace.sch             \
      Engine/pass.sch Init/pass-args-parse.sch 
./SawMsil/compile.o ./SawMsil/compile.class: Tvector/tvector.sch 
./Object/java-access.o ./Object/java-access.class: Tools/trace.sch 
./Cfa/iterate.o ./Cfa/iterate.class: Tools/trace.sch 
Globalize/kapture.o Globalize/kapture.class: Tools/trace.sch 
./Type/tenv.o ./Type/tenv.class: Tools/trace.sch Type/coercer.sch 
./Expand/expander.o ./Expand/expander.class: Expand/expander.sch 
Cfa/arithmetic.o Cfa/arithmetic.class: Tools/trace.sch 
Expand/case.o Expand/case.class: Tools/trace.sch 
Module/alibrary.o Module/alibrary.class: Ast/unit.sch 
Module/class.o Module/class.class: Ast/unit.sch Tools/trace.sch 
Cgen/emit-cop.o Cgen/emit-cop.class: Tools/location.sch Tools/fprint.sch \
      Tools/trace.sch 
Expand/define.o Expand/define.class: Expand/expander.sch Tools/trace.sch 
Ast/labels.o Ast/labels.class: Ast/node.sch Tools/trace.sch              \
      Tools/location.sch 
Expand/initial.o Expand/initial.class: Tools/location.sch 
./Ast/init.o ./Ast/init.class: Engine/pass.sch Ast/unit.sch              \
      Tools/trace.sch 
./Cnst/alloc.o ./Cnst/alloc.class: Tvector/tvector.sch 
Ast/unit.o Ast/unit.class: Ast/unit.sch Ast/node.sch Tools/trace.sch 
./Coerce/coerce.o ./Coerce/coerce.class: Tools/trace.sch 
Bdb/walk.o Bdb/walk.class: Engine/pass.sch 
Foreign/ctype.o Foreign/ctype.class: Tools/trace.sch 
./Inline/simple.o ./Inline/simple.class: Tools/trace.sch                 \
      Tools/verbose.sch 
./Callcc/walk.o ./Callcc/walk.class: Engine/pass.sch Ast/node.sch 
./Cfa/tvector.o ./Cfa/tvector.class: Tools/trace.sch Ast/unit.sch        \
      Tvector/tvector.sch 
./Integrate/kk.o ./Integrate/kk.class: Tools/trace.sch 
./SawJvm/compile.o ./SawJvm/compile.class: Tvector/tvector.sch           \
      Tools/trace.sch 
./Integrate/ctn.o ./Integrate/ctn.class: Tools/trace.sch 
./Ast/toccur.o ./Ast/toccur.class: Ast/node.sch 
./Ast/glo-decl.o ./Ast/glo-decl.class: Tools/trace.sch 
./Foreign/jtype.o ./Foreign/jtype.class: Tools/trace.sch 
./Inline/walk.o ./Inline/walk.class: Engine/pass.sch Tools/trace.sch     \
      Ast/node.sch 
./User/user.o ./User/user.class: Engine/pass.sch Ast/unit.sch 
./Ast/let.o ./Ast/let.class: Ast/node.sch Tools/trace.sch                \
      Tools/location.sch 
./Integrate/free.o ./Integrate/free.class: Tools/trace.sch 
Integrate/g.o Integrate/g.class: Tools/trace.sch 
Expand/mvalue.o Expand/mvalue.class: Expand/expander.sch Tools/trace.sch 
Ast/alphatize.o Ast/alphatize.class: Ast/node.sch Tools/location.sch 
./Cfa/loose.o ./Cfa/loose.class: Tools/trace.sch 
Inline/recursion.o Inline/recursion.class: Tools/trace.sch               \
      Tools/verbose.sch 
./Cfa/setup.o ./Cfa/setup.class: Tools/trace.sch 
./Cfa/app.o ./Cfa/app.class: Tools/trace.sch 
./BackEnd/c.o ./BackEnd/c.class: Engine/pass.sch Ast/unit.sch            \
      Tools/trace.sch 
Effect/feffect.o Effect/feffect.class: Tools/trace.sch 
Cfa/collect.o Cfa/collect.class: Tools/trace.sch 
Module/foreign.o Module/foreign.class: Ast/unit.sch Tools/trace.sch 
Ast/sexp.o Ast/sexp.class: Tools/trace.sch Tools/location.sch 
Tvector/cnst.o Tvector/cnst.class: Tvector/tvector.sch 
./Hgen/walk.o ./Hgen/walk.class: Engine/pass.sch 
Trace/walk.o Trace/walk.class: Engine/pass.sch Ast/node.sch              \
      Tools/location.sch 
Inline/inline.o Inline/inline.class: Ast/node.sch Tools/trace.sch 
./Ast/pragma.o ./Ast/pragma.class: Ast/node.sch 
Ast/let.o Ast/let.class: Ast/node.sch Tools/trace.sch Tools/location.sch 
./Globalize/node.o ./Globalize/node.class: Tools/trace.sch 
Inline/variant.o Inline/variant.class: Tools/trace.sch 
Ast/occur.o Ast/occur.class: Ast/node.sch 
Ast/substitute.o Ast/substitute.class: Tools/trace.sch 
Tools/error.o Tools/error.class: Tools/location.sch Ast/node.sch 
Reduce/copy.o Reduce/copy.class: Tools/trace.sch 
Cfa/app.o Cfa/app.class: Tools/trace.sch 
Cgen/cgen.o Cgen/cgen.class: Tools/trace.sch Tools/location.sch 
Cnst/initialize.o Cnst/initialize.class: Tools/trace.sch Ast/unit.sch 
Inline/size.o Inline/size.class: Ast/node.sch 
Expand/exit.o Expand/exit.class: Expand/expander.sch Tools/trace.sch     \
      Tools/location.sch 
Object/coercion.o Object/coercion.class: Tools/trace.sch 
Cfa/walk.o Cfa/walk.class: Engine/pass.sch 
Inline/loop.o Inline/loop.class: Tools/trace.sch 
./Foreign/cptr.o ./Foreign/cptr.class: Tools/trace.sch 
Inline/app.o Inline/app.class: Tools/trace.sch 
./Globalize/globalize.o ./Globalize/globalize.class: Tools/trace.sch     \
      Tools/verbose.sch 
Type/tenv.o Type/tenv.class: Tools/trace.sch Type/coercer.sch 
Ast/find-gdefs.o Ast/find-gdefs.class: Tools/trace.sch Ast/node.sch 
./Integrate/u.o ./Integrate/u.class: Tools/trace.sch 
./Foreign/calias.o ./Foreign/calias.class: Tools/trace.sch 
./Cfa/funcall.o ./Cfa/funcall.class: Tools/trace.sch 
./Cfa/struct.o ./Cfa/struct.class: Tools/trace.sch 
./Reduce/walk.o ./Reduce/walk.class: Engine/pass.sch 
./Prof/unit.o ./Prof/unit.class: Engine/pass.sch Ast/unit.sch 
Bdb/bdb-emit.o Bdb/bdb-emit.class: Ast/node.sch Ast/unit.sch             \
      Tools/location.sch 
Reduce/cond.o Reduce/cond.class: Tools/trace.sch 
Module/module.o Module/module.class: Ast/unit.sch Engine/pass.sch 
./Reduce/same.o ./Reduce/same.class: Tools/trace.sch 
Reduce/ftypec.o Reduce/ftypec.class: Tools/trace.sch 
./Reduce/typec.o ./Reduce/typec.class: Tools/trace.sch 
./Type/coercion.o ./Type/coercion.class: Type/coercer.sch                \
      Tools/trace.sch 
./Heap/make.o ./Heap/make.class: Engine/pass.sch Tools/trace.sch 
Integrate/node.o Integrate/node.class: Tools/trace.sch 
Integrate/cto.o Integrate/cto.class: Tools/trace.sch 
./Cfa/procedure.o ./Cfa/procedure.class: Tools/trace.sch 
./Tools/location.o ./Tools/location.class: Tools/location.sch 
./Tools/shape.o ./Tools/shape.class: Ast/node.sch Tvector/tvector.sch 
./Module/include.o ./Module/include.class: Ast/unit.sch 
./Effect/cgraph.o ./Effect/cgraph.class: Ast/node.sch Tools/trace.sch 
./Object/nil.o ./Object/nil.class: Tools/trace.sch 
./Foreign/cstruct.o ./Foreign/cstruct.class: Tools/trace.sch 
./BackEnd/cplib.o ./BackEnd/cplib.class: Tvector/tvector.sch 
./Integrate/walk.o ./Integrate/walk.class: Engine/pass.sch 
./Type/typeof.o ./Type/typeof.class: Tvector/tvector.sch 
./Inline/recursion.o ./Inline/recursion.class: Tools/trace.sch           \
      Tools/verbose.sch 
./Cgen/capp.o ./Cgen/capp.class: Tools/trace.sch 
Reduce/1occ.o Reduce/1occ.class: Tools/trace.sch 
User/user.o User/user.class: Engine/pass.sch Ast/unit.sch 
./Ast/check-sharing.o ./Ast/check-sharing.class: Ast/unit.sch            \
      Engine/pass.sch 
./Cnst/walk.o ./Cnst/walk.class: Engine/pass.sch 
./Bdb/spread-obj.o ./Bdb/spread-obj.class: Engine/pass.sch Ast/node.sch 
Init/extend.o Init/extend.class: Engine/pass.sch 
Object/slots.o Object/slots.class: Tools/trace.sch 
Effect/spread.o Effect/spread.class: Ast/node.sch 
Cfa/loose.o Cfa/loose.class: Tools/trace.sch 
./Ast/remove.o ./Ast/remove.class: Ast/node.sch 
BackEnd/c.o BackEnd/c.class: Engine/pass.sch Ast/unit.sch                \
      Tools/trace.sch 
Cfa/setup.o Cfa/setup.class: Tools/trace.sch 
Module/library.o Module/library.class: Ast/unit.sch 
Globalize/integration28c.o Globalize/integration28c.class:               \
      Tools/trace.sch 
./Cfa/arithmetic.o ./Cfa/arithmetic.class: Tools/trace.sch 
Hgen/walk.o Hgen/walk.class: Engine/pass.sch 
Expand/lambda.o Expand/lambda.class: Tools/trace.sch 
Engine/compiler.o Engine/compiler.class: Ast/unit.sch Engine/pass.sch 
Cfa/iterate.o Cfa/iterate.class: Tools/trace.sch 
./Coerce/app.o ./Coerce/app.class: Tools/trace.sch 
./Expand/initial.o ./Expand/initial.class: Tools/location.sch 
./Expand/assert.o ./Expand/assert.class: Tools/location.sch 
Object/creator.o Object/creator.class: Tools/trace.sch 
Object/wide-access.o Object/wide-access.class: Tools/trace.sch 
./Ast/app.o ./Ast/app.class: Ast/node.sch Tools/trace.sch 
Cfa/vector.o Cfa/vector.class: Tools/trace.sch 
./Object/wide-access.o ./Object/wide-access.class: Tools/trace.sch 
./Module/option.o ./Module/option.class: Ast/unit.sch 
./Cfa/closure.o ./Cfa/closure.class: Tools/trace.sch 
./Ast/dump.o ./Ast/dump.class: Tvector/tvector.sch Ast/node.sch 
./Globalize/kapture.o ./Globalize/kapture.class: Tools/trace.sch 
Ast/exit.o Ast/exit.class: Ast/node.sch Tools/trace.sch 
./Integrate/a.o ./Integrate/a.class: Tools/trace.sch 
Callcc/walk.o Callcc/walk.class: Engine/pass.sch Ast/node.sch 
Cfa/tvector.o Cfa/tvector.class: Tools/trace.sch Ast/unit.sch            \
      Tvector/tvector.sch 
Ast/app.o Ast/app.class: Ast/node.sch Tools/trace.sch 
./Cfa/show.o ./Cfa/show.class: Tools/trace.sch 
Expand/eps.o Expand/eps.class: Expand/expander.sch Tools/trace.sch       \
      Engine/pass.sch Ast/unit.sch 
Prof/unit.o Prof/unit.class: Engine/pass.sch Ast/unit.sch 
Coerce/apply.o Coerce/apply.class: Tools/trace.sch Tools/location.sch 
Coerce/funcall.o Coerce/funcall.class: Tools/trace.sch                   \
      Tools/location.sch 
Inline/walk.o Inline/walk.class: Engine/pass.sch Tools/trace.sch         \
      Ast/node.sch 
./Effect/feffect.o ./Effect/feffect.class: Tools/trace.sch 
./Module/foreign.o ./Module/foreign.class: Ast/unit.sch Tools/trace.sch 
./Module/java.o ./Module/java.class: Ast/unit.sch Tools/trace.sch 
./Expand/if.o ./Expand/if.class: Tools/trace.sch 
Tailc/walk.o Tailc/walk.class: Engine/pass.sch 
Heap/make.o Heap/make.class: Engine/pass.sch Tools/trace.sch 
Ast/init.o Ast/init.class: Engine/pass.sch Ast/unit.sch Tools/trace.sch 
./Ast/apply.o ./Ast/apply.class: Ast/node.sch 
./Ast/substitute.o ./Ast/substitute.class: Tools/trace.sch 
./Module/class.o ./Module/class.class: Ast/unit.sch Tools/trace.sch 
./Inline/variant.o ./Inline/variant.class: Tools/trace.sch 
./Effect/walk.o ./Effect/walk.class: Engine/pass.sch Tools/trace.sch 
./Globalize/ginfo.o ./Globalize/ginfo.class: Ast/node.sch 
./Cfa/cfa.o ./Cfa/cfa.class: Tools/trace.sch 
Cgen/capp.o Cgen/capp.class: Tools/trace.sch 
./Fail/walk.o ./Fail/walk.class: Engine/pass.sch Tools/location.sch      \
      Ast/node.sch 
./Object/getter.o ./Object/getter.class: Tools/trace.sch 
Cnst/walk.o Cnst/walk.class: Engine/pass.sch 
Globalize/new-body.o Globalize/new-body.class: Tools/trace.sch 
Expand/let.o Expand/let.class: Ast/node.sch Tools/location.sch 
./Ast/find-gdefs.o ./Ast/find-gdefs.class: Tools/trace.sch Ast/node.sch 
BackEnd/c-proto.o BackEnd/c-proto.class: Tvector/tvector.sch             \
      Tools/fprint.sch 
Cfa/cfa.o Cfa/cfa.class: Tools/trace.sch 
Write/expanded.o Write/expanded.class: Ast/unit.sch 
./Module/eval.o ./Module/eval.class: Ast/unit.sch Ast/node.sch 
./Globalize/new-body.o ./Globalize/new-body.class: Tools/trace.sch 
SawMill/node2rtl.o SawMill/node2rtl.class: Tools/location.sch 
Cfa/specialize.o Cfa/specialize.class: Tools/trace.sch 
Globalize/free.o Globalize/free.class: Tools/trace.sch 
./Coerce/pproto.o ./Coerce/pproto.class: Tools/verbose.sch 
Coerce/coerce.o Coerce/coerce.class: Tools/trace.sch 
./Module/alibrary.o ./Module/alibrary.class: Ast/unit.sch 
Inline/simple.o Inline/simple.class: Tools/trace.sch Tools/verbose.sch 
./Reduce/cse.o ./Reduce/cse.class: Tools/trace.sch 
./Beta/walk.o ./Beta/walk.class: Engine/pass.sch Tools/trace.sch         \
      Ast/node.sch 
./Cfa/box.o ./Cfa/box.class: Tools/trace.sch 
Integrate/ctn.o Integrate/ctn.class: Tools/trace.sch 
Integrate/u.o Integrate/u.class: Tools/trace.sch 
./Effect/effect.o ./Effect/effect.class: Ast/node.sch 
Bdb/bdb-initialize.o Bdb/bdb-initialize.class: Tools/trace.sch           \
      Ast/unit.sch 
Reduce/sbeta.o Reduce/sbeta.class: Tools/trace.sch 
Reduce/walk.o Reduce/walk.class: Engine/pass.sch 
Cfa/funcall.o Cfa/funcall.class: Tools/trace.sch 
Ast/check-init.o Ast/check-init.class: Ast/unit.sch 
./Module/impuse.o ./Module/impuse.class: Ast/unit.sch Tools/trace.sch 
./Integrate/node.o ./Integrate/node.class: Tools/trace.sch 
Reduce/same.o Reduce/same.class: Tools/trace.sch 
Foreign/jtype.o Foreign/jtype.class: Tools/trace.sch 
Globalize/clocto.o Globalize/clocto.class: Tools/trace.sch 
./Tools/progn.o ./Tools/progn.class: Tools/location.sch 
./Tvector/cnst.o ./Tvector/cnst.class: Tvector/tvector.sch 
./Bdb/bdb-initialize.o ./Bdb/bdb-initialize.class: Tools/trace.sch       \
      Ast/unit.sch 
./Cnst/node.o ./Cnst/node.class: Tools/trace.sch Tvector/tvector.sch 
Cfa/box.o Cfa/box.class: Tools/trace.sch 
Cnst/alloc.o Cnst/alloc.class: Tvector/tvector.sch 
Tools/shape.o Tools/shape.class: Ast/node.sch Tvector/tvector.sch 
./Read/load.o ./Read/load.class: Tools/trace.sch 
Init/parse-args.o Init/parse-args.class: Tools/trace.sch Engine/pass.sch \
      Init/pass-args-parse.sch 
SawMsil/compile.o SawMsil/compile.class: Tvector/tvector.sch 
Type/typeof.o Type/typeof.class: Tvector/tvector.sch 
./Ast/labels.o ./Ast/labels.class: Ast/node.sch Tools/trace.sch          \
      Tools/location.sch 
./Module/library.o ./Module/library.class: Ast/unit.sch 
Ast/toccur.o Ast/toccur.class: Ast/node.sch 
Expand/if.o Expand/if.class: Tools/trace.sch 
Expand/expander.o Expand/expander.class: Expand/expander.sch 
Module/statexp.o Module/statexp.class: Ast/unit.sch 
Globalize/gn.o Globalize/gn.class: Tools/trace.sch 
Ast/apply.o Ast/apply.class: Ast/node.sch 
./Coerce/walk.o ./Coerce/walk.class: Engine/pass.sch 
Integrate/loc2glo.o Integrate/loc2glo.class: Tools/trace.sch 
./Object/creator.o ./Object/creator.class: Tools/trace.sch 
./Ast/glo-def.o ./Ast/glo-def.class: Tools/trace.sch 
Fail/walk.o Fail/walk.class: Engine/pass.sch Tools/location.sch          \
      Ast/node.sch 
Globalize/loc2glo.o Globalize/loc2glo.class: Tools/trace.sch 
Prof/prof-emit.o Prof/prof-emit.class: Ast/node.sch Ast/unit.sch         \
      Tools/location.sch 
Heap/restore.o Heap/restore.class: Engine/pass.sch 
./Bdb/bdb-emit.o ./Bdb/bdb-emit.class: Ast/node.sch Ast/unit.sch         \
      Tools/location.sch 
./Cgen/emit-cop.o ./Cgen/emit-cop.class: Tools/location.sch              \
      Tools/fprint.sch Tools/trace.sch 
./Expand/define.o ./Expand/define.class: Expand/expander.sch             \
      Tools/trace.sch 
./SawMill/node2rtl.o ./SawMill/node2rtl.class: Tools/location.sch 
Globalize/walk.o Globalize/walk.class: Engine/pass.sch Ast/node.sch      \
      Tools/trace.sch 
Ast/pragma.o Ast/pragma.class: Ast/node.sch 
./Cnst/initialize.o ./Cnst/initialize.class: Tools/trace.sch             \
      Ast/unit.sch 
SawC/compile.o SawC/compile.class: Engine/pass.sch Ast/unit.sch          \
      Tools/trace.sch 
Cfa/closure.o Cfa/closure.class: Tools/trace.sch 
./Object/coercion.o ./Object/coercion.class: Tools/trace.sch 
Integrate/let-fun.o Integrate/let-fun.class: Tools/trace.sch 
./Module/type.o ./Module/type.class: Ast/unit.sch Tools/trace.sch 
Globalize/integration.o Globalize/integration.class: Tools/trace.sch 
./Foreign/ctype.o ./Foreign/ctype.class: Tools/trace.sch 
Integrate/a.o Integrate/a.class: Tools/trace.sch 
Type/coercion.o Type/coercion.class: Type/coercer.sch Tools/trace.sch 
./Coerce/funcall.o ./Coerce/funcall.class: Tools/trace.sch               \
      Tools/location.sch 
./Trace/walk.o ./Trace/walk.class: Engine/pass.sch Ast/node.sch          \
      Tools/location.sch 
Cfa/procedure.o Cfa/procedure.class: Tools/trace.sch 
Beta/walk.o Beta/walk.class: Engine/pass.sch Tools/trace.sch             \
      Ast/node.sch 
./Globalize/clocto.o ./Globalize/clocto.class: Tools/trace.sch 
BackEnd/cplib.o BackEnd/cplib.class: Tvector/tvector.sch 
Effect/cgraph.o Effect/cgraph.class: Ast/node.sch Tools/trace.sch 
./Object/slots.o ./Object/slots.class: Tools/trace.sch 
./Integrate/definition.o ./Integrate/definition.class: Tools/trace.sch   \
      Tools/verbose.sch 
Integrate/kk.o Integrate/kk.class: Tools/trace.sch 
./Read/inline.o ./Read/inline.class: Tools/trace.sch Ast/unit.sch 
./SawJvm/code.o ./SawJvm/code.class: Tools/location.sch 
Module/java.o Module/java.class: Ast/unit.sch Tools/trace.sch 
./Ast/alphatize.o ./Ast/alphatize.class: Ast/node.sch Tools/location.sch 
./Expand/mvalue.o ./Expand/mvalue.class: Expand/expander.sch             \
      Tools/trace.sch 
Cfa/struct.o Cfa/struct.class: Tools/trace.sch 
Ast/glo-decl.o Ast/glo-decl.class: Tools/trace.sch 
BackEnd/dotnet.o BackEnd/dotnet.class: Engine/pass.sch 
Cnst/node.o Cnst/node.class: Tools/trace.sch Tvector/tvector.sch 
Ast/dump.o Ast/dump.class: Tvector/tvector.sch Ast/node.sch 
./Ast/unit.o ./Ast/unit.class: Ast/unit.sch Ast/node.sch Tools/trace.sch 
./Inline/app.o ./Inline/app.class: Tools/trace.sch 
./Bdb/walk.o ./Bdb/walk.class: Engine/pass.sch 
Effect/walk.o Effect/walk.class: Engine/pass.sch Tools/trace.sch 
Read/load.o Read/load.class: Tools/trace.sch 
./Inline/inline.o ./Inline/inline.class: Ast/node.sch Tools/trace.sch 
Cfa/show.o Cfa/show.class: Tools/trace.sch 
./Globalize/integration.o ./Globalize/integration.class: Tools/trace.sch 
SawMill/regalloc.o SawMill/regalloc.class: Tools/trace.sch 
Object/nil.o Object/nil.class: Tools/trace.sch 
Coerce/convert.o Coerce/convert.class: Tools/trace.sch                   \
      Tools/location.sch Type/coercer.sch 
Expand/assert.o Expand/assert.class: Tools/location.sch 
./Write/expanded.o ./Write/expanded.class: Ast/unit.sch 
./Expand/case.o ./Expand/case.class: Tools/trace.sch 
./Write/ast.o ./Write/ast.class: Engine/pass.sch Ast/node.sch 
./Globalize/free.o ./Globalize/free.class: Tools/trace.sch 
./Cfa/specialize.o ./Cfa/specialize.class: Tools/trace.sch 
./Cc/exec.o ./Cc/exec.class: Tools/trace.sch 
Object/predicate.o Object/predicate.class: Tools/trace.sch 
Module/option.o Module/option.class: Ast/unit.sch 
./Engine/compiler.o ./Engine/compiler.class: Ast/unit.sch                \
      Engine/pass.sch 
./SawC/code.o ./SawC/code.class: Tools/trace.sch Tools/location.sch 
Module/eval.o Module/eval.class: Ast/unit.sch Ast/node.sch 
Ast/remove.o Ast/remove.class: Ast/node.sch 
./Ast/sexp.o ./Ast/sexp.class: Tools/trace.sch Tools/location.sch 
./Ast/build.o ./Ast/build.class: Engine/pass.sch Ast/node.sch 
./Coerce/apply.o ./Coerce/apply.class: Tools/trace.sch                   \
      Tools/location.sch 
./Ast/check-init.o ./Ast/check-init.class: Ast/unit.sch 
Integrate/definition.o Integrate/definition.class: Tools/trace.sch       \
      Tools/verbose.sch 
Foreign/cptr.o Foreign/cptr.class: Tools/trace.sch 
Cc/exec.o Cc/exec.class: Tools/trace.sch 
Object/plain-access.o Object/plain-access.class: Tools/trace.sch 
Coerce/app.o Coerce/app.class: Tools/trace.sch 
Foreign/cstruct.o Foreign/cstruct.class: Tools/trace.sch 
./Module/module.o ./Module/module.class: Ast/unit.sch Engine/pass.sch 
SawJvm/compile.o SawJvm/compile.class: Tvector/tvector.sch               \
      Tools/trace.sch 
Globalize/gloclo.o Globalize/gloclo.class: Tools/trace.sch 
./Reduce/ftypec.o ./Reduce/ftypec.class: Tools/trace.sch 
./Integrate/g.o ./Integrate/g.class: Tools/trace.sch 
Tools/progn.o Tools/progn.class: Tools/location.sch 
./Cfa/walk.o ./Cfa/walk.class: Engine/pass.sch 
./Integrate/cto.o ./Integrate/cto.class: Tools/trace.sch 
./Cfa/collect.o ./Cfa/collect.class: Tools/trace.sch 
Reduce/typec.o Reduce/typec.class: Tools/trace.sch 
Integrate/free.o Integrate/free.class: Tools/trace.sch 
./Module/statexp.o ./Module/statexp.class: Ast/unit.sch 
Integrate/kaptured.o Integrate/kaptured.class: Tools/trace.sch 
./Cfa/vector.o ./Cfa/vector.class: Tools/trace.sch 
Object/getter.o Object/getter.class: Tools/trace.sch 
./Effect/spread.o ./Effect/spread.class: Ast/node.sch 
./Tools/error.o ./Tools/error.class: Tools/location.sch Ast/node.sch 
./Integrate/kaptured.o ./Integrate/kaptured.class: Tools/trace.sch 
./Reduce/copy.o ./Reduce/copy.class: Tools/trace.sch 
Coerce/walk.o Coerce/walk.class: Engine/pass.sch 

#bdepend stop
