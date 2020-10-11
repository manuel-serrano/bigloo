#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/comptime/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 08:29:03 1998                          */
#*    Last change :  Tue Mar 24 15:47:57 2020 (serrano)                */
#*    Copyright   :  1998-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the compiler part of Bigloo (bootstrap).   */
#*    This Makefile has to be processed by Gnu make.                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the produced executable
BIGLOO_DEST	= $(BOOTBINDIR)/bigloo
# the shell to be used
SHELL           = /bin/sh
# another produced executable
NEW_SUFFIX	= new
# another target.
DEFAULT		= plain
# These variables control how we are booting (from Scheme or C)
SCMOBJSUFFIX	= %.o
SCMCLASSSUFFIX	= %.class
COBJSUFFIX	= 
CSRCSUFFIX	= 

#*---------------------------------------------------------------------*/
#*    Bigloo compilation flags                                         */
#*---------------------------------------------------------------------*/
LIBDIRFLAGS	= -lib-dir $(BOOTLIBDIR)
# Commmon flags
COMMONFLAGS	= -q \
                  -afile .afile \
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
# Link flag
LDBOOTFLAGS     = -static-bigloo $(LDSTRIPFLAGS)
# C compiler flag
CADDFLAGS	= -I$(BOOTLIBDIR)

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
TOOLS_FILE	= args location progn trace speek error shape misc \
                  dsssl

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
                  check_type object walk

AST_OBJ		= $(AST_FILE:%=Ast/%.o)

#*--- beta ------------------------------------------------------------*/
BETA_FILE	= walk

BETA_OBJ	= $(BETA_FILE:%=Beta/%.o)

#*--- cc --------------------------------------------------------------*/
CC_FILE		= exec indent cc ld roots

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
                  if initial lambda let map struct exit object mvalue

EXPAND_OBJ	= $(EXPAND_FILE:%=Expand/%.o)

#*--- inline ----------------------------------------------------------*/
INLINE_FILE	= walk inline app size simple recursion variant loop

INLINE_OBJ	= $(INLINE_FILE:%=Inline/%.o)

#*--- fxop ------------------------------------------------------------*/
FXOP_FILE	= walk

FXOP_OBJ	= $(FXOP_FILE:%=Fxop/%.o)

#*--- coerce ----------------------------------------------------------*/
COERCE_FILE	= walk pproto coerce app funcall apply convert

COERCE_OBJ	= $(COERCE_FILE:%=Coerce/%.o)

#*--- Dataflow --------------------------------------------------------*/
DATAFLOW_FILE	= walk

DATAFLOW_OBJ	= $(DATAFLOW_FILE:%=Dataflow/%.o)

#*--- Initflow --------------------------------------------------------*/
INITFLOW_FILE	= walk

INITFLOW_OBJ	= $(INITFLOW_FILE:%=Initflow/%.o)

#*--- stackable -------------------------------------------------------*/
STACKABLE_FILE  = walk

STACKABLE_OBJ   = $(STACKABLE_FILE:%=Stackable/%.o)

#*--- globalize -------------------------------------------------------*/
GLOBALIZE_FILE	= walk ginfo escape gn node free kapture \
                  integration new_body loc2glo gloclo clocto globalize

GLOBALIZE_OBJ	= $(GLOBALIZE_FILE:%=Globalize/%.o)

#*--- cnst ------------------------------------------------------------*/
CNST_FILE	= walk cache alloc node initialize

CNST_OBJ	= $(CNST_FILE:%=Cnst/%.o)

#*--- cfa -------------------------------------------------------------*/
CFA_FILE	= app cinfo iterate set approx collect loose show walk \
                  setup vector pair cfa funcall procedure struct type \
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
                  jvm jvm_class

BACKEND_OBJ	= $(BACKEND_FILE:%=BackEnd/%.o)

#*--- SawMill ---------------------------------------------------------*/
SAW_FILE        = lib defs rtl node2rtl collapse remove InlineReturn \
		  blockorder gotos cast expr woodcutter \
		  procedures elsewhere regalloc regset regutils \
                  bbv bbv-types bbv-specialize bbv-cache bbv-utils

SAW_OBJ		= $(SAW_FILE:%=SawMill/%.o)

#*--- SawC ------------------------------------------------------------*/
SAW_C_FILE      = compile code

SAW_C_OBJ	= $(SAW_C_FILE:%=SawC/%.o)

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
TRACE_FILE	= walk isloop

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

#*--- sync ------------------------------------------------------------*/
SYNC_FILE	= node failsafe

SYNC_OBJ	= $(SYNC_FILE:%=Sync/%.o)

#*--- object ----------------------------------------------------------*/
OBJECT_FILE	= class generic method getter \
                  java_access \
                  slots tools coercion predicate creator nil \
                  classgen

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

#*--- return ----------------------------------------------------------*/
RETURN_FILE	= walk

RETURN_OBJ	= $(RETURN_FILE:%=Return/%.o)

#*--- uncell ----------------------------------------------------------*/
UNCELL_FILE	= walk

UNCELL_OBJ	= $(UNCELL_FILE:%=Uncell/%.o)

#*--- user ------------------------------------------------------------*/
USER_FILE	= user

USER_OBJ	= $(USER_FILE:%=User/%.o)

#*--- isa -------------------------------------------------------------*/
ISA_FILE	= walk

ISA_OBJ	        = $(ISA_FILE:%=Isa/%.o)

#*---------------------------------------------------------------------*/
#*    The object files                                                 */
#*---------------------------------------------------------------------*/
O_OBJECTS_NOIJ  = $(ENGINE_OBJ) $(WRITE_OBJ) \
                  $(READ_OBJ) $(AST_OBJ) $(BETA_OBJ) $(CC_OBJ) $(FOREIGN_OBJ) \
                  $(HEAP_OBJ) $(MODULE_OBJ) $(TYPE_OBJ) $(EXPAND_OBJ) \
                  $(INLINE_OBJ) $(BIVALUE_OBJ) $(USER_OBJ) $(HGEN_OBJ) \
                  $(FXOP_OBJ) $(COERCE_OBJ) $(DATAFLOW_OBJ) $(INITFLOW_OBJ) \
                  $(GLOBALIZE_OBJ) $(CNST_OBJ) $(CFA_OBJ) $(INTEGRATE_OBJ) \
                  $(TAILC_OBJ) $(CGEN_OBJ) $(TVECTOR_OBJ) $(TSTRUCT_OBJ) \
                  $(TRACE_OBJ) $(CALLCC_OBJ) $(EVAL_OBJ) $(LIFEXT_OBJ) \
                  $(FAIL_OBJ) $(REDUCE_OBJ) $(EFFECT_OBJ) $(OBJECT_OBJ) \
                  $(TOOLS_OBJ) $(BDB_OBJ) $(PROF_OBJ) $(ABOUND_OBJ) \
                  $(JAS_OBJ) $(BACKEND_OBJ) $(SAW_OBJ) $(SAW_C_OBJ) \
                  $(SAW_JVM_OBJ) $(SYNC_OBJ) $(ISA_OBJ) $(RETURN_OBJ) \
                  $(STACKABLE_OBJ) $(UNCELL_OBJ)

O_OBJECTS_NOINIT=$(O_OBJECTS_NOIJ)

O_OBJECTS       = $(O_OBJECTS_NOINIT) $(INIT_OBJ)

G_OBJECTS	= Tools/date.o Tools/license.o

CLASS_OBJECTS	= $(O_OBJECTS:%.o=%.class)

# This entry is used by QNC, for building bigloo++. Mainly Christian
# defines is own main entry point. This list does not contains the
# init/main.o file
O_OBJECTS_NOMAIN= $(O_OBJECTS_NOINIT) $(INIT_OBJ_NOMAIN)

C_OBJECTS	= $(O_OBJECTS:%.o=%.c)

SOURCE_FILES	= $(O_OBJECTS:%.o=%.scm)

POPULATION	= $(SOURCE_FILES) \
		  Ast/node.sch Ast/unit.sch Ast/var.sch Ast/nodetype.sch \
		  Engine/pass.sch \
		  Expand/expander.sch \
		  Tools/location.sch Tools/fprint.sch Tools/verbose.sch \
		  Tools/trace.sch \
		  Tvector/tvector.sch Tvector/tvectortype.sch \
		  Type/coercer.sch Type/type.sch \
                  Init/pass-args-parse.sch \
		  Module/libinfo.sch Module/impuse.sch Module/module.sch \
                  Module/java.sch \
                  BackEnd/backend.sch BackEnd/cvm.sch BackEnd/bvm.sch \
                  BackEnd/jvm_class.sch \
                  Object/class.sch Object/slots.sch \
                  Inline/inline.sch Inline/size.sch Inline/variant.sch \
                  Foreign/ctype.sch Foreign/jtype.sch \
                  Dataflow/walk.sch \
                  Initflow/walk.sch \
                  Globalize/ginfo.sch \
                  Cnst/node.sch \
                  Cfa/cinfo.sch Cfa/cinfo2.sch Cfa/cinfo3.sch \
                  Cfa/specialize.sch \
                  Integrate/iinfo.sch \
                  Tailc/walk.sch \
                  Cgen/cop.sch \
                  Callcc/walk.sch \
                  Effect/cgraph.sch \
                  Jas/classfile.sch \
                  SawMill/defs.sch SawMill/regalloc.sch SawMill/node2rtl.sch \
                  SawMill/collapse.sch SawMill/remove.sch \
                  SawMill/InlineReturn.sch SawMill/blockorder.sch \
                  SawMill/expr.sch SawMill/regset.sch SawMill/bbv-types.sch \
                  SawMill/bbv-interval.sch SawC/code.sch \
                  SawJvm/funcall.sch SawJvm/code.sch SawJvm/names.sch \
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
jvm: .jfile
	$(MAKE) jvm-sans-jfile

jvm-sans-jfile: $(CLASS_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(EFLAGS) -jvm -jvm-jar -jvm-java $(JAVA) \
             -jvm-shell $(JSHELL) \
             -jvm-classpath $(CLASSPATH) -jvm-jarpath $(JARPATH) \
             $(BDISTFLAGS) -o $(BIGLOO_DEST).jvm $(CLASS_OBJECTS)
	@ echo "$(BIGLOO_DEST).jvm done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: Tools/date.scm Tools/license.scm
	@ if [ `pwd` = $(BOOTDIR)/comptime ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	$(MAKE) .afile
	$(MAKE) .jfile
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

bigloo.plain: Tools/license.scm .afile
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
$(BIGLOO_DEST)$(EXE_SUFFIX): .afile .etags $(O_OBJECTS) $(G_OBJECTS)
	@ echo "Linking..."
	if [ "$(LDRPATHOPT) " != " " ]; then \
	   $(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) \
	          -copt "$(EXTRA_LD_OPT) $(LDRPATHOPT)$(RPATH)" \
	          -o $(BIGLOO_DEST).aux \
	          $(O_OBJECTS) $(G_OBJECTS); \
        else \
	   $(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) \
	          -copt "$(EXTRA_LD_OPT) $(LDRPATHOPT)$(RPATH)" \
	          -o $(BIGLOO_DEST).aux \
	          $(O_OBJECTS) $(G_OBJECTS); \
        fi
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
$(BIGLOO_DEST).$(NEW_SUFFIX): .afile .etags $(O_OBJECTS) $(G_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) -o $@ $(O_OBJECTS) $(G_OBJECTS)
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    bigloo.$(SHAREDSUFFIX)                                           */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST).$(SHAREDSUFFIX): $(O_OBJECTS) $(G_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(BBFLAGS) $(BFLAGS) $(EFLAGS) -o $@ \
                    $(O_OBJECTS) $(G_OBJECTS) \
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
dist-bigloo.exe: $(C_OBJECTS) Tools/date.c Tools/license.c
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

ude: Tools/date.scm Tools/license.scm .afile .jfile .etags
	$(MAKE) dep

.afile: $(SOURCE_FILES) Tools/license.scm
	$(BGLAFILE) -o .afile $(SOURCE_FILES) Tools/license.scm

.jfile: .afile
	$(BGLJFILE) -o .jfile $(SOURCE_FILES) Tools/license.scm

.etags: .afile
	$(BGLTAGS) --define-var doc-define -o .etags $(SOURCE_FILES) Tools/license.scm

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
	$(BGLDEPEND) -fno-mco $(SOURCE_FILES) >> /tmp/Makefile.aux
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
	touch .afile
	touch .jfile
	if [ "$(SHRD_COMP)" = "yes" -a -f $(BOOTLIBDIR)/lib$(LIBRARYNAME)_s-$(RELEASE).$(SHAREDSUFFIX) ]; then \
	  $(MAKE) doboot.shared SCMOBJSUFFIX="" COBJSUFFIX=%.o; \
        else \
	  $(MAKE) doboot.static SCMOBJSUFFIX="" COBJSUFFIX=%.o; \
	fi
	@ echo "$(BIGLOO_DEST)$(EXE_SUFFIX) done..."

doboot.shared: $(O_OBJECTS) $(G_OBJECTS)
	$(CC) $(EXTRA_LD_OPT) $(CFLAGS) -o $(BIGLOO_DEST)$(EXE_SUFFIX) $(O_OBJECTS) $(G_OBJECTS) -L$(BOOTLIBDIR) $(LDCOMPLIBS_SHARED) $(LDCOMPLIBS)

doboot.static: $(O_OBJECTS) $(G_OBJECTS)
	 $(CC) $(EXTRA_LD_OPT) $(CFLAGS) -o $(BIGLOO_DEST)$(EXE_SUFFIX) $(O_OBJECTS) $(G_OBJECTS) $(LDCOMPLIBS_STATIC) $(LDCOMPLIBS)
	-$(STRIP) $(BIGLOO_DEST)$(EXE_SUFFIX)

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
           cat $(BOOTBINDIR)/bigloo.sh | \
              sed -e "s|$(BOOTBINDIR)|$(BINDIR)|g" \
                  -e "s|$(BOOTLIBDIR)|$(LIBDIR)/$(FILDIR)|g" \
                  -e "s|(BIGLOO)|$(BINDIR)/bigloo|g" \
              > $(DESTDIR)$(BINDIR)/bigloo.sh && \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bigloo.sh; \
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
	- $(RM) -f .afile
	- $(RM) -f .jfile
	- $(RM) -f .etags
	find . \( -name '*[~%]'                     \
                       -o -name '.??*[~%]'          \
                       -o -name '#*#'               \
                       -o -name '?*#'               \
                       -o -name \*core \)           \
                     -type f -exec $(RM) {} \;   

clean-c:
	- $(RM) -f $(O_OBJECTS) $(G_OBJECTS)

clean-jvm:
	- $(RM) -f $(CLASS_OBJECTS)
	- $(RM) -f $(BIGLOO_DEST).jvm
	- $(RM) -f $(BIGLOO_DEST).jar

clean: clean-common clean-c clean-jvm
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
	- $(RM) -f $(O_OBJECTS) $(G_OBJECTS)
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
pop: 
	@ echo $(POPULATION:%=comptime/%)

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
Engine/param.o: $(BOOTLIBDIR)/bigloo_config.h ../configure
Module/class.o: $(BOOTDIR)/runtime/Llib/object.sch

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
./Globalize/integration.o ./Globalize/integration.class: Tools/trace.sch 
./SawJvm/funcall.o ./SawJvm/funcall.class: SawJvm/funcall.sch 
Effect/effect.o Effect/effect.class: Ast/node.sch 
./Expand/case.o ./Expand/case.class: Tools/trace.sch 
Globalize/gloclo.o Globalize/gloclo.class: Tools/trace.sch 
Inline/recursion.o Inline/recursion.class: Tools/trace.sch               \
      Tools/verbose.sch 
Type/typeof.o Type/typeof.class: Tvector/tvector.sch 
./Init/parse_args.o ./Init/parse_args.class: Tools/trace.sch             \
      Engine/pass.sch Init/pass-args-parse.sch 
./Tools/error.o ./Tools/error.class: Tools/location.sch Ast/node.sch 
Sync/failsafe.o Sync/failsafe.class: Tools/trace.sch Tools/location.sch 
SawMill/expr.o SawMill/expr.class: SawMill/expr.sch 
Integrate/u.o Integrate/u.class: Tools/trace.sch 
Ast/build.o Ast/build.class: Engine/pass.sch Ast/node.sch 
./Effect/effect.o ./Effect/effect.class: Ast/node.sch 
./Object/nil.o ./Object/nil.class: Tools/trace.sch 
Tools/progn.o Tools/progn.class: Tools/location.sch 
Expand/expander.o Expand/expander.class: Expand/expander.sch 
./Integrate/kk.o ./Integrate/kk.class: Tools/trace.sch 
Ast/remove.o Ast/remove.class: Ast/node.sch 
./Ast/build.o ./Ast/build.class: Engine/pass.sch Ast/node.sch 
./Cfa/iterate.o ./Cfa/iterate.class: Tools/trace.sch 
./Integrate/iinfo.o ./Integrate/iinfo.class: Integrate/iinfo.sch 
./Ast/toccur.o ./Ast/toccur.class: Ast/node.sch 
./Sync/failsafe.o ./Sync/failsafe.class: Tools/trace.sch                 \
      Tools/location.sch 
./Bdb/walk.o ./Bdb/walk.class: Engine/pass.sch 
Cfa/arithmetic.o Cfa/arithmetic.class: Tools/trace.sch 
SawJvm/code.o SawJvm/code.class: SawJvm/code.sch Tools/location.sch 
Reduce/cse.o Reduce/cse.class: Tools/trace.sch 
BackEnd/cplib.o BackEnd/cplib.class: Tvector/tvector.sch 
./Inline/variant.o ./Inline/variant.class: Tools/trace.sch               \
      Inline/variant.sch 
./Cfa/cfa.o ./Cfa/cfa.class: Tools/trace.sch 
./Cfa/box.o ./Cfa/box.class: Tools/trace.sch 
./Reduce/copy.o ./Reduce/copy.class: Tools/trace.sch 
Module/foreign.o Module/foreign.class: Ast/unit.sch Tools/trace.sch 
./Foreign/cstruct.o ./Foreign/cstruct.class: Tools/trace.sch 
./Coerce/app.o ./Coerce/app.class: Tools/trace.sch 
./BackEnd/cplib.o ./BackEnd/cplib.class: Tvector/tvector.sch 
Cfa/funcall.o Cfa/funcall.class: Tools/trace.sch 
./Ast/var.o ./Ast/var.class: Ast/var.sch 
./Callcc/walk.o ./Callcc/walk.class: Engine/pass.sch Ast/node.sch        \
      Callcc/walk.sch 
Tvector/tvector.o Tvector/tvector.class: Tvector/tvectortype.sch 
Globalize/gn.o Globalize/gn.class: Tools/trace.sch 
Ast/apply.o Ast/apply.class: Ast/node.sch 
Ast/node.o Ast/node.class: Ast/nodetype.sch 
./Cfa/collect.o ./Cfa/collect.class: Tools/trace.sch 
Object/class.o Object/class.class: Object/class.sch 
Integrate/definition.o Integrate/definition.class: Tools/trace.sch       \
      Tools/verbose.sch 
Cfa/loose.o Cfa/loose.class: Tools/trace.sch 
./Ast/apply.o ./Ast/apply.class: Ast/node.sch 
./Effect/walk2.o ./Effect/walk2.class: Engine/pass.sch Tools/trace.sch 
Globalize/loc2glo.o Globalize/loc2glo.class: Tools/trace.sch 
./Ast/let.o ./Ast/let.class: Ast/node.sch Tools/trace.sch                \
      Tools/location.sch 
Tools/location.o Tools/location.class: Tools/location.sch 
Integrate/g.o Integrate/g.class: Tools/trace.sch 
./Cfa/loose.o ./Cfa/loose.class: Tools/trace.sch 
Object/coercion.o Object/coercion.class: Tools/trace.sch 
./Globalize/loc2glo.o ./Globalize/loc2glo.class: Tools/trace.sch 
./Cfa/show.o ./Cfa/show.class: Tools/trace.sch 
Expand/eps.o Expand/eps.class: Expand/expander.sch Tools/trace.sch       \
      Engine/pass.sch Ast/unit.sch 
Module/type.o Module/type.class: Ast/unit.sch Tools/trace.sch 
Ast/sexp.o Ast/sexp.class: Tools/trace.sch Tools/location.sch 
./Module/include.o ./Module/include.class: Ast/unit.sch 
Object/classgen.o Object/classgen.class: Engine/pass.sch 
Cfa/closure.o Cfa/closure.class: Tools/trace.sch 
./Integrate/node.o ./Integrate/node.class: Tools/trace.sch 
./Ast/unit.o ./Ast/unit.class: Ast/unit.sch Ast/node.sch Tools/trace.sch 
./Abound/walk.o ./Abound/walk.class: Engine/pass.sch Ast/node.sch        \
      Tools/location.sch 
./Reduce/walk.o ./Reduce/walk.class: Engine/pass.sch 
./SawMill/remove.o ./SawMill/remove.class: SawMill/remove.sch 
Integrate/a.o Integrate/a.class: Tools/trace.sch 
Cfa/approx.o Cfa/approx.class: Tools/trace.sch 
Ast/occur.o Ast/occur.class: Ast/node.sch 
SawMill/regset.o SawMill/regset.class: SawMill/regset.sch 
Reduce/ftypec.o Reduce/ftypec.class: Tools/trace.sch 
Integrate/kaptured.o Integrate/kaptured.class: Tools/trace.sch 
./Expand/let.o ./Expand/let.class: Ast/node.sch Tools/location.sch 
SawMill/regalloc.o SawMill/regalloc.class: Tools/trace.sch               \
      SawMill/regalloc.sch 
Effect/cgraph.o Effect/cgraph.class: Ast/node.sch Tools/trace.sch        \
      Effect/cgraph.sch 
./Ast/occur.o ./Ast/occur.class: Ast/node.sch 
./Ast/substitute.o ./Ast/substitute.class: Tools/trace.sch 
./Reduce/ftypec.o ./Reduce/ftypec.class: Tools/trace.sch 
Inline/app.o Inline/app.class: Tools/trace.sch 
./Ast/exit.o ./Ast/exit.class: Ast/node.sch Tools/trace.sch 
./Effect/cgraph.o ./Effect/cgraph.class: Ast/node.sch Tools/trace.sch    \
      Effect/cgraph.sch 
./Bdb/spread_obj.o ./Bdb/spread_obj.class: Engine/pass.sch Ast/node.sch 
./SawJvm/compile.o ./SawJvm/compile.class: Tvector/tvector.sch           \
      Tools/trace.sch 
BackEnd/c_proto.o BackEnd/c_proto.class: Tvector/tvector.sch             \
      Tools/fprint.sch 
Cnst/node.o Cnst/node.class: Tools/trace.sch Tvector/tvector.sch         \
      Cnst/node.sch 
User/user.o User/user.class: Engine/pass.sch Ast/unit.sch 
Ast/dump.o Ast/dump.class: Tvector/tvector.sch Ast/node.sch 
SawJvm/funcall.o SawJvm/funcall.class: SawJvm/funcall.sch 
./Cgen/cop.o ./Cgen/cop.class: Cgen/cop.sch 
Init/extend.o Init/extend.class: Engine/pass.sch 
Effect/walk.o Effect/walk.class: Engine/pass.sch Tools/trace.sch 
Reduce/cond.o Reduce/cond.class: Tools/trace.sch 
Module/class.o Module/class.class: Ast/unit.sch Tools/trace.sch 
Heap/make.o Heap/make.class: Engine/pass.sch Tools/trace.sch 
./Bdb/bdb_emit.o ./Bdb/bdb_emit.class: Ast/node.sch Ast/unit.sch         \
      Tools/location.sch 
./Cnst/node.o ./Cnst/node.class: Tools/trace.sch Tvector/tvector.sch     \
      Cnst/node.sch 
./User/user.o ./User/user.class: Engine/pass.sch Ast/unit.sch 
Read/inline.o Read/inline.class: Tools/trace.sch Ast/unit.sch 
./Globalize/clocto.o ./Globalize/clocto.class: Tools/trace.sch 
Object/nil.o Object/nil.class: Tools/trace.sch 
Cgen/cgen.o Cgen/cgen.class: Tools/trace.sch Tools/location.sch 
./Heap/make.o ./Heap/make.class: Engine/pass.sch Tools/trace.sch 
./Ast/init.o ./Ast/init.class: Engine/pass.sch Ast/unit.sch              \
      Tools/trace.sch 
Cfa/walk.o Cfa/walk.class: Engine/pass.sch Tools/trace.sch 
./Ast/labels.o ./Ast/labels.class: Ast/node.sch Tools/trace.sch          \
      Tools/location.sch 
./SawMill/defs.o ./SawMill/defs.class: SawMill/defs.sch 
SawMill/InlineReturn.o SawMill/InlineReturn.class:                       \
      SawMill/InlineReturn.sch 
Ast/toccur.o Ast/toccur.class: Ast/node.sch 
./Inline/size.o ./Inline/size.class: Ast/node.sch Inline/size.sch 
./Expand/exit.o ./Expand/exit.class: Expand/expander.sch Tools/trace.sch \
      Tools/location.sch 
./Cgen/cgen.o ./Cgen/cgen.class: Tools/trace.sch Tools/location.sch 
./Inline/recursion.o ./Inline/recursion.class: Tools/trace.sch           \
      Tools/verbose.sch 
./Inline/loop.o ./Inline/loop.class: Tools/trace.sch 
./Ast/check_init.o ./Ast/check_init.class: Ast/unit.sch 
./Globalize/gloclo.o ./Globalize/gloclo.class: Tools/trace.sch 
Cfa/pair.o Cfa/pair.class: Tools/trace.sch 
./Ast/glo_def.o ./Ast/glo_def.class: Tools/trace.sch 
./SawMill/expr.o ./SawMill/expr.class: SawMill/expr.sch 
BackEnd/jvm.o BackEnd/jvm.class: Engine/pass.sch 
Integrate/loc2glo.o Integrate/loc2glo.class: Tools/trace.sch 
Coerce/pproto.o Coerce/pproto.class: Tools/verbose.sch 
Inline/variant.o Inline/variant.class: Tools/trace.sch                   \
      Inline/variant.sch 
Type/coercion.o Type/coercion.class: Type/coercer.sch Tools/trace.sch 
./Coerce/funcall.o ./Coerce/funcall.class: Tools/trace.sch               \
      Tools/location.sch 
./Coerce/pproto.o ./Coerce/pproto.class: Tools/verbose.sch 
./Integrate/loc2glo.o ./Integrate/loc2glo.class: Tools/trace.sch 
Globalize/globalize.o Globalize/globalize.class: Tools/trace.sch         \
      Tools/verbose.sch 
Coerce/app.o Coerce/app.class: Tools/trace.sch 
./Write/expanded.o ./Write/expanded.class: Ast/unit.sch 
./Type/coercion.o ./Type/coercion.class: Type/coercer.sch                \
      Tools/trace.sch 
Cfa/tvector.o Cfa/tvector.class: Tools/trace.sch Ast/unit.sch            \
      Tvector/tvector.sch 
Hgen/walk.o Hgen/walk.class: Engine/pass.sch 
Ast/check_sharing.o Ast/check_sharing.class: Ast/unit.sch                \
      Engine/pass.sch 
BackEnd/backend.o BackEnd/backend.class: BackEnd/backend.sch 
./Coerce/convert.o ./Coerce/convert.class: Tools/trace.sch               \
      Tools/location.sch Type/coercer.sch 
./Integrate/walk.o ./Integrate/walk.class: Engine/pass.sch 
Expand/if.o Expand/if.class: Tools/trace.sch 
./Engine/compiler.o ./Engine/compiler.class: Ast/unit.sch                \
      Engine/pass.sch 
./Ast/find_gdefs.o ./Ast/find_gdefs.class: Tools/trace.sch Ast/node.sch 
./Ast/check_sharing.o ./Ast/check_sharing.class: Ast/unit.sch            \
      Engine/pass.sch 
./Hgen/walk.o ./Hgen/walk.class: Engine/pass.sch 
Inline/walk.o Inline/walk.class: Engine/pass.sch Tools/trace.sch         \
      Ast/node.sch 
./Globalize/new_body.o ./Globalize/new_body.class: Tools/trace.sch 
BackEnd/bvm.o BackEnd/bvm.class: BackEnd/bvm.sch 
./Module/statexp.o ./Module/statexp.class: Ast/unit.sch 
./Expand/if.o ./Expand/if.class: Tools/trace.sch 
./Cfa/struct.o ./Cfa/struct.class: Tools/trace.sch 
SawJvm/names.o SawJvm/names.class: SawJvm/names.sch 
SawMill/collapse.o SawMill/collapse.class: SawMill/collapse.sch 
Globalize/ginfo.o Globalize/ginfo.class: Ast/node.sch                    \
      Globalize/ginfo.sch 
Module/module.o Module/module.class: Ast/unit.sch Engine/pass.sch        \
      Module/module.sch 
./Ast/node.o ./Ast/node.class: Ast/nodetype.sch 
./Module/eval.o ./Module/eval.class: Ast/unit.sch Ast/node.sch           \
      Module/libinfo.sch 
./Globalize/gn.o ./Globalize/gn.class: Tools/trace.sch 
./BackEnd/cvm.o ./BackEnd/cvm.class: BackEnd/cvm.sch 
Fail/walk.o Fail/walk.class: Engine/pass.sch Tools/location.sch          \
      Tools/trace.sch Ast/node.sch 
Cc/exec.o Cc/exec.class: Tools/trace.sch 
./Object/class.o ./Object/class.class: Object/class.sch 
Module/include.o Module/include.class: Ast/unit.sch 
./Module/module.o ./Module/module.class: Ast/unit.sch Engine/pass.sch    \
      Module/module.sch 
./Integrate/definition.o ./Integrate/definition.class: Tools/trace.sch   \
      Tools/verbose.sch 
Integrate/node.o Integrate/node.class: Tools/trace.sch 
Integrate/cto.o Integrate/cto.class: Tools/trace.sch 
Inline/simple.o Inline/simple.class: Tools/trace.sch Tools/verbose.sch 
Ast/app.o Ast/app.class: Ast/node.sch Tools/trace.sch 
./Fail/walk.o ./Fail/walk.class: Engine/pass.sch Tools/location.sch      \
      Tools/trace.sch Ast/node.sch 
./Cfa/app.o ./Cfa/app.class: Tools/trace.sch 
Object/predicate.o Object/predicate.class: Tools/trace.sch 
Integrate/ctn.o Integrate/ctn.class: Tools/trace.sch 
Module/java.o Module/java.class: Ast/unit.sch Tools/trace.sch            \
      Module/java.sch 
./Expand/initial.o ./Expand/initial.class: Tools/location.sch 
./Cnst/initialize.o ./Cnst/initialize.class: Tools/trace.sch             \
      Ast/unit.sch 
SawMill/remove.o SawMill/remove.class: SawMill/remove.sch 
Cnst/walk.o Cnst/walk.class: Engine/pass.sch Tools/trace.sch 
./Inline/simple.o ./Inline/simple.class: Tools/trace.sch                 \
      Tools/verbose.sch 
./Integrate/cto.o ./Integrate/cto.class: Tools/trace.sch 
./Ast/sexp.o ./Ast/sexp.class: Tools/trace.sch Tools/location.sch 
./Integrate/ctn.o ./Integrate/ctn.class: Tools/trace.sch 
Expand/let.o Expand/let.class: Ast/node.sch Tools/location.sch 
./Object/creator.o ./Object/creator.class: Tools/trace.sch 
./Cnst/walk.o ./Cnst/walk.class: Engine/pass.sch Tools/trace.sch 
Type/type.o Type/type.class: Type/type.sch 
Ast/substitute.o Ast/substitute.class: Tools/trace.sch 
SawMill/node2rtl.o SawMill/node2rtl.class: Tools/location.sch            \
      SawMill/node2rtl.sch 
Foreign/cptr.o Foreign/cptr.class: Tools/trace.sch 
./Integrate/free.o ./Integrate/free.class: Tools/trace.sch 
Tools/shape.o Tools/shape.class: Ast/node.sch Tvector/tvector.sch 
Coerce/coerce.o Coerce/coerce.class: Tools/trace.sch 
./Type/type.o ./Type/type.class: Type/type.sch 
SawJvm/compile.o SawJvm/compile.class: Tvector/tvector.sch               \
      Tools/trace.sch 
Bdb/spread_obj.o Bdb/spread_obj.class: Engine/pass.sch Ast/node.sch 
Cgen/capp.o Cgen/capp.class: Tools/trace.sch 
./Cnst/alloc.o ./Cnst/alloc.class: Tvector/tvector.sch 
./Coerce/coerce.o ./Coerce/coerce.class: Tools/trace.sch 
./SawMill/regalloc.o ./SawMill/regalloc.class: Tools/trace.sch           \
      SawMill/regalloc.sch 
Tvector/cnst.o Tvector/cnst.class: Tvector/tvector.sch 
./Bdb/bdb_initialize.o ./Bdb/bdb_initialize.class: Tools/trace.sch       \
      Ast/unit.sch 
Reduce/same.o Reduce/same.class: Tools/trace.sch 
./Cgen/capp.o ./Cgen/capp.class: Tools/trace.sch 
./Coerce/walk.o ./Coerce/walk.class: Engine/pass.sch 
./SawMill/blockorder.o ./SawMill/blockorder.class:                       \
      SawMill/blockorder.sch 
Reduce/typec.o Reduce/typec.class: Tools/trace.sch 
./Globalize/node.o ./Globalize/node.class: Tools/trace.sch 
Expand/case.o Expand/case.class: Tools/trace.sch 
Read/load.o Read/load.class: Tools/trace.sch 
./Ast/dump.o ./Ast/dump.class: Tvector/tvector.sch Ast/node.sch 
./Reduce/1occ.o ./Reduce/1occ.class: Tools/trace.sch 
Init/parse_args.o Init/parse_args.class: Tools/trace.sch Engine/pass.sch \
      Init/pass-args-parse.sch 
Tools/error.o Tools/error.class: Tools/location.sch Ast/node.sch 
Cfa/setup.o Cfa/setup.class: Tools/trace.sch 
Ast/labels.o Ast/labels.class: Ast/node.sch Tools/trace.sch              \
      Tools/location.sch 
./Cfa/vector.o ./Cfa/vector.class: Tools/trace.sch 
./Module/class.o ./Module/class.class: Ast/unit.sch Tools/trace.sch 
./Read/load.o ./Read/load.class: Tools/trace.sch 
./Module/alibrary.o ./Module/alibrary.class: Module/libinfo.sch          \
      Ast/unit.sch 
./Prof/prof_emit.o ./Prof/prof_emit.class: Ast/node.sch Ast/unit.sch     \
      Tools/location.sch 
Cfa/cinfo.o Cfa/cinfo.class: Cfa/cinfo.sch 
Module/impuse.o Module/impuse.class: Ast/unit.sch Tools/trace.sch        \
      Module/impuse.sch 
Ast/check_init.o Ast/check_init.class: Ast/unit.sch 
./Cfa/setup.o ./Cfa/setup.class: Tools/trace.sch 
Integrate/iinfo.o Integrate/iinfo.class: Integrate/iinfo.sch 
Cfa/iterate.o Cfa/iterate.class: Tools/trace.sch 
Prof/unit.o Prof/unit.class: Engine/pass.sch Ast/unit.sch 
Coerce/apply.o Coerce/apply.class: Tools/trace.sch Tools/location.sch 
./Type/typeof.o ./Type/typeof.class: Tvector/tvector.sch 
./Module/impuse.o ./Module/impuse.class: Ast/unit.sch Tools/trace.sch    \
      Module/impuse.sch 
./Cfa/walk.o ./Cfa/walk.class: Engine/pass.sch Tools/trace.sch 
./Cfa/cinfo.o ./Cfa/cinfo.class: Cfa/cinfo.sch 
Coerce/funcall.o Coerce/funcall.class: Tools/trace.sch                   \
      Tools/location.sch 
./Integrate/u.o ./Integrate/u.class: Tools/trace.sch 
./SawMill/InlineReturn.o ./SawMill/InlineReturn.class:                   \
      SawMill/InlineReturn.sch 
Globalize/kapture.o Globalize/kapture.class: Tools/trace.sch 
./Tools/progn.o ./Tools/progn.class: Tools/location.sch 
./Expand/expander.o ./Expand/expander.class: Expand/expander.sch 
./Foreign/calias.o ./Foreign/calias.class: Tools/trace.sch 
./Prof/unit.o ./Prof/unit.class: Engine/pass.sch Ast/unit.sch 
Cfa/cfa.o Cfa/cfa.class: Tools/trace.sch 
Write/expanded.o Write/expanded.class: Ast/unit.sch 
./Cfa/pair.o ./Cfa/pair.class: Tools/trace.sch 
Reduce/copy.o Reduce/copy.class: Tools/trace.sch 
Cfa/box.o Cfa/box.class: Tools/trace.sch 
Cfa/procedure.o Cfa/procedure.class: Tools/trace.sch 
Write/ast.o Write/ast.class: Engine/pass.sch Ast/node.sch 
./Globalize/kapture.o ./Globalize/kapture.class: Tools/trace.sch 
Integrate/walk.o Integrate/walk.class: Engine/pass.sch 
Coerce/convert.o Coerce/convert.class: Tools/trace.sch                   \
      Tools/location.sch Type/coercer.sch 
Expand/assert.o Expand/assert.class: Tools/location.sch 
Foreign/cstruct.o Foreign/cstruct.class: Tools/trace.sch 
Callcc/walk.o Callcc/walk.class: Engine/pass.sch Ast/node.sch            \
      Callcc/walk.sch 
Ast/find_gdefs.o Ast/find_gdefs.class: Tools/trace.sch Ast/node.sch 
Ast/var.o Ast/var.class: Ast/var.sch 
./Write/ast.o ./Write/ast.class: Engine/pass.sch Ast/node.sch 
./Cfa/procedure.o ./Cfa/procedure.class: Tools/trace.sch 
./Cfa/specialize.o ./Cfa/specialize.class: Tools/trace.sch               \
      Cfa/specialize.sch 
./SawJvm/code.o ./SawJvm/code.class: SawJvm/code.sch Tools/location.sch 
Cfa/collect.o Cfa/collect.class: Tools/trace.sch 
Globalize/new_body.o Globalize/new_body.class: Tools/trace.sch 
./Expand/assert.o ./Expand/assert.class: Tools/location.sch 
Cfa/struct.o Cfa/struct.class: Tools/trace.sch 
Module/statexp.o Module/statexp.class: Ast/unit.sch 
Ast/let.o Ast/let.class: Ast/node.sch Tools/trace.sch Tools/location.sch 
./Cfa/funcall.o ./Cfa/funcall.class: Tools/trace.sch 
Object/slots.o Object/slots.class: Tools/trace.sch Object/slots.sch 
./Tvector/tvector.o ./Tvector/tvector.class: Tvector/tvectortype.sch 
BackEnd/c.o BackEnd/c.class: Engine/pass.sch Ast/unit.sch                \
      Tools/trace.sch 
Beta/walk.o Beta/walk.class: Engine/pass.sch Tools/trace.sch             \
      Ast/node.sch 
Globalize/integration28c.o Globalize/integration28c.class:               \
      Tools/trace.sch 
Expand/initial.o Expand/initial.class: Tools/location.sch 
./Cfa/cinfo3.o ./Cfa/cinfo3.class: Cfa/cinfo3.sch 
./SawJvm/names.o ./SawJvm/names.class: SawJvm/names.sch 
./SawMill/collapse.o ./SawMill/collapse.class: SawMill/collapse.sch 
./Beta/walk.o ./Beta/walk.class: Engine/pass.sch Tools/trace.sch         \
      Ast/node.sch 
./Cfa/cinfo2.o ./Cfa/cinfo2.class: Cfa/cinfo2.sch 
./BackEnd/c.o ./BackEnd/c.class: Engine/pass.sch Ast/unit.sch            \
      Tools/trace.sch 
Abound/walk.o Abound/walk.class: Engine/pass.sch Ast/node.sch            \
      Tools/location.sch 
./Object/java_access.o ./Object/java_access.class: Tools/trace.sch 
./Integrate/g.o ./Integrate/g.class: Tools/trace.sch 
Object/creator.o Object/creator.class: Tools/trace.sch 
Reduce/sbeta.o Reduce/sbeta.class: Tools/trace.sch 
Reduce/walk.o Reduce/walk.class: Engine/pass.sch 
./Object/coercion.o ./Object/coercion.class: Tools/trace.sch 
./Module/type.o ./Module/type.class: Ast/unit.sch Tools/trace.sch 
./Object/predicate.o ./Object/predicate.class: Tools/trace.sch 
./Globalize/walk.o ./Globalize/walk.class: Engine/pass.sch Ast/node.sch  \
      Tools/trace.sch 
Integrate/free.o Integrate/free.class: Tools/trace.sch 
./Object/classgen.o ./Object/classgen.class: Engine/pass.sch 
./Ast/check_type.o ./Ast/check_type.class: Ast/unit.sch Engine/pass.sch 
./Cfa/closure.o ./Cfa/closure.class: Tools/trace.sch 
Sync/node.o Sync/node.class: Tools/trace.sch Tools/location.sch 
Cnst/alloc.o Cnst/alloc.class: Tvector/tvector.sch 
Object/getter.o Object/getter.class: Tools/trace.sch 
Heap/restore.o Heap/restore.class: Engine/pass.sch 
./Integrate/a.o ./Integrate/a.class: Tools/trace.sch 
Bdb/bdb_initialize.o Bdb/bdb_initialize.class: Tools/trace.sch           \
      Ast/unit.sch 
Expand/lambda.o Expand/lambda.class: Tools/trace.sch 
Ast/glo_decl.o Ast/glo_decl.class: Tools/trace.sch 
./Foreign/cptr.o ./Foreign/cptr.class: Tools/trace.sch 
./SawMill/node2rtl.o ./SawMill/node2rtl.class: Tools/location.sch        \
      SawMill/node2rtl.sch 
./Sync/node.o ./Sync/node.class: Tools/trace.sch Tools/location.sch 
SawMill/blockorder.o SawMill/blockorder.class: SawMill/blockorder.sch 
./Object/getter.o ./Object/getter.class: Tools/trace.sch 
Globalize/node.o Globalize/node.class: Tools/trace.sch 
./Expand/lambda.o ./Expand/lambda.class: Tools/trace.sch 
Inline/inline.o Inline/inline.class: Ast/node.sch Tools/trace.sch        \
      Inline/inline.sch 
./Ast/pragma.o ./Ast/pragma.class: Ast/node.sch 
./Effect/feffect.o ./Effect/feffect.class: Tools/trace.sch 
Cfa/vector.o Cfa/vector.class: Tools/trace.sch 
./Ast/object.o ./Ast/object.class: Tools/trace.sch Tools/location.sch 
./Tvector/cnst.o ./Tvector/cnst.class: Tvector/tvector.sch 
Prof/prof_emit.o Prof/prof_emit.class: Ast/node.sch Ast/unit.sch         \
      Tools/location.sch 
./Inline/inline.o ./Inline/inline.class: Ast/node.sch Tools/trace.sch    \
      Inline/inline.sch 
./BackEnd/c_proto.o ./BackEnd/c_proto.class: Tvector/tvector.sch         \
      Tools/fprint.sch 
./Expand/map.o ./Expand/map.class: Tools/location.sch 
./Trace/walk.o ./Trace/walk.class: Engine/pass.sch Ast/node.sch          \
      Tools/location.sch 
Initflow/walk.o Initflow/walk.class: Engine/pass.sch Tools/trace.sch     \
      Initflow/walk.sch 
./Reduce/typec.o ./Reduce/typec.class: Tools/trace.sch 
Inline/size.o Inline/size.class: Ast/node.sch Inline/size.sch 
Expand/exit.o Expand/exit.class: Expand/expander.sch Tools/trace.sch     \
      Tools/location.sch 
./Tailc/walk.o ./Tailc/walk.class: Engine/pass.sch Tailc/walk.sch 
./Reduce/cond.o ./Reduce/cond.class: Tools/trace.sch 
./Init/extend.o ./Init/extend.class: Engine/pass.sch 
Integrate/kk.o Integrate/kk.class: Tools/trace.sch 
Inline/loop.o Inline/loop.class: Tools/trace.sch 
./Read/inline.o ./Read/inline.class: Tools/trace.sch Ast/unit.sch 
./Module/library.o ./Module/library.class: Ast/unit.sch 
./Initflow/walk.o ./Initflow/walk.class: Engine/pass.sch Tools/trace.sch \
      Initflow/walk.sch 
./Globalize/free.o ./Globalize/free.class: Tools/trace.sch 
Ast/glo_def.o Ast/glo_def.class: Tools/trace.sch 
SawC/compile.o SawC/compile.class: Engine/pass.sch Ast/unit.sch          \
      Tools/trace.sch 
Bdb/walk.o Bdb/walk.class: Engine/pass.sch 
Foreign/calias.o Foreign/calias.class: Tools/trace.sch 
Foreign/jtype.o Foreign/jtype.class: Tools/trace.sch Foreign/jtype.sch 
./Coerce/apply.o ./Coerce/apply.class: Tools/trace.sch                   \
      Tools/location.sch 
Jas/classfile.o Jas/classfile.class: Jas/classfile.sch 
./Ast/remove.o ./Ast/remove.class: Ast/node.sch 
./Foreign/jtype.o ./Foreign/jtype.class: Tools/trace.sch                 \
      Foreign/jtype.sch 
Cfa/specialize.o Cfa/specialize.class: Tools/trace.sch                   \
      Cfa/specialize.sch 
./Cfa/arithmetic.o ./Cfa/arithmetic.class: Tools/trace.sch 
./Jas/classfile.o ./Jas/classfile.class: Jas/classfile.sch 
Engine/compiler.o Engine/compiler.class: Ast/unit.sch Engine/pass.sch 
./Reduce/cse.o ./Reduce/cse.class: Tools/trace.sch 
Integrate/let_fun.o Integrate/let_fun.class: Tools/trace.sch 
./Globalize/globalize.o ./Globalize/globalize.class: Tools/trace.sch     \
      Tools/verbose.sch 
Foreign/ctype.o Foreign/ctype.class: Tools/trace.sch Foreign/ctype.sch 
./Cfa/tvector.o ./Cfa/tvector.class: Tools/trace.sch Ast/unit.sch        \
      Tvector/tvector.sch 
BackEnd/cvm.o BackEnd/cvm.class: BackEnd/cvm.sch 
Module/eval.o Module/eval.class: Ast/unit.sch Ast/node.sch               \
      Module/libinfo.sch 
./BackEnd/backend.o ./BackEnd/backend.class: BackEnd/backend.sch 
./Module/foreign.o ./Module/foreign.class: Ast/unit.sch Tools/trace.sch 
./Integrate/let_fun.o ./Integrate/let_fun.class: Tools/trace.sch 
Cgen/emit_cop.o Cgen/emit_cop.class: Tools/location.sch Tools/fprint.sch \
      Tools/trace.sch 
./Foreign/ctype.o ./Foreign/ctype.class: Tools/trace.sch                 \
      Foreign/ctype.sch 
Effect/spread.o Effect/spread.class: Ast/node.sch 
Cfa/cinfo3.o Cfa/cinfo3.class: Cfa/cinfo3.sch 
./Inline/walk.o ./Inline/walk.class: Engine/pass.sch Tools/trace.sch     \
      Ast/node.sch 
Cfa/cinfo2.o Cfa/cinfo2.class: Cfa/cinfo2.sch 
Cfa/show.o Cfa/show.class: Tools/trace.sch 
Cfa/app.o Cfa/app.class: Tools/trace.sch 
Expand/define.o Expand/define.class: Expand/expander.sch Tools/trace.sch 
./Cgen/emit_cop.o ./Cgen/emit_cop.class: Tools/location.sch              \
      Tools/fprint.sch Tools/trace.sch 
./BackEnd/bvm.o ./BackEnd/bvm.class: BackEnd/bvm.sch 
BackEnd/jvm_class.o BackEnd/jvm_class.class: BackEnd/jvm_class.sch 
Object/java_access.o Object/java_access.class: Tools/trace.sch 
Cnst/initialize.o Cnst/initialize.class: Tools/trace.sch Ast/unit.sch 
Expand/mvalue.o Expand/mvalue.class: Expand/expander.sch Tools/trace.sch 
Ast/alphatize.o Ast/alphatize.class: Ast/node.sch Tools/location.sch 
./Object/slots.o ./Object/slots.class: Tools/trace.sch Object/slots.sch 
./Effect/spread.o ./Effect/spread.class: Ast/node.sch 
./Globalize/ginfo.o ./Globalize/ginfo.class: Ast/node.sch                \
      Globalize/ginfo.sch 
SawC/code.o SawC/code.class: SawC/code.sch Tools/trace.sch               \
      Tools/location.sch 
./Expand/define.o ./Expand/define.class: Expand/expander.sch             \
      Tools/trace.sch 
./Cc/exec.o ./Cc/exec.class: Tools/trace.sch 
Globalize/walk.o Globalize/walk.class: Engine/pass.sch Ast/node.sch      \
      Tools/trace.sch 
Ast/unit.o Ast/unit.class: Ast/unit.sch Ast/node.sch Tools/trace.sch 
./Tools/location.o ./Tools/location.class: Tools/location.sch 
./Ast/alphatize.o ./Ast/alphatize.class: Ast/node.sch Tools/location.sch 
./Expand/mvalue.o ./Expand/mvalue.class: Expand/expander.sch             \
      Tools/trace.sch 
./Globalize/integration28c.o ./Globalize/integration28c.class:           \
      Tools/trace.sch 
./BackEnd/jvm_class.o ./BackEnd/jvm_class.class: BackEnd/jvm_class.sch 
Ast/check_type.o Ast/check_type.class: Ast/unit.sch Engine/pass.sch 
./Ast/app.o ./Ast/app.class: Ast/node.sch Tools/trace.sch 
./SawC/code.o ./SawC/code.class: SawC/code.sch Tools/trace.sch           \
      Tools/location.sch 
./Expand/eps.o ./Expand/eps.class: Expand/expander.sch Tools/trace.sch   \
      Engine/pass.sch Ast/unit.sch 
./Module/java.o ./Module/java.class: Ast/unit.sch Tools/trace.sch        \
      Module/java.sch 
./Reduce/sbeta.o ./Reduce/sbeta.class: Tools/trace.sch 
Ast/exit.o Ast/exit.class: Ast/node.sch Tools/trace.sch 
./Cfa/approx.o ./Cfa/approx.class: Tools/trace.sch 
Dataflow/walk.o Dataflow/walk.class: Engine/pass.sch Tools/trace.sch     \
      Dataflow/walk.sch 
./Integrate/kaptured.o ./Integrate/kaptured.class: Tools/trace.sch 
./SawMill/regset.o ./SawMill/regset.class: SawMill/regset.sch 
Effect/feffect.o Effect/feffect.class: Tools/trace.sch 
Coerce/walk.o Coerce/walk.class: Engine/pass.sch 
Ast/pragma.o Ast/pragma.class: Ast/node.sch 
./Tools/shape.o ./Tools/shape.class: Ast/node.sch Tvector/tvector.sch 
./Heap/restore.o ./Heap/restore.class: Engine/pass.sch 
Cgen/cop.o Cgen/cop.class: Cgen/cop.sch 
Type/tenv.o Type/tenv.class: Tools/trace.sch Type/coercer.sch 
Module/option.o Module/option.class: Ast/unit.sch 
Ast/object.o Ast/object.class: Tools/trace.sch Tools/location.sch 
./Ast/glo_decl.o ./Ast/glo_decl.class: Tools/trace.sch 
./Dataflow/walk.o ./Dataflow/walk.class: Engine/pass.sch Tools/trace.sch \
      Dataflow/walk.sch 
Bdb/bdb_emit.o Bdb/bdb_emit.class: Ast/node.sch Ast/unit.sch             \
      Tools/location.sch 
Reduce/1occ.o Reduce/1occ.class: Tools/trace.sch 
Trace/walk.o Trace/walk.class: Engine/pass.sch Ast/node.sch              \
      Tools/location.sch 
Expand/map.o Expand/map.class: Tools/location.sch 
./Inline/app.o ./Inline/app.class: Tools/trace.sch 
Globalize/clocto.o Globalize/clocto.class: Tools/trace.sch 
Globalize/integration.o Globalize/integration.class: Tools/trace.sch 
./Type/tenv.o ./Type/tenv.class: Tools/trace.sch Type/coercer.sch 
./Module/option.o ./Module/option.class: Ast/unit.sch 
Tailc/walk.o Tailc/walk.class: Engine/pass.sch Tailc/walk.sch 
Module/alibrary.o Module/alibrary.class: Module/libinfo.sch Ast/unit.sch 
Ast/init.o Ast/init.class: Engine/pass.sch Ast/unit.sch Tools/trace.sch 
./Reduce/same.o ./Reduce/same.class: Tools/trace.sch 
SawMill/defs.o SawMill/defs.class: SawMill/defs.sch 
Globalize/free.o Globalize/free.class: Tools/trace.sch 
Module/library.o Module/library.class: Ast/unit.sch 

#bdepend stop
