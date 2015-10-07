#*=====================================================================*/
#*    serrano/prgm/project/bigloo/gmp/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 15:43:12 1998                          */
#*    Last change :  Wed Oct  7 08:36:57 2015 (serrano)                */
#*    Copyright   :  1998-2015 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The small Makefile to handle custom GMP libraries.               */
#*=====================================================================*/
do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config
include ../Makefile.misc

POPULATION = Makefile configure-gmp install-gmp

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
CCFLAGS = $(CFLAGS) $(CGMPFLAGS) $(GMPOFLAGS) $(CPICFLAGS) -I $(BOOTLIBDIR)
GMP = $(CUSTOMGMP)
OUTDIR = $(GMP)/.libs

#* OBJDIR = objs                                                       */
#* OBJECTS_5_0_2 = .libs/assert.o .libs/compat.o .libs/errno.o .libs/extract-dbl.o .libs/invalid.o .libs/memory.o .libs/mp_bpl.o .libs/mp_clz_tab.o .libs/mp_dv_tab.o .libs/mp_minv_tab.o .libs/mp_get_fns.o .libs/mp_set_fns.o .libs/rand.o .libs/randclr.o .libs/randdef.o .libs/randiset.o .libs/randlc2s.o .libs/randlc2x.o .libs/randmt.o .libs/randmts.o .libs/rands.o .libs/randsd.o .libs/randsdui.o .libs/randbui.o .libs/randmui.o .libs/version.o .libs/nextprime.o .libs/tal-reent.o mpf/.libs/init.o mpf/.libs/init2.o mpf/.libs/inits.o mpf/.libs/set.o mpf/.libs/set_ui.o mpf/.libs/set_si.o mpf/.libs/set_str.o mpf/.libs/set_d.o mpf/.libs/set_z.o mpf/.libs/iset.o mpf/.libs/iset_ui.o mpf/.libs/iset_si.o mpf/.libs/iset_str.o mpf/.libs/iset_d.o mpf/.libs/clear.o mpf/.libs/clears.o mpf/.libs/get_str.o mpf/.libs/dump.o mpf/.libs/size.o mpf/.libs/eq.o mpf/.libs/reldiff.o mpf/.libs/sqrt.o mpf/.libs/random2.o mpf/.libs/inp_str.o mpf/.libs/out_str.o mpf/.libs/add.o mpf/.libs/add_ui.o mpf/.libs/sub.o mpf/.libs/sub_ui.o mpf/.libs/ui_sub.o mpf/.libs/mul.o mpf/.libs/mul_ui.o mpf/.libs/div.o mpf/.libs/div_ui.o mpf/.libs/cmp.o mpf/.libs/cmp_d.o mpf/.libs/cmp_ui.o mpf/.libs/cmp_si.o mpf/.libs/mul_2exp.o mpf/.libs/div_2exp.o mpf/.libs/abs.o mpf/.libs/neg.o mpf/.libs/set_q.o mpf/.libs/get_d.o mpf/.libs/get_d_2exp.o mpf/.libs/set_dfl_prec.o mpf/.libs/set_prc.o mpf/.libs/set_prc_raw.o mpf/.libs/get_dfl_prec.o mpf/.libs/get_prc.o mpf/.libs/ui_div.o mpf/.libs/sqrt_ui.o mpf/.libs/ceilfloor.o mpf/.libs/trunc.o mpf/.libs/pow_ui.o mpf/.libs/urandomb.o mpf/.libs/swap.o mpf/.libs/fits_sint.o mpf/.libs/fits_slong.o mpf/.libs/fits_sshort.o mpf/.libs/fits_uint.o mpf/.libs/fits_ulong.o mpf/.libs/fits_ushort.o mpf/.libs/get_si.o mpf/.libs/get_ui.o mpf/.libs/int_p.o mpz/.libs/abs.o mpz/.libs/add.o mpz/.libs/add_ui.o mpz/.libs/aorsmul.o mpz/.libs/aorsmul_i.o mpz/.libs/and.o mpz/.libs/array_init.o mpz/.libs/bin_ui.o mpz/.libs/bin_uiui.o mpz/.libs/cdiv_q.o mpz/.libs/cdiv_q_ui.o mpz/.libs/cdiv_qr.o mpz/.libs/cdiv_qr_ui.o mpz/.libs/cdiv_r.o mpz/.libs/cdiv_r_ui.o mpz/.libs/cdiv_ui.o mpz/.libs/cfdiv_q_2exp.o mpz/.libs/cfdiv_r_2exp.o mpz/.libs/clear.o mpz/.libs/clears.o mpz/.libs/clrbit.o mpz/.libs/cmp.o mpz/.libs/cmp_d.o mpz/.libs/cmp_si.o mpz/.libs/cmp_ui.o mpz/.libs/cmpabs.o mpz/.libs/cmpabs_d.o mpz/.libs/cmpabs_ui.o mpz/.libs/com.o mpz/.libs/combit.o mpz/.libs/cong.o mpz/.libs/cong_2exp.o mpz/.libs/cong_ui.o mpz/.libs/divexact.o mpz/.libs/divegcd.o mpz/.libs/dive_ui.o mpz/.libs/divis.o mpz/.libs/divis_ui.o mpz/.libs/divis_2exp.o mpz/.libs/dump.o mpz/.libs/export.o mpz/.libs/fac_ui.o mpz/.libs/fdiv_q.o mpz/.libs/fdiv_q_ui.o mpz/.libs/fdiv_qr.o mpz/.libs/fdiv_qr_ui.o mpz/.libs/fdiv_r.o mpz/.libs/fdiv_r_ui.o mpz/.libs/fdiv_ui.o mpz/.libs/fib_ui.o mpz/.libs/fib2_ui.o mpz/.libs/fits_sint.o mpz/.libs/fits_slong.o mpz/.libs/fits_sshort.o mpz/.libs/fits_uint.o mpz/.libs/fits_ulong.o mpz/.libs/fits_ushort.o mpz/.libs/gcd.o mpz/.libs/gcd_ui.o mpz/.libs/gcdext.o mpz/.libs/get_d.o mpz/.libs/get_d_2exp.o mpz/.libs/get_si.o mpz/.libs/get_str.o mpz/.libs/get_ui.o mpz/.libs/getlimbn.o mpz/.libs/hamdist.o mpz/.libs/import.o mpz/.libs/init.o mpz/.libs/init2.o mpz/.libs/inits.o mpz/.libs/inp_raw.o mpz/.libs/inp_str.o mpz/.libs/invert.o mpz/.libs/ior.o mpz/.libs/iset.o mpz/.libs/iset_d.o mpz/.libs/iset_si.o mpz/.libs/iset_str.o mpz/.libs/iset_ui.o mpz/.libs/jacobi.o mpz/.libs/kronsz.o mpz/.libs/kronuz.o mpz/.libs/kronzs.o mpz/.libs/kronzu.o mpz/.libs/lcm.o mpz/.libs/lcm_ui.o mpz/.libs/lucnum_ui.o mpz/.libs/lucnum2_ui.o mpz/.libs/millerrabin.o mpz/.libs/mod.o mpz/.libs/mul.o mpz/.libs/mul_2exp.o mpz/.libs/mul_si.o mpz/.libs/mul_ui.o mpz/.libs/n_pow_ui.o mpz/.libs/neg.o mpz/.libs/nextprime.o mpz/.libs/out_raw.o mpz/.libs/out_str.o mpz/.libs/perfpow.o mpz/.libs/perfsqr.o mpz/.libs/popcount.o mpz/.libs/pow_ui.o mpz/.libs/powm.o mpz/.libs/powm_sec.o mpz/.libs/powm_ui.o mpz/.libs/pprime_p.o mpz/.libs/random.o mpz/.libs/random2.o mpz/.libs/realloc.o mpz/.libs/realloc2.o mpz/.libs/remove.o mpz/.libs/root.o mpz/.libs/rootrem.o mpz/.libs/rrandomb.o mpz/.libs/scan0.o mpz/.libs/scan1.o mpz/.libs/set.o mpz/.libs/set_d.o mpz/.libs/set_f.o mpz/.libs/set_q.o mpz/.libs/set_si.o mpz/.libs/set_str.o mpz/.libs/set_ui.o mpz/.libs/setbit.o mpz/.libs/size.o mpz/.libs/sizeinbase.o mpz/.libs/sqrt.o mpz/.libs/sqrtrem.o mpz/.libs/sub.o mpz/.libs/sub_ui.o mpz/.libs/swap.o mpz/.libs/tdiv_ui.o mpz/.libs/tdiv_q.o mpz/.libs/tdiv_q_2exp.o mpz/.libs/tdiv_q_ui.o mpz/.libs/tdiv_qr.o mpz/.libs/tdiv_qr_ui.o mpz/.libs/tdiv_r.o mpz/.libs/tdiv_r_2exp.o mpz/.libs/tdiv_r_ui.o mpz/.libs/tstbit.o mpz/.libs/ui_pow_ui.o mpz/.libs/ui_sub.o mpz/.libs/urandomb.o mpz/.libs/urandomm.o mpz/.libs/xor.o mpq/.libs/abs.o mpq/.libs/aors.o mpq/.libs/canonicalize.o mpq/.libs/clear.o mpq/.libs/clears.o mpq/.libs/cmp.o mpq/.libs/cmp_si.o mpq/.libs/cmp_ui.o mpq/.libs/div.o mpq/.libs/get_d.o mpq/.libs/get_den.o mpq/.libs/get_num.o mpq/.libs/get_str.o mpq/.libs/init.o mpq/.libs/inits.o mpq/.libs/inp_str.o mpq/.libs/inv.o mpq/.libs/md_2exp.o mpq/.libs/mul.o mpq/.libs/neg.o mpq/.libs/out_str.o mpq/.libs/set.o mpq/.libs/set_den.o mpq/.libs/set_num.o mpq/.libs/set_si.o mpq/.libs/set_str.o mpq/.libs/set_ui.o mpq/.libs/equal.o mpq/.libs/set_z.o mpq/.libs/set_d.o mpq/.libs/set_f.o mpq/.libs/swap.o mpn/.libs/fib_table.o mpn/.libs/mp_bases.o mpn/.libs/add.o mpn/.libs/add_1.o mpn/.libs/add_n.o mpn/.libs/sub.o mpn/.libs/sub_1.o mpn/.libs/sub_n.o mpn/.libs/neg.o mpn/.libs/com.o mpn/.libs/mul_1.o mpn/.libs/addmul_1.o mpn/.libs/submul_1.o mpn/.libs/lshift.o mpn/.libs/rshift.o mpn/.libs/dive_1.o mpn/.libs/diveby3.o mpn/.libs/divis.o mpn/.libs/divrem.o mpn/.libs/divrem_1.o mpn/.libs/divrem_2.o mpn/.libs/fib2_ui.o mpn/.libs/mod_1.o mpn/.libs/mod_34lsub1.o mpn/.libs/mode1o.o mpn/.libs/pre_mod_1.o mpn/.libs/dump.o mpn/.libs/mod_1_1.o mpn/.libs/mod_1_2.o mpn/.libs/mod_1_3.o mpn/.libs/mod_1_4.o mpn/.libs/lshiftc.o mpn/.libs/mul.o mpn/.libs/mul_fft.o mpn/.libs/mul_n.o mpn/.libs/sqr.o mpn/.libs/mul_basecase.o mpn/.libs/sqr_basecase.o mpn/.libs/nussbaumer_mul.o mpn/.libs/random.o mpn/.libs/random2.o mpn/.libs/pow_1.o mpn/.libs/rootrem.o mpn/.libs/sqrtrem.o mpn/.libs/get_str.o mpn/.libs/set_str.o mpn/.libs/scan0.o mpn/.libs/scan1.o mpn/.libs/popcount.o mpn/.libs/hamdist.o mpn/.libs/cmp.o mpn/.libs/perfsqr.o mpn/.libs/perfpow.o mpn/.libs/gcd_1.o mpn/.libs/gcd.o mpn/.libs/gcdext_1.o mpn/.libs/gcdext.o mpn/.libs/gcd_lehmer.o mpn/.libs/gcd_subdiv_step.o mpn/.libs/gcdext_lehmer.o mpn/.libs/gcdext_subdiv_step.o mpn/.libs/div_q.o mpn/.libs/tdiv_qr.o mpn/.libs/jacbase.o mpn/.libs/get_d.o mpn/.libs/matrix22_mul.o mpn/.libs/hgcd2.o mpn/.libs/hgcd.o mpn/.libs/mullo_n.o mpn/.libs/mullo_basecase.o mpn/.libs/toom22_mul.o mpn/.libs/toom32_mul.o mpn/.libs/toom42_mul.o mpn/.libs/toom52_mul.o mpn/.libs/toom62_mul.o mpn/.libs/toom33_mul.o mpn/.libs/toom43_mul.o mpn/.libs/toom53_mul.o mpn/.libs/toom63_mul.o mpn/.libs/toom44_mul.o mpn/.libs/toom6h_mul.o mpn/.libs/toom6_sqr.o mpn/.libs/toom8h_mul.o mpn/.libs/toom8_sqr.o mpn/.libs/toom_couple_handling.o mpn/.libs/toom2_sqr.o mpn/.libs/toom3_sqr.o mpn/.libs/toom4_sqr.o mpn/.libs/toom_eval_dgr3_pm1.o mpn/.libs/toom_eval_dgr3_pm2.o mpn/.libs/toom_eval_pm1.o mpn/.libs/toom_eval_pm2.o mpn/.libs/toom_eval_pm2exp.o mpn/.libs/toom_eval_pm2rexp.o mpn/.libs/toom_interpolate_5pts.o mpn/.libs/toom_interpolate_6pts.o mpn/.libs/toom_interpolate_7pts.o mpn/.libs/toom_interpolate_8pts.o mpn/.libs/toom_interpolate_12pts.o mpn/.libs/toom_interpolate_16pts.o mpn/.libs/invertappr.o mpn/.libs/invert.o mpn/.libs/binvert.o mpn/.libs/mulmod_bnm1.o mpn/.libs/sqrmod_bnm1.o mpn/.libs/sbpi1_div_q.o mpn/.libs/sbpi1_div_qr.o mpn/.libs/sbpi1_divappr_q.o mpn/.libs/dcpi1_div_q.o mpn/.libs/dcpi1_div_qr.o mpn/.libs/dcpi1_divappr_q.o mpn/.libs/mu_div_qr.o mpn/.libs/mu_divappr_q.o mpn/.libs/mu_div_q.o mpn/.libs/bdiv_q_1.o mpn/.libs/sbpi1_bdiv_q.o mpn/.libs/sbpi1_bdiv_qr.o mpn/.libs/dcpi1_bdiv_q.o mpn/.libs/dcpi1_bdiv_qr.o mpn/.libs/mu_bdiv_q.o mpn/.libs/mu_bdiv_qr.o mpn/.libs/bdiv_q.o mpn/.libs/bdiv_qr.o mpn/.libs/divexact.o mpn/.libs/bdiv_dbm1c.o mpn/.libs/redc_1.o mpn/.libs/redc_2.o mpn/.libs/redc_n.o mpn/.libs/powm.o mpn/.libs/powlo.o mpn/.libs/powm_sec.o mpn/.libs/subcnd_n.o mpn/.libs/redc_1_sec.o mpn/.libs/trialdiv.o mpn/.libs/remove.o mpn/.libs/and_n.o mpn/.libs/andn_n.o mpn/.libs/nand_n.o mpn/.libs/ior_n.o mpn/.libs/iorn_n.o mpn/.libs/nior_n.o mpn/.libs/xor_n.o mpn/.libs/xnor_n.o mpn/.libs/copyi.o mpn/.libs/copyd.o mpn/.libs/zero.o mpn/.libs/pre_divrem_1.o printf/.libs/asprintf.o printf/.libs/asprntffuns.o printf/.libs/doprnt.o printf/.libs/doprntf.o printf/.libs/doprnti.o printf/.libs/fprintf.o printf/.libs/obprintf.o printf/.libs/obvprintf.o printf/.libs/obprntffuns.o printf/.libs/printf.o printf/.libs/printffuns.o printf/.libs/snprintf.o printf/.libs/snprntffuns.o printf/.libs/sprintf.o printf/.libs/sprintffuns.o printf/.libs/vasprintf.o printf/.libs/vfprintf.o printf/.libs/vprintf.o printf/.libs/vsnprintf.o printf/.libs/vsprintf.o printf/.libs/repl-vsnprintf.o scanf/.libs/doscan.o scanf/.libs/fscanf.o scanf/.libs/fscanffuns.o scanf/.libs/scanf.o scanf/.libs/sscanf.o scanf/.libs/sscanffuns.o scanf/.libs/vfscanf.o scanf/.libs/vscanf.o scanf/.libs/vsscanf.o */
#* OBJECTS = $(OBJECTS_5_0_2)                                          */

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-gmp
.NOTPARALLEL: boot

boot: boot-gmp

boot-gmp: $(OUTDIR)/libgmp.a
	$(MAKE) $(BOOTLIBDIR)/libbigloogmp-$(RELEASE).$(SHAREDSUFFIX)
	$(MAKE) $(BOOTLIBDIR)/libbigloogmp-$(RELEASE).a

objects:

$(OUTDIR)/libgmp.a:
	$(MAKE) -C $(GMP) CFLAGS="$(CCFLAGS)"

$(BOOTLIBDIR)/libbigloogmp-$(RELEASE).$(SHAREDSUFFIX): $(OUTDIR)/libgmp.a
	(mkdir -p GMP_OBJ; cd GMP_OBJ; ar x ../$(OUTDIR)/libgmp.a)

ld:
	$(MAKE) shared-lib \
             LIBDEST=$@ \
	     LIBPATH=$(BOOTLIBDIR) \
             SONAME=libbigloogmp-$(RELEASE).$(SHAREDSUFFIX) \
             GCCLOSELIB= \
             CLOSELIBS= -lm -lc \
             EXTRALIBS= \
             LDLIBS= -lm -lc \
             OBJECTS="$(wildcard GMP_OBJ/*.o)"

$(BOOTLIBDIR)/libbigloogmp-$(RELEASE).a:
	cp $(OUTDIR)/libgmp.a $@
	$(RANLIB) $@

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=gmp/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d "$(GMP)" ]; then \
	  $(MAKE) -C $(GMP) clean; \
        fi
	$(RM) -rf objs

cleanall: clean

distclean:
	$(RM) -rf gmp

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
.PHONY: distrib

distrib:

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install

install:
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloogmp-$(RELEASE); \
          cp $(GMP)/gmp.h $(LIBDIR)/$(FILDIR)/gmp.h && \
          chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/gmp.h; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) uninstall-lib LIB=libbigloogmp-$(RELEASE)
	$(MAKE) SHAREDSUFFIX=$(SHAREDSUFFIX).1 uninstall-lib LIB=libbigloogmp-$(RELEASE)

