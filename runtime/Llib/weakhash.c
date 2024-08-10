/*===========================================================================*/
/*   (Llib/weakhash.scm)                                                     */
/*   Bigloo (4.6a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Tue Jul 23 08:26:45 CEST 2024       */
/*===========================================================================*/
/* COMPILATION: (../bin/bigloo.new -saw -srfi bigloo-unsafe -O3 -fcfa-arithmetic -q -lib-dir /home/hgruniaux/bigloo/lib/bigloo/4.6a -rm -copt -w -lib-dir /home/hgruniaux/bigloo/lib/bigloo/4.6a -mklib -cc gcc -fsharing -q -no-hello -unsafe -safee -O4 -s -c Llib/weakhash.scm -rm) */

/* SAW compilation */
#define BGL_SAW 1
/* GC selection */
#define THE_GC BOEHM_GC

/* unsafe mode */
#define BIGLOO_UNSAFE 1

/* traces mode */
#define BIGLOO_TRACE 0

/* standard Bigloo include */
#include <bigloo.h>

#ifdef __cplusplus
extern "C" {
#endif
#ifndef BGL_MODULE_TYPE_DEFINITIONS
#define BGL_MODULE_TYPE_DEFINITIONS
#ifndef BGL___WEAKHASH_TYPE_DEFINITIONS
#define BGL___WEAKHASH_TYPE_DEFINITIONS
#endif // BGL___WEAKHASH_TYPE_DEFINITIONS
#endif // BGL_MODULE_TYPE_DEFINITIONS

BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2putz12z12zz__weakhashz00(obj_t, obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2getz00zz__weakhashz00(obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2forzd2eachzd2zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2forzd2eachzb0zz__weakhashz00(obj_t, obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2keyzd2listzd2zz__weakhashz00(obj_t);
static obj_t BGl_z62weakzd2hashtablezd2keyzd2listzb0zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31370ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31362ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_requirezd2initializa7ationz75zz__weakhashz00 = BUNSPEC;
static obj_t BGl_traversezd2bucketszd2zz__weakhashz00(obj_t, obj_t, long, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2updatez12z12zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2getz62zz__weakhashz00(obj_t, obj_t, obj_t);
extern obj_t BGl_typezd2errorzd2zz__errorz00(obj_t, obj_t, obj_t, obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_modulezd2initializa7ationz75zz__weakhashz00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__threadz00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__objectz00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__bexitz00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__hashz00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__paramz00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__r4_symbols_6_4z00(long, char *);
extern obj_t BGl_modulezd2initializa7ationz75zz__errorz00(long, char *);
static obj_t BGl_removez00zz__weakhashz00 = BUNSPEC;
static obj_t BGl_z62zc3z04anonymousza31623ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31380ze3ze5zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31372ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
extern long BGl_hashtablezd2siza7ez75zz__hashz00(obj_t);
static obj_t BGl_z62weakzd2hashtablezd2clearz12z70zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_symbol2129z00zz__weakhashz00 = BUNSPEC;
static obj_t BGl_weakzd2oldzd2hashtablezd2addz12zc0zz__weakhashz00(obj_t, obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31301ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
static bool_t BGl_weakzd2oldzd2hashtablezd2removez12zc0zz__weakhashz00(obj_t, obj_t);
extern bool_t BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00(obj_t);
static obj_t BGl_toplevelzd2initzd2zz__weakhashz00(void);
static obj_t BGl_z62zc3z04anonymousza31527ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31438ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_weakzd2oldzd2hashtablezd2putz12zc0zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_keepgoingz00zz__weakhashz00 = BUNSPEC;
extern void bgl_weakptr_ref_set(obj_t, obj_t);
extern obj_t BGl_formatz00zz__r4_output_6_10_3z00(obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31374ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
static bool_t BGl_weakzd2keyszd2hashtablezd2containszf3z21zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_cnstzd2initzd2zz__weakhashz00(void);
static obj_t BGl_weakzd2oldzd2hashtablezd2expandz12zc0zz__weakhashz00(obj_t);
static obj_t BGl_genericzd2initzd2zz__weakhashz00(void);
static bool_t BGl_weakzd2oldzd2hashtablezd2filterz12zc0zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2removez12z70zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2ze3vectorz81zz__weakhashz00(obj_t, obj_t);
static bool_t BGl_weakzd2keyszd2hashtablezd2removez12zc0zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_importedzd2moduleszd2initz00zz__weakhashz00(void);
static obj_t BGl_gczd2rootszd2initz00zz__weakhashz00(void);
static obj_t BGl_z62zc3z04anonymousza31480ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_weakzd2keyszd2hashtablezd2getzd2zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_objectzd2initzd2zz__weakhashz00(void);
extern obj_t BGl_copyzd2vectorzd2zz__r4_vectors_6_8z00(obj_t, long);
extern obj_t BGl_2za2za2zz__r4_numbers_6_5z00(obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2expandz12z70zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00(obj_t);
static obj_t BGl_z62weakzd2hashtablezd2filterz12z70zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2addz12z70zz__weakhashz00(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
static bool_t BGl_weakzd2keyszd2hashtablezd2filterz12zc0zz__weakhashz00(obj_t, obj_t);
static bool_t BGl_keyszd2traversezd2hashz00zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31410ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
extern obj_t bgl_weakptr_data(obj_t);
extern obj_t bgl_weakptr_ref(obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2mapz00zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2putz12z70zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2ze3listz81zz__weakhashz00(obj_t, obj_t);
static obj_t BGl_weakzd2oldzd2hashtablezd2getzd2zz__weakhashz00(obj_t, obj_t);
extern obj_t BGl_filterz12z12zz__r4_control_features_6_9z00(obj_t, obj_t);
BGL_EXPORTED_DECL bool_t BGl_weakzd2hashtablezd2containszf3zf3zz__weakhashz00(obj_t, obj_t);
static bool_t BGl_weakzd2oldzd2hashtablezd2containszf3z21zz__weakhashz00(obj_t, obj_t);
extern obj_t bstring_to_symbol(obj_t);
extern obj_t make_vector(long, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2ze3listze3zz__weakhashz00(obj_t);
static obj_t BGl_z62weakzd2hashtablezd2mapz62zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_weakzd2oldzd2hashtablezd2updatez12zc0zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_methodzd2initzd2zz__weakhashz00(void);
extern obj_t bgl_make_weakptr(obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2removez12z12zz__weakhashz00(obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2ze3vectorze3zz__weakhashz00(obj_t);
static obj_t BGl_z62zc3z04anonymousza31422ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_z62zc3z04anonymousza31406ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2updatez12z70zz__weakhashz00(obj_t, obj_t, obj_t, obj_t, obj_t);
static obj_t BGl_weakzd2keyszd2hashtablezd2updatez12zc0zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2expandz12z12zz__weakhashz00(obj_t);
static obj_t BGl_removestopz00zz__weakhashz00 = BUNSPEC;
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2filterz12z12zz__weakhashz00(obj_t, obj_t);
extern long BGl_getzd2hashnumberzd2persistentz00zz__hashz00(obj_t);
static obj_t BGl_z62zc3z04anonymousza31399ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
extern long BGl_getzd2hashnumberzd2zz__hashz00(obj_t);
static obj_t BGl_weakzd2keyszd2hashtablezd2addz12zc0zz__weakhashz00(obj_t, obj_t, obj_t, obj_t, obj_t);
extern bool_t BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00(obj_t);
static bool_t BGl_oldzd2traversezd2hashz00zz__weakhashz00(obj_t, obj_t);
extern obj_t BGl_errorz00zz__errorz00(obj_t, obj_t, obj_t);
extern obj_t BGl_gcz00zz__biglooz00(obj_t);
static obj_t BGl_weakzd2keyszd2hashtablezd2putz12zc0zz__weakhashz00(obj_t, obj_t, obj_t);
static obj_t BGl_z62weakzd2hashtablezd2containszf3z91zz__weakhashz00(obj_t, obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2clearz12z12zz__weakhashz00(obj_t);
static obj_t BGl_z62zc3z04anonymousza31586ze3ze5zz__weakhashz00(obj_t, obj_t, obj_t, obj_t);
BGL_EXPORTED_DECL obj_t BGl_weakzd2hashtablezd2addz12z12zz__weakhashz00(obj_t, obj_t, obj_t, obj_t, obj_t);
static obj_t *__cnst;


DEFINE_STRING( BGl_string2120z00zz__weakhashz00, BgL_bgl_string2120za700za7za7_2141za7, "&weak-hashtable->list", 21 );
DEFINE_STRING( BGl_string2121z00zz__weakhashz00, BgL_bgl_string2121za700za7za7_2142za7, "&weak-hashtable-key-list", 24 );
DEFINE_STRING( BGl_string2122z00zz__weakhashz00, BgL_bgl_string2122za700za7za7_2143za7, "&weak-hashtable-map", 19 );
DEFINE_STATIC_BGL_PROCEDURE( BGl_proc2116z00zz__weakhashz00, BgL_bgl_za762za7c3za704anonymo2144za7, BGl_z62zc3z04anonymousza31301ze3ze5zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_STRING( BGl_string2123z00zz__weakhashz00, BgL_bgl_string2123za700za7za7_2145za7, "procedure", 9 );
DEFINE_STRING( BGl_string2124z00zz__weakhashz00, BgL_bgl_string2124za700za7za7_2146za7, "&weak-hashtable-for-each", 24 );
DEFINE_STRING( BGl_string2125z00zz__weakhashz00, BgL_bgl_string2125za700za7za7_2147za7, "&weak-hashtable-filter!", 23 );
DEFINE_STRING( BGl_string2128z00zz__weakhashz00, BgL_bgl_string2128za700za7za7_2148za7, "&weak-hashtable-clear!", 22 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2filterz12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2149z00, BGl_z62weakzd2hashtablezd2filterz12z70zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_STRING( BGl_string2130z00zz__weakhashz00, BgL_bgl_string2130za700za7za7_2150za7, "persistent", 10 );
DEFINE_STRING( BGl_string2131z00zz__weakhashz00, BgL_bgl_string2131za700za7za7_2151za7, "&weak-hashtable-contains?", 25 );
DEFINE_STRING( BGl_string2132z00zz__weakhashz00, BgL_bgl_string2132za700za7za7_2152za7, "&weak-hashtable-get", 19 );
DEFINE_STATIC_BGL_PROCEDURE( BGl_proc2126z00zz__weakhashz00, BgL_bgl_za762za7c3za704anonymo2153za7, BGl_z62zc3z04anonymousza31406ze3ze5zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_STRING( BGl_string2133z00zz__weakhashz00, BgL_bgl_string2133za700za7za7_2154za7, "&weak-hashtable-put!", 20 );
DEFINE_STATIC_BGL_PROCEDURE( BGl_proc2127z00zz__weakhashz00, BgL_bgl_za762za7c3za704anonymo2155za7, BGl_z62zc3z04anonymousza31410ze3ze5zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_STRING( BGl_string2134z00zz__weakhashz00, BgL_bgl_string2134za700za7za7_2156za7, "&weak-hashtable-update!", 23 );
DEFINE_STRING( BGl_string2135z00zz__weakhashz00, BgL_bgl_string2135za700za7za7_2157za7, "&weak-hashtable-add!", 20 );
DEFINE_STRING( BGl_string2136z00zz__weakhashz00, BgL_bgl_string2136za700za7za7_2158za7, "&weak-hashtable-remove!", 23 );
DEFINE_STRING( BGl_string2137z00zz__weakhashz00, BgL_bgl_string2137za700za7za7_2159za7, "Hashtable too large (new-len=~a/~a, size=~a)", 44 );
DEFINE_STRING( BGl_string2138z00zz__weakhashz00, BgL_bgl_string2138za700za7za7_2160za7, "hashtable-put!", 14 );
DEFINE_STRING( BGl_string2139z00zz__weakhashz00, BgL_bgl_string2139za700za7za7_2161za7, "&weak-hashtable-expand!", 23 );
DEFINE_STRING( BGl_string2140z00zz__weakhashz00, BgL_bgl_string2140za700za7za7_2162za7, "__weakhash", 10 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2clearz12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2163z00, BGl_z62weakzd2hashtablezd2clearz12z70zz__weakhashz00, 0L, BUNSPEC, 1 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2ze3vectorzd2envz31zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2164z00, BGl_z62weakzd2hashtablezd2ze3vectorz81zz__weakhashz00, 0L, BUNSPEC, 1 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2mapzd2envzd2zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2165z00, BGl_z62weakzd2hashtablezd2mapz62zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2getzd2envzd2zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2166z00, BGl_z62weakzd2hashtablezd2getz62zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2containszf3zd2envz21zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2167z00, BGl_z62weakzd2hashtablezd2containszf3z91zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2expandz12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2168z00, BGl_z62weakzd2hashtablezd2expandz12z70zz__weakhashz00, 0L, BUNSPEC, 1 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2ze3listzd2envz31zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2169z00, BGl_z62weakzd2hashtablezd2ze3listz81zz__weakhashz00, 0L, BUNSPEC, 1 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2updatez12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2170z00, BGl_z62weakzd2hashtablezd2updatez12z70zz__weakhashz00, 0L, BUNSPEC, 4 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2forzd2eachzd2envz00zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2171z00, BGl_z62weakzd2hashtablezd2forzd2eachzb0zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2addz12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2172z00, BGl_z62weakzd2hashtablezd2addz12z70zz__weakhashz00, 0L, BUNSPEC, 5 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2removez12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2173z00, BGl_z62weakzd2hashtablezd2removez12z70zz__weakhashz00, 0L, BUNSPEC, 2 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2putz12zd2envzc0zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2174z00, BGl_z62weakzd2hashtablezd2putz12z70zz__weakhashz00, 0L, BUNSPEC, 3 );
DEFINE_STRING( BGl_string2117z00zz__weakhashz00, BgL_bgl_string2117za700za7za7_2175za7, "/home/hgruniaux/bigloo/runtime/Llib/weakhash.scm", 48 );
DEFINE_STRING( BGl_string2118z00zz__weakhashz00, BgL_bgl_string2118za700za7za7_2176za7, "&weak-hashtable->vector", 23 );
DEFINE_STRING( BGl_string2119z00zz__weakhashz00, BgL_bgl_string2119za700za7za7_2177za7, "struct", 6 );
DEFINE_EXPORT_BGL_PROCEDURE( BGl_weakzd2hashtablezd2keyzd2listzd2envz00zz__weakhashz00, BgL_bgl_za762weakza7d2hashta2178z00, BGl_z62weakzd2hashtablezd2keyzd2listzb0zz__weakhashz00, 0L, BUNSPEC, 1 );

/* GC roots registration */
static obj_t bgl_gc_roots_register() {
#if defined( BGL_GC_ROOTS )
#define ADD_ROOT( addr ) (addr > roots_max ? roots_max = addr : (addr < roots_min ? roots_min = addr : 0))
void *roots_min = (void*)ULONG_MAX, *roots_max = 0;
ADD_ROOT( (void *)(&BGl_requirezd2initializa7ationz75zz__weakhashz00) );
ADD_ROOT( (void *)(&BGl_removez00zz__weakhashz00) );
ADD_ROOT( (void *)(&BGl_symbol2129z00zz__weakhashz00) );
ADD_ROOT( (void *)(&BGl_keepgoingz00zz__weakhashz00) );
ADD_ROOT( (void *)(&BGl_removestopz00zz__weakhashz00) );
#undef ADD_ROOT
if( roots_max > 0 ) GC_add_roots( roots_min, ((void **)roots_max) + 1 );
#endif
return BUNSPEC;
}


obj_t BGl_modulezd2initializa7ationz75zz__weakhashz00(long V0, char * V1) {
L0:	BGL_RTL_IFNE(L2, CBOOL((BGl_requirezd2initializa7ationz75zz__weakhashz00)));
L1:	return((BGL_RTL_CAST(obj_t , (BUNSPEC))));
L2:	BGL_RTL_STOREG(BGl_requirezd2initializa7ationz75zz__weakhashz00, BGL_RTL_CAST(obj_t , BBOOL((((bool_t)0)))));
	BGl_gczd2rootszd2initz00zz__weakhashz00();
	BGl_cnstzd2initzd2zz__weakhashz00();
	BGl_importedzd2moduleszd2initz00zz__weakhashz00();
	BGl_objectzd2initzd2zz__weakhashz00();
	BGl_genericzd2initzd2zz__weakhashz00();
	BGl_methodzd2initzd2zz__weakhashz00();
	return((BGl_toplevelzd2initzd2zz__weakhashz00()));

}

obj_t BGl_cnstzd2initzd2zz__weakhashz00() {
L0:	BGL_RTL_STOREG(BGl_symbol2129z00zz__weakhashz00, bstring_to_symbol((BGl_string2130z00zz__weakhashz00)));
	return(BGL_RTL_NOP());

}

obj_t BGl_gczd2rootszd2initz00zz__weakhashz00() {
L0:	return(bgl_gc_roots_register());

}

obj_t BGl_toplevelzd2initzd2zz__weakhashz00() {
L0:	BGL_RTL_STOREG(BGl_keepgoingz00zz__weakhashz00, MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (BUNSPEC)), BGL_RTL_CAST(obj_t , (BUNSPEC))));
	BGL_RTL_STOREG(BGl_removez00zz__weakhashz00, MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (BUNSPEC)), BGL_RTL_CAST(obj_t , (BUNSPEC))));
	BGL_RTL_STOREG(BGl_removestopz00zz__weakhashz00, MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (BUNSPEC)), BGL_RTL_CAST(obj_t , (BUNSPEC))));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_NOP()));

}

obj_t BGl_traversezd2bucketszd2zz__weakhashz00(obj_t V0, obj_t V1, long V2, obj_t V3) {
 obj_t R54;
 int R53;
 obj_t R52;
 obj_t R51;
 obj_t V50;
 obj_t V49;
 obj_t V48;
 obj_t R47;
 int R46;
 obj_t R45;
 obj_t R44;
 obj_t R43;
 obj_t R42;
 int R41;
 obj_t V40;
 obj_t R39;
 obj_t V38;
 obj_t R37;
 obj_t V36;
 obj_t V35;
 obj_t R34;
 int R33;
 obj_t R32;
 obj_t R31;
 obj_t R30;
 obj_t R29;
 int R28;
 obj_t V27;
 obj_t R26;
 obj_t V25;
 obj_t R24;
 obj_t V23;
 obj_t V22;
 obj_t R21;
 int R20;
 obj_t R19;
 obj_t R18;
 obj_t R17;
 obj_t R16;
 int R15;
 obj_t V14;
 obj_t R13;
 obj_t V12;
 obj_t R11;
 obj_t V10;
 obj_t R9;
 int R8;
 obj_t R7;
 obj_t R6;
 obj_t V5;
 obj_t V4;
L0:	BGL_RTL_IFNE(L19, BGL_RTL_EQ((1L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L1:	BGL_RTL_IFNE(L37, BGL_RTL_EQ((2L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L2:	BGL_RTL_IFNE(L55, BGL_RTL_EQ((3L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L3:	V4 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V5 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V2))));
	BGL_RTL_GO(L9);
L4:	V4 = ((V5));
	V5 = ((CDR(BGL_RTL_CAST(obj_t , (V5)))));
	BGL_RTL_GO(L9);
L5:	R6 = BGL_RTL_CAST(obj_t , (V4));
	R7 = (CDR(BGL_RTL_CAST(obj_t , (V5))));
	SET_CDR(R6, R7);
	BGL_RTL_GO(L8);
L6:	R8 = (int)((0L));
	R9 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R8, R9);
	BGL_RTL_IFNE(L5, CBOOL((V4)));
L7:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V5)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L8:	V5 = ((CDR(BGL_RTL_CAST(obj_t , (V5)))));
L9:	BGL_RTL_IFNE(L69, NULLP((V5)));
L10:	V10 = ((obj_t (*)())PROCEDURE_L_ENTRY((V3)))((V3), (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V5)))))), (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V5)))))), (V5));
	BGL_RTL_IFNE(L4, BGL_RTL_EQ((V10), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L11:	BGL_RTL_IFNE(L6, BGL_RTL_EQ((V10), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L12:	BGL_RTL_IFNE(L71, BGL_RTL_EQ((V10), BGL_RTL_CAST(obj_t , (BGl_removestopz00zz__weakhashz00))));
L13:	return((((((V10))))));
L14:	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L15:	R11 = BGL_RTL_CAST(obj_t , (V12));
	R13 = (CDR(BGL_RTL_CAST(obj_t , (V14))));
	SET_CDR(R11, R13);
	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L16:	R15 = (int)((0L));
	R16 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R15, R16);
	BGL_RTL_IFNE(L15, CBOOL((V12)));
L17:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V14)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L18:	R17 = BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00));
	BGL_RTL_GO(L28);
L19:	V12 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V14 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V2))));
	BGL_RTL_GO(L25);
L20:	V12 = ((V14));
	V14 = ((CDR(BGL_RTL_CAST(obj_t , (V14)))));
	BGL_RTL_GO(L25);
L21:	R18 = BGL_RTL_CAST(obj_t , (V12));
	R19 = (CDR(BGL_RTL_CAST(obj_t , (V14))));
	SET_CDR(R18, R19);
	BGL_RTL_GO(L24);
L22:	R20 = (int)((0L));
	R21 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R20, R21);
	BGL_RTL_IFNE(L21, CBOOL((V12)));
L23:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V14)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L24:	V14 = ((CDR(BGL_RTL_CAST(obj_t , (V14)))));
L25:	BGL_RTL_IFNE(L14, NULLP((V14)));
L26:	V22 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V14))))))));
	BGL_RTL_IFNE(L18, BGL_RTL_EQ((V22), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L27:	R17 = ((obj_t (*)())PROCEDURE_L_ENTRY((V3)))((V3), (V22), (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V14)))))), (V14));
L28:	V23 = (R17);
	BGL_RTL_IFNE(L20, BGL_RTL_EQ((V23), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L29:	BGL_RTL_IFNE(L22, BGL_RTL_EQ((V23), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L30:	BGL_RTL_IFNE(L16, BGL_RTL_EQ((V23), BGL_RTL_CAST(obj_t , (BGl_removestopz00zz__weakhashz00))));
L31:	return(((V23)));
L32:	return(((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00)))));
L33:	R24 = BGL_RTL_CAST(obj_t , (V25));
	R26 = (CDR(BGL_RTL_CAST(obj_t , (V27))));
	SET_CDR(R24, R26);
	return(((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00)))));
L34:	R28 = (int)((0L));
	R29 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R28, R29);
	BGL_RTL_IFNE(L33, CBOOL((V25)));
L35:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V27)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
	return(((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00)))));
L36:	R30 = BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00));
	BGL_RTL_GO(L46);
L37:	V25 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V27 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V2))));
	BGL_RTL_GO(L43);
L38:	V25 = ((V27));
	V27 = ((CDR(BGL_RTL_CAST(obj_t , (V27)))));
	BGL_RTL_GO(L43);
L39:	R31 = BGL_RTL_CAST(obj_t , (V25));
	R32 = (CDR(BGL_RTL_CAST(obj_t , (V27))));
	SET_CDR(R31, R32);
	BGL_RTL_GO(L42);
L40:	R33 = (int)((0L));
	R34 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R33, R34);
	BGL_RTL_IFNE(L39, CBOOL((V25)));
L41:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V27)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L42:	V27 = ((CDR(BGL_RTL_CAST(obj_t , (V27)))));
L43:	BGL_RTL_IFNE(L32, NULLP((V27)));
L44:	V35 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V27))))))));
	BGL_RTL_IFNE(L36, BGL_RTL_EQ((V35), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L45:	R30 = ((obj_t (*)())PROCEDURE_L_ENTRY((V3)))((V3), (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V27)))))), (V35), (V27));
L46:	V36 = (R30);
	BGL_RTL_IFNE(L38, BGL_RTL_EQ((V36), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L47:	BGL_RTL_IFNE(L40, BGL_RTL_EQ((V36), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L48:	BGL_RTL_IFNE(L34, BGL_RTL_EQ((V36), BGL_RTL_CAST(obj_t , (BGl_removestopz00zz__weakhashz00))));
L49:	return((((V36))));
L50:	return((((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))))));
L51:	R37 = BGL_RTL_CAST(obj_t , (V38));
	R39 = (CDR(BGL_RTL_CAST(obj_t , (V40))));
	SET_CDR(R37, R39);
	return((((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))))));
L52:	R41 = (int)((0L));
	R42 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R41, R42);
	BGL_RTL_IFNE(L51, CBOOL((V38)));
L53:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V40)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
	return((((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))))));
L54:	R43 = BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00));
	BGL_RTL_GO(L65);
L55:	V38 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V40 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V2))));
	BGL_RTL_GO(L61);
L56:	V38 = ((V40));
	V40 = ((CDR(BGL_RTL_CAST(obj_t , (V40)))));
	BGL_RTL_GO(L61);
L57:	R44 = BGL_RTL_CAST(obj_t , (V38));
	R45 = (CDR(BGL_RTL_CAST(obj_t , (V40))));
	SET_CDR(R44, R45);
	BGL_RTL_GO(L60);
L58:	R46 = (int)((0L));
	R47 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R46, R47);
	BGL_RTL_IFNE(L57, CBOOL((V38)));
L59:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V40)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L60:	V40 = ((CDR(BGL_RTL_CAST(obj_t , (V40)))));
L61:	BGL_RTL_IFNE(L50, NULLP((V40)));
L62:	V48 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V40))))))));
	V49 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V40))))))));
	BGL_RTL_IFNE(L54, BGL_RTL_EQ((V48), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L63:	BGL_RTL_IFNE(L54, BGL_RTL_EQ((V49), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L64:	R43 = ((obj_t (*)())PROCEDURE_L_ENTRY((V3)))((V3), (V48), (V49), (V40));
L65:	V50 = (R43);
	BGL_RTL_IFNE(L56, BGL_RTL_EQ((V50), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L66:	BGL_RTL_IFNE(L58, BGL_RTL_EQ((V50), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L67:	BGL_RTL_IFNE(L52, BGL_RTL_EQ((V50), BGL_RTL_CAST(obj_t , (BGl_removestopz00zz__weakhashz00))));
L68:	return(((((V50)))));
L69:	return(((((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00)))))));
L70:	R51 = BGL_RTL_CAST(obj_t , (V4));
	R52 = (CDR(BGL_RTL_CAST(obj_t , (V5))));
	SET_CDR(R51, R52);
	return(((((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00)))))));
L71:	R53 = (int)((0L));
	R54 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R53, R54);
	BGL_RTL_IFNE(L70, CBOOL((V4)));
L72:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V2), (CDR(BGL_RTL_CAST(obj_t , (V5)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
	return(((((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00)))))));

}

bool_t BGl_keyszd2traversezd2hashz00zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t V6;
 obj_t V5;
 obj_t V4;
 long V3;
 obj_t V2;
L0:	BGl_weakzd2keyszd2hashtablezd2filterz12zc0zz__weakhashz00((V0), (BGl_proc2116z00zz__weakhashz00));
	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = ((0L));
	BGL_RTL_IFNE(L8, BGL_RTL_EQ((V3), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)))));
L1:	V4 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V2)), (V3))));
	BGL_RTL_GO(L6);
L2:	BGL_RTL_CAST(obj_t , (BFALSE));
	BGL_RTL_GO(L5);
L3:	V5 = CAR(BGL_RTL_CAST(obj_t , (V4)));
	BGL_RTL_IFNE(L2, BGL_RTL_EQ((bgl_weakptr_data(BGL_RTL_CAST(obj_t , (V5)))), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L4:	V6 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (V5)));
	((obj_t (*)())PROCEDURE_ENTRY((V1)))((V1), (V6), (bgl_weakptr_ref(BGL_RTL_CAST(obj_t , (V5)))), BEOA);
L5:	V4 = ((CDR(BGL_RTL_CAST(obj_t , (V4)))));
L6:	BGL_RTL_IFNE(L3, PAIRP((V4)));
L7:	return(((((bool_t)1))));
L8:	return(((((bool_t)0))));

}

obj_t BGl_z62zc3z04anonymousza31301ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
L0:	return(BGL_RTL_CAST(obj_t , BBOOL((((bool_t)1)))));

}

bool_t BGl_oldzd2traversezd2hashz00zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t V41;
 obj_t V40;
 obj_t R39;
 obj_t R38;
 int R37;
 obj_t R36;
 obj_t R35;
 obj_t V34;
 obj_t V33;
 long V32;
 obj_t V31;
 obj_t V30;
 obj_t R29;
 obj_t R28;
 int R27;
 obj_t R26;
 obj_t R25;
 obj_t V24;
 obj_t V23;
 long V22;
 obj_t V21;
 obj_t V20;
 obj_t R19;
 obj_t R18;
 int R17;
 obj_t R16;
 obj_t R15;
 obj_t V14;
 obj_t V13;
 long V12;
 obj_t V11;
 obj_t R10;
 obj_t R9;
 int R8;
 obj_t R7;
 obj_t R6;
 obj_t V5;
 obj_t V4;
 long V3;
 obj_t V2;
L0:	BGL_RTL_IFNE(L15, BGL_RTL_EQ((1L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L1:	BGL_RTL_IFNE(L30, BGL_RTL_EQ((2L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L2:	BGL_RTL_IFNE(L45, BGL_RTL_EQ((3L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L3:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = ((0L));
	BGL_RTL_GO(L13);
L4:	V4 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V5 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V2)), (V3))));
	BGL_RTL_GO(L11);
L5:	R6 = BGL_RTL_CAST(obj_t , (V4));
	R7 = (CDR(BGL_RTL_CAST(obj_t , (V5))));
	SET_CDR(R6, R7);
	BGL_RTL_GO(L8);
L6:	R8 = (int)((0L));
	R9 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R8, R9);
	BGL_RTL_IFNE(L5, CBOOL((V4)));
L7:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V2)), (V3), (CDR(BGL_RTL_CAST(obj_t , (V5)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L8:	V5 = ((CDR(BGL_RTL_CAST(obj_t , (V5)))));
	BGL_RTL_GO(L11);
L9:	R10 = (((obj_t (*)())PROCEDURE_ENTRY((V1)))((V1), (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V5)))))), (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V5)))))), BEOA));
	BGL_RTL_IFNE(L6, BGL_RTL_EQ(R10, BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L10:	V4 = ((V5));
	V5 = ((CDR(BGL_RTL_CAST(obj_t , (V5)))));
L11:	BGL_RTL_IFEQ(L9, NULLP((V5)));
L12:	V3 = (BGL_RTL_ADD((V3), (1L)));
L13:	BGL_RTL_IFEQ(L4, BGL_RTL_EQ((V3), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)))));
L14:	return((((((((bool_t)0)))))));
L15:	V11 = STRUCT_REF((V0), (int)((2L)));
	V12 = ((0L));
	BGL_RTL_GO(L28);
L16:	V13 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V14 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V11)), (V12))));
	BGL_RTL_GO(L26);
L17:	R15 = BGL_RTL_CAST(obj_t , (V13));
	R16 = (CDR(BGL_RTL_CAST(obj_t , (V14))));
	SET_CDR(R15, R16);
	BGL_RTL_GO(L20);
L18:	R17 = (int)((0L));
	R18 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R17, R18);
	BGL_RTL_IFNE(L17, CBOOL((V13)));
L19:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V11)), (V12), (CDR(BGL_RTL_CAST(obj_t , (V14)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L20:	V14 = ((CDR(BGL_RTL_CAST(obj_t , (V14)))));
	BGL_RTL_GO(L26);
L21:	R19 = BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00));
	BGL_RTL_GO(L24);
L22:	V20 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V14))))))));
	BGL_RTL_IFNE(L21, BGL_RTL_EQ((V20), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L23:	R19 = ((obj_t (*)())PROCEDURE_ENTRY((V1)))((V1), (V20), (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V14)))))), BEOA);
L24:	BGL_RTL_IFNE(L18, BGL_RTL_EQ(((R19)), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L25:	V13 = ((V14));
	V14 = ((CDR(BGL_RTL_CAST(obj_t , (V14)))));
L26:	BGL_RTL_IFEQ(L22, NULLP((V14)));
L27:	V12 = (BGL_RTL_ADD((V12), (1L)));
L28:	BGL_RTL_IFEQ(L16, BGL_RTL_EQ((V12), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V11)))));
L29:	return(((((bool_t)0))));
L30:	V21 = STRUCT_REF((V0), (int)((2L)));
	V22 = ((0L));
	BGL_RTL_GO(L43);
L31:	V23 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V24 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V21)), (V22))));
	BGL_RTL_GO(L41);
L32:	R25 = BGL_RTL_CAST(obj_t , (V23));
	R26 = (CDR(BGL_RTL_CAST(obj_t , (V24))));
	SET_CDR(R25, R26);
	BGL_RTL_GO(L35);
L33:	R27 = (int)((0L));
	R28 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R27, R28);
	BGL_RTL_IFNE(L32, CBOOL((V23)));
L34:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V21)), (V22), (CDR(BGL_RTL_CAST(obj_t , (V24)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L35:	V24 = ((CDR(BGL_RTL_CAST(obj_t , (V24)))));
	BGL_RTL_GO(L41);
L36:	R29 = BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00));
	BGL_RTL_GO(L39);
L37:	V30 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V24))))))));
	BGL_RTL_IFNE(L36, BGL_RTL_EQ((V30), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L38:	R29 = ((obj_t (*)())PROCEDURE_ENTRY((V1)))((V1), (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V24)))))), (V30), BEOA);
L39:	BGL_RTL_IFNE(L33, BGL_RTL_EQ(((R29)), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L40:	V23 = ((V24));
	V24 = ((CDR(BGL_RTL_CAST(obj_t , (V24)))));
L41:	BGL_RTL_IFEQ(L37, NULLP((V24)));
L42:	V22 = (BGL_RTL_ADD((V22), (1L)));
L43:	BGL_RTL_IFEQ(L31, BGL_RTL_EQ((V22), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V21)))));
L44:	return((((((bool_t)0)))));
L45:	V31 = STRUCT_REF((V0), (int)((2L)));
	V32 = ((0L));
	BGL_RTL_GO(L59);
L46:	V33 = (BGL_RTL_CAST(obj_t , (BFALSE)));
	V34 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V31)), (V32))));
	BGL_RTL_GO(L57);
L47:	R35 = BGL_RTL_CAST(obj_t , (V33));
	R36 = (CDR(BGL_RTL_CAST(obj_t , (V34))));
	SET_CDR(R35, R36);
	BGL_RTL_GO(L50);
L48:	R37 = (int)((0L));
	R38 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R37, R38);
	BGL_RTL_IFNE(L47, CBOOL((V33)));
L49:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V31)), (V32), (CDR(BGL_RTL_CAST(obj_t , (V34)))));
	BGL_RTL_CAST(obj_t , BGL_RTL_NOP());
L50:	V34 = ((CDR(BGL_RTL_CAST(obj_t , (V34)))));
	BGL_RTL_GO(L57);
L51:	R39 = BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00));
	BGL_RTL_GO(L55);
L52:	V40 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V34))))))));
	V41 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CDR(BGL_RTL_CAST(obj_t , CAR(BGL_RTL_CAST(obj_t , (V34))))))));
	BGL_RTL_IFNE(L51, BGL_RTL_EQ((V40), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L53:	BGL_RTL_IFNE(L51, BGL_RTL_EQ((V41), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L54:	R39 = ((obj_t (*)())PROCEDURE_ENTRY((V1)))((V1), (V40), (V41), BEOA);
L55:	BGL_RTL_IFNE(L48, BGL_RTL_EQ(((R39)), BGL_RTL_CAST(obj_t , (BGl_removez00zz__weakhashz00))));
L56:	V33 = ((V34));
	V34 = ((CDR(BGL_RTL_CAST(obj_t , (V34)))));
L57:	BGL_RTL_IFEQ(L52, NULLP((V34)));
L58:	V32 = (BGL_RTL_ADD((V32), (1L)));
L59:	BGL_RTL_IFEQ(L46, BGL_RTL_EQ((V32), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V31)))));
L60:	return(((((((bool_t)0))))));

}

obj_t BGl_weakzd2hashtablezd2ze3vectorze3zz__weakhashz00(obj_t V0) {
 long V4;
 obj_t V3;
 obj_t V2;
 obj_t V1;
L0:	V1 = make_vector((BGl_hashtablezd2siza7ez75zz__hashz00((V0))), BGL_RTL_CAST(obj_t , (BUNSPEC)));
	V2 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , BINT((0L))));
	V3 = MAKE_FX_PROCEDURE((function_t) BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31362ze3ze5zz__weakhashz00), (int)((2L)), (int)((2L)));
	PROCEDURE_SET((V3), (int)((0L)), BGL_RTL_CAST(obj_t , (V1)));
	PROCEDURE_SET((V3), (int)((1L)), BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L5, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	BGl_oldzd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V3)));
L2:	V4 = BGl_hashtablezd2siza7ez75zz__hashz00((V0));
	BGL_RTL_IFNE(L4, BGL_RTL_LT((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V2)))), (V4)));
L3:	return(((V1)));
L4:	return((BGl_copyzd2vectorzd2zz__r4_vectors_6_8z00((V1), (long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V2)))))));
L5:	BGl_keyszd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V3)));
	BGL_RTL_GO(L2);

}

obj_t BGl_z62weakzd2hashtablezd2ze3vectorz81zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t R2;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2ze3vectorze3zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1)))));
L2:	R2 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((9857L))), BGL_RTL_CAST(obj_t , (BGl_string2118z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R2, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_z62zc3z04anonymousza31362ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t V3;
L0:	V3 = PROCEDURE_REF((V0), (int)((1L)));
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (BGL_RTL_CAST(obj_t , BGL_RTL_CAST(obj_t , PROCEDURE_REF((V0), (int)((0L))))))), ((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V3))))), (V2));
	BGL_RTL_BOXSET((V3), (BGL_RTL_CAST(obj_t , BINT(BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V3)))), (1L))))));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_NOP()));

}

obj_t BGl_weakzd2hashtablezd2ze3listze3zz__weakhashz00(obj_t V0) {
 obj_t V2;
 obj_t V1;
L0:	V1 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , (BNIL)));
	V2 = MAKE_FX_PROCEDURE((function_t) BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31370ze3ze5zz__weakhashz00), (int)((2L)), (int)((1L)));
	PROCEDURE_SET((V2), (int)((0L)), BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	BGl_oldzd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V2)));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V1))));
L2:	BGl_keyszd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V2)));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V1))));

}

obj_t BGl_z62weakzd2hashtablezd2ze3listz81zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t R2;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2ze3listze3zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1)))));
L2:	R2 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((10362L))), BGL_RTL_CAST(obj_t , (BGl_string2120z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R2, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_z62zc3z04anonymousza31370ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t V3;
L0:	V3 = PROCEDURE_REF((V0), (int)((0L)));
	BGL_RTL_BOXSET((V3), (BGL_RTL_CAST(obj_t , MAKE_YOUNG_PAIR((V2), BGL_RTL_BOXREF((V3))))));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_NOP()));

}

obj_t BGl_weakzd2hashtablezd2keyzd2listzd2zz__weakhashz00(obj_t V0) {
 obj_t V2;
 obj_t V1;
L0:	V1 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , (BNIL)));
	V2 = MAKE_FX_PROCEDURE((function_t) BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31372ze3ze5zz__weakhashz00), (int)((2L)), (int)((1L)));
	PROCEDURE_SET((V2), (int)((0L)), BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	BGl_oldzd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V2)));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V1))));
L2:	BGl_keyszd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V2)));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V1))));

}

obj_t BGl_z62weakzd2hashtablezd2keyzd2listzb0zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t R2;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2keyzd2listzd2zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1)))));
L2:	R2 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((10745L))), BGL_RTL_CAST(obj_t , (BGl_string2121z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R2, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_z62zc3z04anonymousza31372ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t V3;
L0:	V3 = PROCEDURE_REF((V0), (int)((0L)));
	BGL_RTL_BOXSET((V3), (BGL_RTL_CAST(obj_t , MAKE_YOUNG_PAIR((V1), BGL_RTL_BOXREF((V3))))));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_NOP()));

}

obj_t BGl_weakzd2hashtablezd2mapz00zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t V3;
 obj_t V2;
L0:	V2 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , (BNIL)));
	V3 = MAKE_FX_PROCEDURE((function_t) BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31374ze3ze5zz__weakhashz00), (int)((2L)), (int)((2L)));
	PROCEDURE_SET((V3), (int)((0L)), BGL_RTL_CAST(obj_t , (V1)));
	PROCEDURE_SET((V3), (int)((1L)), BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	BGl_oldzd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V3)));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V2))));
L2:	BGl_keyszd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V3)));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V2))));

}

obj_t BGl_z62weakzd2hashtablezd2mapz62zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R5;
 obj_t R4;
 obj_t R3;
L0:	BGL_RTL_IFEQ(L4, STRUCTP((V1)));
L1:	R3 = (BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFEQ(L3, PROCEDUREP((V2)));
L2:	return(BGl_weakzd2hashtablezd2mapz00zz__weakhashz00(R3, (BGL_RTL_CAST(obj_t , (V2)))));
L3:	R4 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((11138L))), BGL_RTL_CAST(obj_t , (BGl_string2122z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2123z00zz__weakhashz00)), (V2));
	BGL_RTL_FAIL(R4, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));
L4:	R5 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((11138L))), BGL_RTL_CAST(obj_t , (BGl_string2122z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R5, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_z62zc3z04anonymousza31374ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R5;
 obj_t R4;
 obj_t V3;
L0:	V3 = PROCEDURE_REF((V0), (int)((1L)));
	R4 = BGL_RTL_CAST(obj_t , (BGL_RTL_CAST(obj_t , PROCEDURE_REF((V0), (int)((0L))))));
	R5 = (((obj_t (*)())PROCEDURE_ENTRY(R4))(R4, (V1), (V2), BEOA));
	BGL_RTL_BOXSET((V3), (BGL_RTL_CAST(obj_t , MAKE_YOUNG_PAIR(R5, BGL_RTL_BOXREF((V3))))));
	return(BGL_RTL_CAST(obj_t , BGL_RTL_NOP()));

}

obj_t BGl_weakzd2hashtablezd2forzd2eachzd2zz__weakhashz00(obj_t V0, obj_t V1) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_oldzd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V1)))))));
L2:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_keyszd2traversezd2hashz00zz__weakhashz00((V0), BGL_RTL_CAST(obj_t , (V1)))))));

}

obj_t BGl_z62weakzd2hashtablezd2forzd2eachzb0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R5;
 obj_t R4;
 obj_t R3;
L0:	BGL_RTL_IFEQ(L4, STRUCTP((V1)));
L1:	R3 = (BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFEQ(L3, PROCEDUREP((V2)));
L2:	return(BGl_weakzd2hashtablezd2forzd2eachzd2zz__weakhashz00(R3, (BGL_RTL_CAST(obj_t , (V2)))));
L3:	R4 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((9474L))), BGL_RTL_CAST(obj_t , (BGl_string2124z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2123z00zz__weakhashz00)), (V2));
	BGL_RTL_FAIL(R4, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));
L4:	R5 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((9474L))), BGL_RTL_CAST(obj_t , (BGl_string2124z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R5, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

bool_t BGl_weakzd2keyszd2hashtablezd2filterz12zc0zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t R8;
 int R7;
 long R6;
 obj_t V5;
 obj_t V4;
 long V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = ((0L));
	BGL_RTL_GO(L2);
L1:	V4 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , BINT((0L))));
	V5 = MAKE_FX_PROCEDURE((function_t) BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31380ze3ze5zz__weakhashz00), (int)((1L)), (int)((2L)));
	PROCEDURE_SET((V5), (int)((0L)), BGL_RTL_CAST(obj_t , (V4)));
	PROCEDURE_SET((V5), (int)((1L)), BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V2)), (V3), BGL_RTL_CAST(obj_t , (BGl_filterz12z12zz__r4_control_features_6_9z00((V5), BGL_RTL_CAST(obj_t , (BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V2)), (V3))))))));
	R6 = (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L)))));
	R7 = (int)((0L));
	R8 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB(R6, (long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V4))))))));
	STRUCT_SET((V0), R7, R8);
	V3 = (BGL_RTL_ADD((V3), (1L)));
L2:	BGL_RTL_IFNE(L1, BGL_RTL_LT((V3), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)))));
L3:	return(((((bool_t)0))));

}

obj_t BGl_z62zc3z04anonymousza31380ze3ze5zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t V4;
 obj_t V3;
 obj_t V2;
L0:	V2 = PROCEDURE_REF((V0), (int)((0L)));
	V3 = PROCEDURE_REF((V0), (int)((1L)));
	BGL_RTL_IFNE(L3, BGL_RTL_EQ((bgl_weakptr_data(BGL_RTL_CAST(obj_t , (V1)))), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L1:	V4 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFEQ(L3, CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V4), (bgl_weakptr_ref(BGL_RTL_CAST(obj_t , (V1)))), BEOA)));
L2:	return(BGL_RTL_CAST(obj_t , BBOOL(((((bool_t)1))))));
L3:	BGL_RTL_BOXSET((V2), (BGL_RTL_CAST(obj_t , BINT(BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V2)))), (1L))))));
	return(BGL_RTL_CAST(obj_t , BBOOL(((((bool_t)0))))));

}

bool_t BGl_weakzd2oldzd2hashtablezd2filterz12zc0zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t V6;
 int R5;
 obj_t R4;
 long V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = ((0L));
	BGL_RTL_GO(L2);
L1:	R4 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31399ze3ze5zz__weakhashz00);
	R5 = (int)((1L));
	V6 = MAKE_L_PROCEDURE((function_t) R4, R5);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V6)), (int)((0L)), BGL_RTL_CAST(obj_t , (V1)));
	BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V2), (V3), (V6));
	V3 = (BGL_RTL_ADD((V3), (1L)));
L2:	BGL_RTL_IFNE(L1, BGL_RTL_LT((V3), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)))));
L3:	return(((((bool_t)0))));

}

obj_t BGl_z62zc3z04anonymousza31399ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 obj_t R4;
L0:	R4 = (PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L))));
	BGL_RTL_IFNE(L2, CBOOL(((obj_t (*)())PROCEDURE_ENTRY(R4))(R4, (V1), (V2), BEOA)));
L1:	return(((BGl_removez00zz__weakhashz00)));
L2:	return(((BGl_keepgoingz00zz__weakhashz00)));

}

obj_t BGl_weakzd2hashtablezd2filterz12z12zz__weakhashz00(obj_t V0, obj_t V1) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_weakzd2oldzd2hashtablezd2filterz12zc0zz__weakhashz00((V0), (V1))))));
L2:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_weakzd2keyszd2hashtablezd2filterz12zc0zz__weakhashz00((V0), (V1))))));

}

obj_t BGl_z62weakzd2hashtablezd2filterz12z70zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R5;
 obj_t R4;
 obj_t R3;
L0:	BGL_RTL_IFEQ(L4, STRUCTP((V1)));
L1:	R3 = (BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFEQ(L3, PROCEDUREP((V2)));
L2:	return(BGl_weakzd2hashtablezd2filterz12z12zz__weakhashz00(R3, (BGL_RTL_CAST(obj_t , (V2)))));
L3:	R4 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((13294L))), BGL_RTL_CAST(obj_t , (BGl_string2125z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2123z00zz__weakhashz00)), (V2));
	BGL_RTL_FAIL(R4, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));
L4:	R5 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((13294L))), BGL_RTL_CAST(obj_t , (BGl_string2125z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R5, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_weakzd2hashtablezd2clearz12z12zz__weakhashz00(obj_t V0) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_weakzd2oldzd2hashtablezd2filterz12zc0zz__weakhashz00((V0), (BGl_proc2127z00zz__weakhashz00))))));
L2:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_weakzd2keyszd2hashtablezd2filterz12zc0zz__weakhashz00((V0), (BGl_proc2126z00zz__weakhashz00))))));

}

obj_t BGl_z62weakzd2hashtablezd2clearz12z70zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t R2;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2clearz12z12zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1)))));
L2:	R2 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((13701L))), BGL_RTL_CAST(obj_t , (BGl_string2128z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R2, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_z62zc3z04anonymousza31406ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
L0:	return(BGL_RTL_CAST(obj_t , BBOOL((((bool_t)0)))));

}

obj_t BGl_z62zc3z04anonymousza31410ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
L0:	return(BGL_RTL_CAST(obj_t , BBOOL((((bool_t)0)))));

}

bool_t BGl_weakzd2keyszd2hashtablezd2containszf3z21zz__weakhashz00(obj_t V0, obj_t V1) {
 long V20;
 long R19;
 int32_t R18;
 int32_t R17;
 int32_t V16;
 long V15;
 char * R14;
 char * R13;
 bool_t R12;
 obj_t V11;
 obj_t V10;
 obj_t V9;
 long R8;
 long V7;
 long V6;
 long R5;
 long R4;
 obj_t V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L25, PROCEDUREP((V3)));
L1:	BGL_RTL_IFNE(L27, BGL_RTL_EQ((V3), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R4 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R5 = (R4);
L4:	V6 = ((R5));
	V7 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L23, BGL_RTL_EQ((((((V6)) | ((V7))) & -2147483648)), (0L)));
L5:	R8 = BGL_RTL_REM((V6), (V7));
L6:	V9 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V2)), ((R8)))));
	BGL_RTL_GO(L8);
L7:	V9 = ((CDR(BGL_RTL_CAST(obj_t , (V9)))));
L8:	BGL_RTL_IFNE(L15, NULLP((V9)));
L9:	V10 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V9))))));
	V11 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L16, PROCEDUREP((V11)));
L10:	BGL_RTL_IFNE(L17, BGL_RTL_EQ((V10), (V1)));
L11:	BGL_RTL_IFNE(L21, STRINGP((V10)));
L12:	R12 = (((bool_t)0));
L13:	BGL_RTL_IFEQ(L7, (R12));
L14:	return(((((bool_t)1))));
L15:	return(((((bool_t)0))));
L16:	R12 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V11)))((V11), (V10), (V1), BEOA));
	BGL_RTL_GO(L13);
L17:	R12 = (((bool_t)1));
	BGL_RTL_GO(L13);
L18:	R13 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V10)));
	R14 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R12 = BGL_RTL_EQ((long)((memcmp(R13, R14, (V15)))), (0L));
	BGL_RTL_GO(L13);
L19:	V15 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V10)));
	BGL_RTL_IFNE(L18, BGL_RTL_EQ((V15), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L20:	R12 = (((bool_t)0));
	BGL_RTL_GO(L13);
L21:	BGL_RTL_IFNE(L19, STRINGP((V1)));
L22:	R12 = (((bool_t)0));
	BGL_RTL_GO(L13);
L23:	V16 = (int32_t)((V6));
	R17 = ((int32_t)((V7)));
	R18 = (BGL_RTL_REM((V16), R17));
	R19 = ((long)(R18));
	R8 = (long)(R19);
	BGL_RTL_GO(L6);
L24:	R5 = NEG((V20));
	BGL_RTL_GO(L4);
L25:	V20 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V1), BEOA))));
	BGL_RTL_IFNE(L24, BGL_RTL_LT((V20), (0L)));
L26:	R5 = (V20);
	BGL_RTL_GO(L4);
L27:	R4 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

bool_t BGl_weakzd2oldzd2hashtablezd2containszf3z21zz__weakhashz00(obj_t V0, obj_t V1) {
 long V17;
 long R16;
 int32_t R15;
 int32_t R14;
 int32_t V13;
 obj_t R12;
 obj_t V11;
 int R10;
 obj_t R9;
 long R8;
 long V7;
 long V6;
 long R5;
 long R4;
 obj_t V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L11, PROCEDUREP((V3)));
L1:	BGL_RTL_IFNE(L13, BGL_RTL_EQ((V3), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R4 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R5 = (R4);
L4:	V6 = ((R5));
	V7 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L9, BGL_RTL_EQ((((((V6)) | ((V7))) & -2147483648)), (0L)));
L5:	R8 = BGL_RTL_REM((V6), (V7));
L6:	R9 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31422ze3ze5zz__weakhashz00);
	R10 = (int)((2L));
	V11 = MAKE_L_PROCEDURE((function_t) R9, R10);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V11)), (int)((0L)), BGL_RTL_CAST(obj_t , (V0)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V11)), (int)((1L)), (V1));
	R12 = (BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V2), ((R8)), (V11)));
	BGL_RTL_IFNE(L8, BGL_RTL_EQ(R12, BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L7:	return(((((bool_t)1))));
L8:	return(((((bool_t)0))));
L9:	V13 = (int32_t)((V6));
	R14 = ((int32_t)((V7)));
	R15 = (BGL_RTL_REM((V13), R14));
	R16 = ((long)(R15));
	R8 = (long)(R16);
	BGL_RTL_GO(L6);
L10:	R5 = NEG((V17));
	BGL_RTL_GO(L4);
L11:	V17 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V1), BEOA))));
	BGL_RTL_IFNE(L10, BGL_RTL_LT((V17), (0L)));
L12:	R5 = (V17);
	BGL_RTL_GO(L4);
L13:	R4 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_z62zc3z04anonymousza31422ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V10;
 char * R9;
 char * R8;
 bool_t R7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
L0:	V4 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L))));
	V5 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((1L)));
	V6 = STRUCT_REF((V4), (int)((3L)));
	BGL_RTL_IFNE(L7, PROCEDUREP((V6)));
L1:	BGL_RTL_IFNE(L8, BGL_RTL_EQ((V5), (V1)));
L2:	BGL_RTL_IFNE(L12, STRINGP((V5)));
L3:	R7 = (((bool_t)0));
L4:	BGL_RTL_IFNE(L6, (R7));
L5:	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L6:	return((BGL_RTL_CAST(obj_t , (BTRUE))));
L7:	R7 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V6)))((V6), (V5), (V1), BEOA));
	BGL_RTL_GO(L4);
L8:	R7 = (((bool_t)1));
	BGL_RTL_GO(L4);
L9:	R8 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V5)));
	R9 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R7 = BGL_RTL_EQ((long)((memcmp(R8, R9, (V10)))), (0L));
	BGL_RTL_GO(L4);
L10:	V10 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V5)));
	BGL_RTL_IFNE(L9, BGL_RTL_EQ((V10), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L11:	R7 = (((bool_t)0));
	BGL_RTL_GO(L4);
L12:	BGL_RTL_IFNE(L10, STRINGP((V1)));
L13:	R7 = (((bool_t)0));
	BGL_RTL_GO(L4);

}

bool_t BGl_weakzd2hashtablezd2containszf3zf3zz__weakhashz00(obj_t V0, obj_t V1) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGl_weakzd2oldzd2hashtablezd2containszf3z21zz__weakhashz00((V0), (V1))));
L2:	return((BGl_weakzd2keyszd2hashtablezd2containszf3z21zz__weakhashz00((V0), (V1))));

}

obj_t BGl_z62weakzd2hashtablezd2containszf3z91zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R3;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BBOOL(BGl_weakzd2hashtablezd2containszf3zf3zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1))), (V2))));
L2:	R3 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((15453L))), BGL_RTL_CAST(obj_t , (BGl_string2131z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R3, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_weakzd2keyszd2hashtablezd2getzd2zz__weakhashz00(obj_t V0, obj_t V1) {
 long V20;
 long R19;
 int32_t R18;
 int32_t R17;
 int32_t V16;
 long V15;
 char * R14;
 char * R13;
 bool_t R12;
 obj_t V11;
 obj_t V10;
 obj_t V9;
 long R8;
 long V7;
 long V6;
 long R5;
 long R4;
 obj_t V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L25, PROCEDUREP((V3)));
L1:	BGL_RTL_IFNE(L27, BGL_RTL_EQ((V3), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R4 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R5 = (R4);
L4:	V6 = ((R5));
	V7 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L23, BGL_RTL_EQ((((((V6)) | ((V7))) & -2147483648)), (0L)));
L5:	R8 = BGL_RTL_REM((V6), (V7));
L6:	V9 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V2)), ((R8)))));
	BGL_RTL_GO(L8);
L7:	V9 = ((CDR(BGL_RTL_CAST(obj_t , (V9)))));
L8:	BGL_RTL_IFNE(L15, NULLP((V9)));
L9:	V10 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V9))))));
	V11 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L16, PROCEDUREP((V11)));
L10:	BGL_RTL_IFNE(L17, BGL_RTL_EQ((V10), (V1)));
L11:	BGL_RTL_IFNE(L21, STRINGP((V10)));
L12:	R12 = (((bool_t)0));
L13:	BGL_RTL_IFEQ(L7, (R12));
L14:	return((bgl_weakptr_ref(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V9))))))));
L15:	return((BGL_RTL_CAST(obj_t , (BFALSE))));
L16:	R12 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V11)))((V11), (V10), (V1), BEOA));
	BGL_RTL_GO(L13);
L17:	R12 = (((bool_t)1));
	BGL_RTL_GO(L13);
L18:	R13 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V10)));
	R14 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R12 = BGL_RTL_EQ((long)((memcmp(R13, R14, (V15)))), (0L));
	BGL_RTL_GO(L13);
L19:	V15 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V10)));
	BGL_RTL_IFNE(L18, BGL_RTL_EQ((V15), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L20:	R12 = (((bool_t)0));
	BGL_RTL_GO(L13);
L21:	BGL_RTL_IFNE(L19, STRINGP((V1)));
L22:	R12 = (((bool_t)0));
	BGL_RTL_GO(L13);
L23:	V16 = (int32_t)((V6));
	R17 = ((int32_t)((V7)));
	R18 = (BGL_RTL_REM((V16), R17));
	R19 = ((long)(R18));
	R8 = (long)(R19);
	BGL_RTL_GO(L6);
L24:	R5 = NEG((V20));
	BGL_RTL_GO(L4);
L25:	V20 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V1), BEOA))));
	BGL_RTL_IFNE(L24, BGL_RTL_LT((V20), (0L)));
L26:	R5 = (V20);
	BGL_RTL_GO(L4);
L27:	R4 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_weakzd2oldzd2hashtablezd2getzd2zz__weakhashz00(obj_t V0, obj_t V1) {
 long V17;
 long R16;
 int32_t R15;
 int32_t R14;
 int32_t V13;
 obj_t V12;
 obj_t V11;
 int R10;
 obj_t R9;
 long R8;
 long V7;
 long V6;
 long R5;
 long R4;
 obj_t V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L11, PROCEDUREP((V3)));
L1:	BGL_RTL_IFNE(L13, BGL_RTL_EQ((V3), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R4 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R5 = (R4);
L4:	V6 = ((R5));
	V7 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L9, BGL_RTL_EQ((((((V6)) | ((V7))) & -2147483648)), (0L)));
L5:	R8 = BGL_RTL_REM((V6), (V7));
L6:	R9 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31438ze3ze5zz__weakhashz00);
	R10 = (int)((2L));
	V11 = MAKE_L_PROCEDURE((function_t) R9, R10);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V11)), (int)((0L)), BGL_RTL_CAST(obj_t , (V0)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V11)), (int)((1L)), (V1));
	V12 = BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V2), ((R8)), (V11));
	BGL_RTL_IFNE(L8, BGL_RTL_EQ((V12), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L7:	return(((V12)));
L8:	return((BGL_RTL_CAST(obj_t , (BFALSE))));
L9:	V13 = (int32_t)((V6));
	R14 = ((int32_t)((V7)));
	R15 = (BGL_RTL_REM((V13), R14));
	R16 = ((long)(R15));
	R8 = (long)(R16);
	BGL_RTL_GO(L6);
L10:	R5 = NEG((V17));
	BGL_RTL_GO(L4);
L11:	V17 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V1), BEOA))));
	BGL_RTL_IFNE(L10, BGL_RTL_LT((V17), (0L)));
L12:	R5 = (V17);
	BGL_RTL_GO(L4);
L13:	R4 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_z62zc3z04anonymousza31438ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V10;
 char * R9;
 char * R8;
 bool_t R7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
L0:	V4 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L))));
	V5 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((1L)));
	V6 = STRUCT_REF((V4), (int)((3L)));
	BGL_RTL_IFNE(L7, PROCEDUREP((V6)));
L1:	BGL_RTL_IFNE(L8, BGL_RTL_EQ((V5), (V1)));
L2:	BGL_RTL_IFNE(L12, STRINGP((V5)));
L3:	R7 = (((bool_t)0));
L4:	BGL_RTL_IFNE(L6, (R7));
L5:	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L6:	return(((V2)));
L7:	R7 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V6)))((V6), (V5), (V1), BEOA));
	BGL_RTL_GO(L4);
L8:	R7 = (((bool_t)1));
	BGL_RTL_GO(L4);
L9:	R8 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V5)));
	R9 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R7 = BGL_RTL_EQ((long)((memcmp(R8, R9, (V10)))), (0L));
	BGL_RTL_GO(L4);
L10:	V10 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V5)));
	BGL_RTL_IFNE(L9, BGL_RTL_EQ((V10), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L11:	R7 = (((bool_t)0));
	BGL_RTL_GO(L4);
L12:	BGL_RTL_IFNE(L10, STRINGP((V1)));
L13:	R7 = (((bool_t)0));
	BGL_RTL_GO(L4);

}

obj_t BGl_weakzd2hashtablezd2getz00zz__weakhashz00(obj_t V0, obj_t V1) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGl_weakzd2oldzd2hashtablezd2getzd2zz__weakhashz00((V0), (V1))));
L2:	return((BGl_weakzd2keyszd2hashtablezd2getzd2zz__weakhashz00((V0), (V1))));

}

obj_t BGl_z62weakzd2hashtablezd2getz62zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R3;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2getz00zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1))), (V2)));
L2:	R3 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((17200L))), BGL_RTL_CAST(obj_t , (BGl_string2132z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R3, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_weakzd2keyszd2hashtablezd2putz12zc0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 long V30;
 long R29;
 int32_t R28;
 int32_t R27;
 int32_t V26;
 long V25;
 char * R24;
 char * R23;
 obj_t R22;
 int R21;
 obj_t R20;
 int R19;
 obj_t V18;
 bool_t R17;
 obj_t V16;
 obj_t V15;
 obj_t V14;
 long V13;
 obj_t V12;
 obj_t V11;
 long V10;
 long R9;
 long V8;
 long V7;
 long R6;
 long R5;
 obj_t V4;
 obj_t V3;
L0:	V3 = STRUCT_REF((V0), (int)((2L)));
	V4 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L29, PROCEDUREP((V4)));
L1:	BGL_RTL_IFNE(L31, BGL_RTL_EQ((V4), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R5 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R6 = (R5);
L4:	V7 = ((R6));
	V8 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V3)));
	BGL_RTL_IFNE(L27, BGL_RTL_EQ((((((V7)) | ((V8))) & -2147483648)), (0L)));
L5:	R9 = BGL_RTL_REM((V7), (V8));
L6:	V10 = (R9);
	V11 = BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V3)), (V10));
	V12 = STRUCT_REF((V0), (int)((1L)));
	BGL_RTL_IFNE(L16, NULLP((V11)));
L7:	V13 = ((0L));
	V14 = ((V11));
	BGL_RTL_GO(L9);
L8:	V13 = ((BGL_RTL_ADD((V13), (1L))));
	V14 = ((CDR(BGL_RTL_CAST(obj_t , (V14)))));
L9:	BGL_RTL_IFNE(L18, NULLP((V14)));
L10:	V15 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V14))))));
	V16 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L20, PROCEDUREP((V16)));
L11:	BGL_RTL_IFNE(L21, BGL_RTL_EQ((V15), (V1)));
L12:	BGL_RTL_IFNE(L25, STRINGP((V15)));
L13:	R17 = (((bool_t)0));
L14:	BGL_RTL_IFEQ(L8, (R17));
L15:	V18 = bgl_weakptr_ref(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V14))))));
	bgl_weakptr_ref_set(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V14))))), (V2));
	return(((V18)));
L16:	R19 = (int)((0L));
	R20 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R19, R20);
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V3)), (V10), BGL_RTL_CAST(obj_t , (BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (bgl_make_weakptr((V1), (V2)))), BGL_RTL_CAST(obj_t , (BNIL))))))));
	return(((V2)));
L17:	BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0));
	return(((V2)));
L18:	R21 = (int)((0L));
	R22 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R21, R22);
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V3)), (V10), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (bgl_make_weakptr((V1), (V2)))), (V11)))));
	BGL_RTL_IFNE(L17, BGL_RTL_GT((V13), (long)CINT(BGL_RTL_CAST(obj_t , (V12)))));
L19:	BGL_RTL_CAST(obj_t , (BFALSE));
	return(((V2)));
L20:	R17 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V16)))((V16), (V15), (V1), BEOA));
	BGL_RTL_GO(L14);
L21:	R17 = (((bool_t)1));
	BGL_RTL_GO(L14);
L22:	R23 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V15)));
	R24 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R17 = BGL_RTL_EQ((long)((memcmp(R23, R24, (V25)))), (0L));
	BGL_RTL_GO(L14);
L23:	V25 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V15)));
	BGL_RTL_IFNE(L22, BGL_RTL_EQ((V25), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L24:	R17 = (((bool_t)0));
	BGL_RTL_GO(L14);
L25:	BGL_RTL_IFNE(L23, STRINGP((V1)));
L26:	R17 = (((bool_t)0));
	BGL_RTL_GO(L14);
L27:	V26 = (int32_t)((V7));
	R27 = ((int32_t)((V8)));
	R28 = (BGL_RTL_REM((V26), R27));
	R29 = ((long)(R28));
	R9 = (long)(R29);
	BGL_RTL_GO(L6);
L28:	R6 = NEG((V30));
	BGL_RTL_GO(L4);
L29:	V30 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V4)))((V4), (V1), BEOA))));
	BGL_RTL_IFNE(L28, BGL_RTL_LT((V30), (0L)));
L30:	R6 = (V30);
	BGL_RTL_GO(L4);
L31:	R5 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_weakzd2oldzd2hashtablezd2putz12zc0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 long V26;
 long R25;
 int32_t R24;
 int32_t R23;
 int32_t V22;
 obj_t V21;
 obj_t R20;
 int R19;
 obj_t R18;
 obj_t R17;
 obj_t V16;
 obj_t V15;
 int R14;
 obj_t R13;
 obj_t V12;
 obj_t V11;
 long V10;
 long R9;
 long V8;
 long V7;
 long R6;
 long R5;
 obj_t V4;
 obj_t V3;
L0:	V3 = STRUCT_REF((V0), (int)((2L)));
	V4 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L21, PROCEDUREP((V4)));
L1:	BGL_RTL_IFNE(L23, BGL_RTL_EQ((V4), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R5 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R6 = (R5);
L4:	V7 = ((R6));
	V8 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V3)));
	BGL_RTL_IFNE(L19, BGL_RTL_EQ((((((V7)) | ((V8))) & -2147483648)), (0L)));
L5:	R9 = BGL_RTL_REM((V7), (V8));
L6:	V10 = (R9);
	BGL_RTL_CAST(obj_t , (V3));
	V11 = STRUCT_REF((V0), (int)((1L)));
	V12 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , BINT((0L))));
	R13 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31480ze3ze5zz__weakhashz00);
	R14 = (int)((4L));
	V15 = MAKE_L_PROCEDURE((function_t) R13, R14);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V15)), (int)((0L)), BGL_RTL_CAST(obj_t , (V12)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V15)), (int)((1L)), (V2));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V15)), (int)((2L)), BGL_RTL_CAST(obj_t , (V0)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V15)), (int)((3L)), (V1));
	V16 = BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V3), (V10), (V15));
	BGL_RTL_IFNE(L13, BGL_RTL_EQ((V16), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L7:	return(((V16)));
L8:	BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0));
	return(((V2)));
L9:	BGL_RTL_IFNE(L8, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L10:	BGL_RTL_CAST(obj_t , BGl_weakzd2oldzd2hashtablezd2expandz12zc0zz__weakhashz00((V0)));
	return(((V2)));
L11:	R17 = BGL_RTL_CAST(obj_t , bgl_make_weakptr((V2), BGL_RTL_CAST(obj_t , (BFALSE))));
	BGL_RTL_GO(L17);
L12:	R18 = BGL_RTL_CAST(obj_t , bgl_make_weakptr((V1), BGL_RTL_CAST(obj_t , (BFALSE))));
	BGL_RTL_GO(L15);
L13:	R19 = (int)((0L));
	R20 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R19, R20);
	BGL_RTL_IFNE(L12, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L14:	R18 = (V1);
L15:	V21 = (R18);
	BGL_RTL_IFNE(L11, BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00((V0)));
L16:	R17 = (V2);
L17:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V3)), (V10), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V21), ((R17))))), (BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (STRUCT_REF((V0), (int)((2L))))), (V10)))))));
	BGL_RTL_IFNE(L9, BGL_RTL_GT((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V12)))), (long)CINT(BGL_RTL_CAST(obj_t , (V11)))));
L18:	BGL_RTL_CAST(obj_t , (BFALSE));
	return(((V2)));
L19:	V22 = (int32_t)((V7));
	R23 = ((int32_t)((V8)));
	R24 = (BGL_RTL_REM((V22), R23));
	R25 = ((long)(R24));
	R9 = (long)(R25);
	BGL_RTL_GO(L6);
L20:	R6 = NEG((V26));
	BGL_RTL_GO(L4);
L21:	V26 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V4)))((V4), (V1), BEOA))));
	BGL_RTL_IFNE(L20, BGL_RTL_LT((V26), (0L)));
L22:	R6 = (V26);
	BGL_RTL_GO(L4);
L23:	R5 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_z62zc3z04anonymousza31480ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V16;
 char * R15;
 char * R14;
 obj_t R13;
 obj_t R12;
 obj_t V11;
 obj_t R10;
 bool_t R9;
 obj_t V8;
 obj_t V7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
L0:	V4 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L)));
	V5 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((1L)));
	V6 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((2L))));
	V7 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((3L)));
	BGL_RTL_BOXSET((V4), (BGL_RTL_CAST(obj_t , BINT(BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V4)))), (1L))))));
	V8 = STRUCT_REF((V6), (int)((3L)));
	BGL_RTL_IFNE(L9, PROCEDUREP((V8)));
L1:	BGL_RTL_IFNE(L10, BGL_RTL_EQ((V1), (V7)));
L2:	BGL_RTL_IFNE(L14, STRINGP((V1)));
L3:	R9 = (((bool_t)0));
L4:	BGL_RTL_IFNE(L7, (R9));
L5:	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L6:	R10 = BGL_RTL_CAST(obj_t , (V11));
	R12 = ((BGL_RTL_CAST(obj_t , bgl_make_weakptr((V5), BGL_RTL_CAST(obj_t , (BFALSE))))));
	SET_CDR(R10, R12);
	return(((V2)));
L7:	V11 = CAR(BGL_RTL_CAST(obj_t , (V3)));
	BGL_RTL_IFNE(L6, BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00((V6)));
L8:	R13 = BGL_RTL_CAST(obj_t , (V11));
	SET_CDR(R13, (((V5))));
	return(((V2)));
L9:	R9 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V8)))((V8), (V1), (V7), BEOA));
	BGL_RTL_GO(L4);
L10:	R9 = (((bool_t)1));
	BGL_RTL_GO(L4);
L11:	R14 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R15 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V7)));
	R9 = BGL_RTL_EQ((long)((memcmp(R14, R15, (V16)))), (0L));
	BGL_RTL_GO(L4);
L12:	V16 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFNE(L11, BGL_RTL_EQ((V16), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V7)))));
L13:	R9 = (((bool_t)0));
	BGL_RTL_GO(L4);
L14:	BGL_RTL_IFNE(L12, STRINGP((V7)));
L15:	R9 = (((bool_t)0));
	BGL_RTL_GO(L4);

}

obj_t BGl_weakzd2hashtablezd2putz12z12zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGl_weakzd2oldzd2hashtablezd2putz12zc0zz__weakhashz00((V0), (V1), (V2))));
L2:	return((BGl_weakzd2keyszd2hashtablezd2putz12zc0zz__weakhashz00((V0), (V1), (V2))));

}

obj_t BGl_z62weakzd2hashtablezd2putz12z70zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 obj_t R4;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2putz12z12zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1))), (V2), (V3)));
L2:	R4 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((20423L))), BGL_RTL_CAST(obj_t , (BGl_string2133z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R4, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_weakzd2keyszd2hashtablezd2updatez12zc0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V32;
 long R31;
 int32_t R30;
 int32_t R29;
 int32_t V28;
 long V27;
 char * R26;
 char * R25;
 obj_t R24;
 int R23;
 obj_t R22;
 int R21;
 obj_t V20;
 obj_t R19;
 bool_t R18;
 obj_t V17;
 obj_t V16;
 obj_t V15;
 long V14;
 obj_t V13;
 obj_t V12;
 long V11;
 long R10;
 long V9;
 long V8;
 long R7;
 long R6;
 obj_t V5;
 obj_t V4;
L0:	V4 = STRUCT_REF((V0), (int)((2L)));
	V5 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L29, PROCEDUREP((V5)));
L1:	BGL_RTL_IFNE(L31, BGL_RTL_EQ((V5), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R6 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R7 = (R6);
L4:	V8 = ((R7));
	V9 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V4)));
	BGL_RTL_IFNE(L27, BGL_RTL_EQ((((((V8)) | ((V9))) & -2147483648)), (0L)));
L5:	R10 = BGL_RTL_REM((V8), (V9));
L6:	V11 = (R10);
	V12 = BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V4)), (V11));
	V13 = STRUCT_REF((V0), (int)((1L)));
	BGL_RTL_IFNE(L16, NULLP((V12)));
L7:	V14 = ((0L));
	V15 = ((V12));
	BGL_RTL_GO(L9);
L8:	V14 = ((BGL_RTL_ADD((V14), (1L))));
	V15 = ((CDR(BGL_RTL_CAST(obj_t , (V15)))));
L9:	BGL_RTL_IFNE(L18, NULLP((V15)));
L10:	V16 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V15))))));
	V17 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L20, PROCEDUREP((V17)));
L11:	BGL_RTL_IFNE(L21, BGL_RTL_EQ((V16), (V1)));
L12:	BGL_RTL_IFNE(L25, STRINGP((V16)));
L13:	R18 = (((bool_t)0));
L14:	BGL_RTL_IFEQ(L8, (R18));
L15:	R19 = BGL_RTL_CAST(obj_t , (V2));
	V20 = ((obj_t (*)())PROCEDURE_ENTRY(R19))(R19, (bgl_weakptr_ref(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V15))))))), BEOA);
	bgl_weakptr_ref_set(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V15))))), (V20));
	return(((V20)));
L16:	R21 = (int)((0L));
	R22 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R21, R22);
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V4)), (V11), BGL_RTL_CAST(obj_t , (BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V1), (V3)))), BGL_RTL_CAST(obj_t , (BNIL))))))));
	return(((V3)));
L17:	BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0));
	return(((V3)));
L18:	R23 = (int)((0L));
	R24 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R23, R24);
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V4)), (V11), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (bgl_make_weakptr((V1), (V3)))), (V12)))));
	BGL_RTL_IFNE(L17, BGL_RTL_GT((V14), (long)CINT(BGL_RTL_CAST(obj_t , (V13)))));
L19:	BGL_RTL_CAST(obj_t , (BFALSE));
	return(((V3)));
L20:	R18 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V17)))((V17), (V16), (V1), BEOA));
	BGL_RTL_GO(L14);
L21:	R18 = (((bool_t)1));
	BGL_RTL_GO(L14);
L22:	R25 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V16)));
	R26 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R18 = BGL_RTL_EQ((long)((memcmp(R25, R26, (V27)))), (0L));
	BGL_RTL_GO(L14);
L23:	V27 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V16)));
	BGL_RTL_IFNE(L22, BGL_RTL_EQ((V27), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L24:	R18 = (((bool_t)0));
	BGL_RTL_GO(L14);
L25:	BGL_RTL_IFNE(L23, STRINGP((V1)));
L26:	R18 = (((bool_t)0));
	BGL_RTL_GO(L14);
L27:	V28 = (int32_t)((V8));
	R29 = ((int32_t)((V9)));
	R30 = (BGL_RTL_REM((V28), R29));
	R31 = ((long)(R30));
	R10 = (long)(R31);
	BGL_RTL_GO(L6);
L28:	R7 = NEG((V32));
	BGL_RTL_GO(L4);
L29:	V32 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V5)))((V5), (V1), BEOA))));
	BGL_RTL_IFNE(L28, BGL_RTL_LT((V32), (0L)));
L30:	R7 = (V32);
	BGL_RTL_GO(L4);
L31:	R6 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_weakzd2oldzd2hashtablezd2updatez12zc0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V27;
 long R26;
 int32_t R25;
 int32_t R24;
 int32_t V23;
 obj_t V22;
 obj_t R21;
 int R20;
 obj_t R19;
 obj_t R18;
 obj_t V17;
 obj_t V16;
 int R15;
 obj_t R14;
 obj_t V13;
 obj_t V12;
 long V11;
 long R10;
 long V9;
 long V8;
 long R7;
 long R6;
 obj_t V5;
 obj_t V4;
L0:	V4 = STRUCT_REF((V0), (int)((2L)));
	V5 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L21, PROCEDUREP((V5)));
L1:	BGL_RTL_IFNE(L23, BGL_RTL_EQ((V5), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R6 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R7 = (R6);
L4:	V8 = ((R7));
	V9 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V4)));
	BGL_RTL_IFNE(L19, BGL_RTL_EQ((((((V8)) | ((V9))) & -2147483648)), (0L)));
L5:	R10 = BGL_RTL_REM((V8), (V9));
L6:	V11 = (R10);
	BGL_RTL_CAST(obj_t , (V4));
	V12 = STRUCT_REF((V0), (int)((1L)));
	V13 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , BINT((0L))));
	R14 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31527ze3ze5zz__weakhashz00);
	R15 = (int)((4L));
	V16 = MAKE_L_PROCEDURE((function_t) R14, R15);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V16)), (int)((0L)), BGL_RTL_CAST(obj_t , (V13)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V16)), (int)((1L)), BGL_RTL_CAST(obj_t , (V2)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V16)), (int)((2L)), BGL_RTL_CAST(obj_t , (V0)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V16)), (int)((3L)), (V1));
	V17 = BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V4), (V11), (V16));
	BGL_RTL_IFNE(L13, BGL_RTL_EQ((V17), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L7:	return(((V17)));
L8:	BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0));
	return(((V3)));
L9:	BGL_RTL_IFNE(L8, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L10:	BGL_RTL_CAST(obj_t , BGl_weakzd2oldzd2hashtablezd2expandz12zc0zz__weakhashz00((V0)));
	return(((V3)));
L11:	R18 = BGL_RTL_CAST(obj_t , bgl_make_weakptr((V3), BGL_RTL_CAST(obj_t , (BFALSE))));
	BGL_RTL_GO(L17);
L12:	R19 = BGL_RTL_CAST(obj_t , bgl_make_weakptr((V1), BGL_RTL_CAST(obj_t , (BFALSE))));
	BGL_RTL_GO(L15);
L13:	R20 = (int)((0L));
	R21 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R20, R21);
	BGL_RTL_IFNE(L12, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L14:	R19 = (V1);
L15:	V22 = (R19);
	BGL_RTL_IFNE(L11, BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00((V0)));
L16:	R18 = (V3);
L17:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V4)), (V11), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V22), ((R18))))), (BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (STRUCT_REF((V0), (int)((2L))))), (V11)))))));
	BGL_RTL_IFNE(L9, BGL_RTL_GT((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V13)))), (long)CINT(BGL_RTL_CAST(obj_t , (V12)))));
L18:	BGL_RTL_CAST(obj_t , (BFALSE));
	return(((V3)));
L19:	V23 = (int32_t)((V8));
	R24 = ((int32_t)((V9)));
	R25 = (BGL_RTL_REM((V23), R24));
	R26 = ((long)(R25));
	R10 = (long)(R26);
	BGL_RTL_GO(L6);
L20:	R7 = NEG((V27));
	BGL_RTL_GO(L4);
L21:	V27 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V5)))((V5), (V1), BEOA))));
	BGL_RTL_IFNE(L20, BGL_RTL_LT((V27), (0L)));
L22:	R7 = (V27);
	BGL_RTL_GO(L4);
L23:	R6 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_z62zc3z04anonymousza31527ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V18;
 char * R17;
 char * R16;
 obj_t R15;
 obj_t R14;
 obj_t V13;
 obj_t R12;
 obj_t V11;
 obj_t R10;
 bool_t R9;
 obj_t V8;
 obj_t V7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
L0:	V4 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L)));
	V5 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((1L))));
	V6 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((2L))));
	V7 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((3L)));
	BGL_RTL_BOXSET((V4), (BGL_RTL_CAST(obj_t , BINT(BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V4)))), (1L))))));
	V8 = STRUCT_REF((V6), (int)((3L)));
	BGL_RTL_IFNE(L9, PROCEDUREP((V8)));
L1:	BGL_RTL_IFNE(L10, BGL_RTL_EQ((V1), (V7)));
L2:	BGL_RTL_IFNE(L14, STRINGP((V1)));
L3:	R9 = (((bool_t)0));
L4:	BGL_RTL_IFNE(L7, (R9));
L5:	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L6:	R10 = BGL_RTL_CAST(obj_t , (V11));
	R12 = ((BGL_RTL_CAST(obj_t , bgl_make_weakptr((V13), BGL_RTL_CAST(obj_t , (BFALSE))))));
	SET_CDR(R10, R12);
	return(((V13)));
L7:	R14 = BGL_RTL_CAST(obj_t , (V5));
	V13 = ((obj_t (*)())PROCEDURE_ENTRY(R14))(R14, (V2), BEOA);
	V11 = CAR(BGL_RTL_CAST(obj_t , (V3)));
	BGL_RTL_IFNE(L6, BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00((V6)));
L8:	R15 = BGL_RTL_CAST(obj_t , (V11));
	SET_CDR(R15, (((V13))));
	return(((V13)));
L9:	R9 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V8)))((V8), (V1), (V7), BEOA));
	BGL_RTL_GO(L4);
L10:	R9 = (((bool_t)1));
	BGL_RTL_GO(L4);
L11:	R16 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R17 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V7)));
	R9 = BGL_RTL_EQ((long)((memcmp(R16, R17, (V18)))), (0L));
	BGL_RTL_GO(L4);
L12:	V18 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFNE(L11, BGL_RTL_EQ((V18), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V7)))));
L13:	R9 = (((bool_t)0));
	BGL_RTL_GO(L4);
L14:	BGL_RTL_IFNE(L12, STRINGP((V7)));
L15:	R9 = (((bool_t)0));
	BGL_RTL_GO(L4);

}

obj_t BGl_weakzd2hashtablezd2updatez12z12zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGl_weakzd2oldzd2hashtablezd2updatez12zc0zz__weakhashz00((V0), (V1), (V2), (V3))));
L2:	return((BGl_weakzd2keyszd2hashtablezd2updatez12zc0zz__weakhashz00((V0), (V1), (V2), (V3))));

}

obj_t BGl_z62weakzd2hashtablezd2updatez12z70zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3, obj_t V4) {
 obj_t R8;
 obj_t R7;
 obj_t R6;
 obj_t R5;
L0:	BGL_RTL_IFEQ(L4, STRUCTP((V1)));
L1:	R5 = (BGL_RTL_CAST(obj_t , (V1)));
	R6 = (V2);
	BGL_RTL_IFEQ(L3, PROCEDUREP((V3)));
L2:	return(BGl_weakzd2hashtablezd2updatez12z12zz__weakhashz00(R5, R6, (BGL_RTL_CAST(obj_t , (V3))), (V4)));
L3:	R7 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((23717L))), BGL_RTL_CAST(obj_t , (BGl_string2134z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2123z00zz__weakhashz00)), (V3));
	BGL_RTL_FAIL(R7, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));
L4:	R8 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((23717L))), BGL_RTL_CAST(obj_t , (BGl_string2134z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R8, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_weakzd2keyszd2hashtablezd2addz12zc0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3, obj_t V4) {
 long V37;
 long R36;
 int32_t R35;
 int32_t R34;
 int32_t V33;
 long V32;
 char * R31;
 char * R30;
 obj_t R29;
 int R28;
 obj_t R27;
 obj_t V26;
 obj_t R25;
 int R24;
 obj_t V23;
 obj_t R22;
 obj_t V21;
 obj_t R20;
 bool_t R19;
 obj_t V18;
 obj_t V17;
 obj_t V16;
 long V15;
 obj_t V14;
 obj_t V13;
 long V12;
 long R11;
 long V10;
 long V9;
 long R8;
 long R7;
 obj_t V6;
 obj_t V5;
L0:	V5 = STRUCT_REF((V0), (int)((2L)));
	V6 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L29, PROCEDUREP((V6)));
L1:	BGL_RTL_IFNE(L31, BGL_RTL_EQ((V6), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R7 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R8 = (R7);
L4:	V9 = ((R8));
	V10 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V5)));
	BGL_RTL_IFNE(L27, BGL_RTL_EQ((((((V9)) | ((V10))) & -2147483648)), (0L)));
L5:	R11 = BGL_RTL_REM((V9), (V10));
L6:	V12 = (R11);
	V13 = BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V5)), (V12));
	V14 = STRUCT_REF((V0), (int)((1L)));
	BGL_RTL_IFNE(L16, NULLP((V13)));
L7:	V15 = ((0L));
	V16 = ((V13));
	BGL_RTL_GO(L9);
L8:	V15 = ((BGL_RTL_ADD((V15), (1L))));
	V16 = ((CDR(BGL_RTL_CAST(obj_t , (V16)))));
L9:	BGL_RTL_IFNE(L18, NULLP((V16)));
L10:	V17 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V16))))));
	V18 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L20, PROCEDUREP((V18)));
L11:	BGL_RTL_IFNE(L21, BGL_RTL_EQ((V17), (V1)));
L12:	BGL_RTL_IFNE(L25, STRINGP((V17)));
L13:	R19 = (((bool_t)0));
L14:	BGL_RTL_IFEQ(L8, (R19));
L15:	R20 = BGL_RTL_CAST(obj_t , (V2));
	V21 = ((obj_t (*)())PROCEDURE_ENTRY(R20))(R20, (V3), (bgl_weakptr_ref(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V16))))))), BEOA);
	bgl_weakptr_ref_set(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V16))))), (V21));
	return(((V21)));
L16:	R22 = BGL_RTL_CAST(obj_t , (V2));
	V23 = ((obj_t (*)())PROCEDURE_ENTRY(R22))(R22, (V3), (V4), BEOA);
	R24 = (int)((0L));
	R25 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R24, R25);
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V5)), (V12), BGL_RTL_CAST(obj_t , (BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V1), (V23)))), BGL_RTL_CAST(obj_t , (BNIL))))))));
	return(((V23)));
L17:	BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0));
	return(((V26)));
L18:	R27 = BGL_RTL_CAST(obj_t , (V2));
	V26 = ((obj_t (*)())PROCEDURE_ENTRY(R27))(R27, (V3), (V4), BEOA);
	R28 = (int)((0L));
	R29 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R28, R29);
	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V5)), (V12), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (bgl_make_weakptr((V1), (V26)))), (V13)))));
	BGL_RTL_IFNE(L17, BGL_RTL_GT((V15), (long)CINT(BGL_RTL_CAST(obj_t , (V14)))));
L19:	BGL_RTL_CAST(obj_t , (BFALSE));
	return(((V26)));
L20:	R19 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V18)))((V18), (V17), (V1), BEOA));
	BGL_RTL_GO(L14);
L21:	R19 = (((bool_t)1));
	BGL_RTL_GO(L14);
L22:	R30 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V17)));
	R31 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R19 = BGL_RTL_EQ((long)((memcmp(R30, R31, (V32)))), (0L));
	BGL_RTL_GO(L14);
L23:	V32 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V17)));
	BGL_RTL_IFNE(L22, BGL_RTL_EQ((V32), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L24:	R19 = (((bool_t)0));
	BGL_RTL_GO(L14);
L25:	BGL_RTL_IFNE(L23, STRINGP((V1)));
L26:	R19 = (((bool_t)0));
	BGL_RTL_GO(L14);
L27:	V33 = (int32_t)((V9));
	R34 = ((int32_t)((V10)));
	R35 = (BGL_RTL_REM((V33), R34));
	R36 = ((long)(R35));
	R11 = (long)(R36);
	BGL_RTL_GO(L6);
L28:	R8 = NEG((V37));
	BGL_RTL_GO(L4);
L29:	V37 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V6)))((V6), (V1), BEOA))));
	BGL_RTL_IFNE(L28, BGL_RTL_LT((V37), (0L)));
L30:	R8 = (V37);
	BGL_RTL_GO(L4);
L31:	R7 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_weakzd2oldzd2hashtablezd2addz12zc0zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3, obj_t V4) {
 long V31;
 long R30;
 int32_t R29;
 int32_t R28;
 int32_t V27;
 obj_t R26;
 int R25;
 obj_t R24;
 obj_t R23;
 obj_t R22;
 obj_t V21;
 obj_t R20;
 obj_t V19;
 obj_t V18;
 obj_t V17;
 int R16;
 obj_t R15;
 obj_t V14;
 obj_t V13;
 long V12;
 long R11;
 long V10;
 long V9;
 long R8;
 long R7;
 obj_t V6;
 obj_t V5;
L0:	V5 = STRUCT_REF((V0), (int)((2L)));
	V6 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L21, PROCEDUREP((V6)));
L1:	BGL_RTL_IFNE(L23, BGL_RTL_EQ((V6), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R7 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R8 = (R7);
L4:	V9 = ((R8));
	V10 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V5)));
	BGL_RTL_IFNE(L19, BGL_RTL_EQ((((((V9)) | ((V10))) & -2147483648)), (0L)));
L5:	R11 = BGL_RTL_REM((V9), (V10));
L6:	V12 = (R11);
	BGL_RTL_CAST(obj_t , (V5));
	V13 = STRUCT_REF((V0), (int)((1L)));
	V14 = BGL_RTL_MAKEBOX(BGL_RTL_CAST(obj_t , BINT((0L))));
	R15 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31586ze3ze5zz__weakhashz00);
	R16 = (int)((4L));
	V17 = MAKE_L_PROCEDURE((function_t) R15, R16);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V17)), (int)((0L)), BGL_RTL_CAST(obj_t , (V14)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V17)), (int)((1L)), BGL_RTL_CAST(obj_t , (V2)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V17)), (int)((2L)), BGL_RTL_CAST(obj_t , (V0)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V17)), (int)((3L)), (V1));
	V18 = BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V5), (V12), (V17));
	BGL_RTL_IFNE(L13, BGL_RTL_EQ((V18), BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L7:	return(((V18)));
L8:	BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0));
	return(((V19)));
L9:	BGL_RTL_IFNE(L8, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L10:	BGL_RTL_CAST(obj_t , BGl_weakzd2oldzd2hashtablezd2expandz12zc0zz__weakhashz00((V0)));
	return(((V19)));
L11:	R20 = BGL_RTL_CAST(obj_t , bgl_make_weakptr((V1), BGL_RTL_CAST(obj_t , (BFALSE))));
	BGL_RTL_GO(L17);
L12:	V21 = bgl_make_weakptr((V3), BGL_RTL_CAST(obj_t , (BFALSE)));
	R22 = BGL_RTL_CAST(obj_t , (V2));
	R23 = ((obj_t (*)())PROCEDURE_ENTRY(R22))(R22, BGL_RTL_CAST(obj_t , (V21)), BGL_RTL_CAST(obj_t , (bgl_make_weakptr((V4), BGL_RTL_CAST(obj_t , (BFALSE))))), BEOA);
	BGL_RTL_GO(L15);
L13:	BGL_RTL_IFNE(L12, BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00((V0)));
L14:	R24 = BGL_RTL_CAST(obj_t , (V2));
	R23 = ((obj_t (*)())PROCEDURE_ENTRY(R24))(R24, (V3), (V4), BEOA);
L15:	V19 = (R23);
	R25 = (int)((0L));
	R26 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R25, R26);
	BGL_RTL_IFNE(L11, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L16:	R20 = (V1);
L17:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V5)), (V12), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(((R20)), (V19)))), (BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (STRUCT_REF((V0), (int)((2L))))), (V12)))))));
	BGL_RTL_IFNE(L9, BGL_RTL_GT((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V14)))), (long)CINT(BGL_RTL_CAST(obj_t , (V13)))));
L18:	BGL_RTL_CAST(obj_t , (BFALSE));
	return(((V19)));
L19:	V27 = (int32_t)((V9));
	R28 = ((int32_t)((V10)));
	R29 = (BGL_RTL_REM((V27), R28));
	R30 = ((long)(R29));
	R11 = (long)(R30);
	BGL_RTL_GO(L6);
L20:	R8 = NEG((V31));
	BGL_RTL_GO(L4);
L21:	V31 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V6)))((V6), (V1), BEOA))));
	BGL_RTL_IFNE(L20, BGL_RTL_LT((V31), (0L)));
L22:	R8 = (V31);
	BGL_RTL_GO(L4);
L23:	R7 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_z62zc3z04anonymousza31586ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V18;
 char * R17;
 char * R16;
 obj_t R15;
 obj_t R14;
 obj_t V13;
 obj_t R12;
 obj_t V11;
 obj_t R10;
 bool_t R9;
 obj_t V8;
 obj_t V7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
L0:	V4 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L)));
	V5 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((1L))));
	V6 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((2L))));
	V7 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((3L)));
	BGL_RTL_BOXSET((V4), (BGL_RTL_CAST(obj_t , BINT(BGL_RTL_ADD((long)CINT(BGL_RTL_CAST(obj_t , BGL_RTL_BOXREF((V4)))), (1L))))));
	V8 = STRUCT_REF((V6), (int)((3L)));
	BGL_RTL_IFNE(L9, PROCEDUREP((V8)));
L1:	BGL_RTL_IFNE(L10, BGL_RTL_EQ((V1), (V7)));
L2:	BGL_RTL_IFNE(L14, STRINGP((V1)));
L3:	R9 = (((bool_t)0));
L4:	BGL_RTL_IFNE(L7, (R9));
L5:	return((BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L6:	R10 = BGL_RTL_CAST(obj_t , (V11));
	R12 = ((BGL_RTL_CAST(obj_t , bgl_make_weakptr((V13), BGL_RTL_CAST(obj_t , (BFALSE))))));
	SET_CDR(R10, R12);
	return(((V13)));
L7:	R14 = BGL_RTL_CAST(obj_t , (V5));
	V13 = ((obj_t (*)())PROCEDURE_ENTRY(R14))(R14, (V2), BEOA);
	V11 = CAR(BGL_RTL_CAST(obj_t , (V3)));
	BGL_RTL_IFNE(L6, BGl_hashtablezd2weakzd2datazf3zf3zz__hashz00((V6)));
L8:	R15 = BGL_RTL_CAST(obj_t , (V11));
	SET_CDR(R15, (((V13))));
	return(((V13)));
L9:	R9 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V8)))((V8), (V1), (V7), BEOA));
	BGL_RTL_GO(L4);
L10:	R9 = (((bool_t)1));
	BGL_RTL_GO(L4);
L11:	R16 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R17 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V7)));
	R9 = BGL_RTL_EQ((long)((memcmp(R16, R17, (V18)))), (0L));
	BGL_RTL_GO(L4);
L12:	V18 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)));
	BGL_RTL_IFNE(L11, BGL_RTL_EQ((V18), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V7)))));
L13:	R9 = (((bool_t)0));
	BGL_RTL_GO(L4);
L14:	BGL_RTL_IFNE(L12, STRINGP((V7)));
L15:	R9 = (((bool_t)0));
	BGL_RTL_GO(L4);

}

obj_t BGl_weakzd2hashtablezd2addz12z12zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3, obj_t V4) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGl_weakzd2oldzd2hashtablezd2addz12zc0zz__weakhashz00((V0), (V1), (V2), (V3), (V4))));
L2:	return((BGl_weakzd2keyszd2hashtablezd2addz12zc0zz__weakhashz00((V0), (V1), (V2), (V3), (V4))));

}

obj_t BGl_z62weakzd2hashtablezd2addz12z70zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3, obj_t V4, obj_t V5) {
 obj_t R9;
 obj_t R8;
 obj_t R7;
 obj_t R6;
L0:	BGL_RTL_IFEQ(L4, STRUCTP((V1)));
L1:	R6 = (BGL_RTL_CAST(obj_t , (V1)));
	R7 = (V2);
	BGL_RTL_IFEQ(L3, PROCEDUREP((V3)));
L2:	return(BGl_weakzd2hashtablezd2addz12z12zz__weakhashz00(R6, R7, (BGL_RTL_CAST(obj_t , (V3))), (V4), (V5)));
L3:	R8 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((27143L))), BGL_RTL_CAST(obj_t , (BGl_string2135z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2123z00zz__weakhashz00)), (V3));
	BGL_RTL_FAIL(R8, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));
L4:	R9 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((27143L))), BGL_RTL_CAST(obj_t , (BGl_string2135z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R9, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

bool_t BGl_weakzd2keyszd2hashtablezd2removez12zc0zz__weakhashz00(obj_t V0, obj_t V1) {
 long V35;
 long R34;
 int32_t R33;
 int32_t R32;
 int32_t V31;
 long V30;
 char * R29;
 char * R28;
 obj_t R27;
 int R26;
 obj_t R25;
 obj_t R24;
 obj_t R23;
 int R22;
 long V21;
 char * R20;
 char * R19;
 obj_t V18;
 obj_t V17;
 bool_t R16;
 obj_t V15;
 obj_t V14;
 bool_t R13;
 obj_t V12;
 obj_t V11;
 obj_t V10;
 long V9;
 long R8;
 long V7;
 long V6;
 long R5;
 long R4;
 obj_t V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L40, PROCEDUREP((V3)));
L1:	BGL_RTL_IFNE(L42, BGL_RTL_EQ((V3), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R4 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R5 = (R4);
L4:	V6 = ((R5));
	V7 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L38, BGL_RTL_EQ((((((V6)) | ((V7))) & -2147483648)), (0L)));
L5:	R8 = BGL_RTL_REM((V6), (V7));
L6:	V9 = (R8);
	V10 = BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V2)), (V9));
	BGL_RTL_IFNE(L28, NULLP((V10)));
L7:	V11 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V10))))));
	V12 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L31, PROCEDUREP((V12)));
L8:	BGL_RTL_IFNE(L32, BGL_RTL_EQ((V11), (V1)));
L9:	BGL_RTL_IFNE(L36, STRINGP((V11)));
L10:	R13 = (((bool_t)0));
L11:	BGL_RTL_IFNE(L29, (R13));
L12:	V14 = ((V10));
	V15 = ((CDR(BGL_RTL_CAST(obj_t , (V10)))));
	BGL_RTL_GO(L26);
L13:	R16 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V17)))((V17), (V18), (V1), BEOA));
	BGL_RTL_GO(L24);
L14:	R16 = (((bool_t)1));
	BGL_RTL_GO(L24);
L15:	R19 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V18)));
	R20 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R16 = BGL_RTL_EQ((long)((memcmp(R19, R20, (V21)))), (0L));
	BGL_RTL_GO(L24);
L16:	V21 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V18)));
	BGL_RTL_IFNE(L15, BGL_RTL_EQ((V21), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L17:	R16 = (((bool_t)0));
	BGL_RTL_GO(L24);
L18:	BGL_RTL_IFNE(L16, STRINGP((V1)));
L19:	R16 = (((bool_t)0));
	BGL_RTL_GO(L24);
L20:	V18 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V15))))));
	V17 = STRUCT_REF((V0), (int)((3L)));
	BGL_RTL_IFNE(L13, PROCEDUREP((V17)));
L21:	BGL_RTL_IFNE(L14, BGL_RTL_EQ((V18), (V1)));
L22:	BGL_RTL_IFNE(L18, STRINGP((V18)));
L23:	R16 = (((bool_t)0));
L24:	BGL_RTL_IFNE(L30, (R16));
L25:	V14 = ((V15));
	V15 = ((CDR(BGL_RTL_CAST(obj_t , (V15)))));
L26:	BGL_RTL_IFNE(L20, PAIRP((V15)));
L27:	return(((((bool_t)0))));
L28:	return(((((bool_t)0))));
L29:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V2)), (V9), (CDR(BGL_RTL_CAST(obj_t , (V10)))));
	R22 = (int)((0L));
	R23 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R22, R23);
	return(((((bool_t)1))));
L30:	R24 = BGL_RTL_CAST(obj_t , (V14));
	R25 = (CDR(BGL_RTL_CAST(obj_t , (V15))));
	SET_CDR(R24, R25);
	R26 = (int)((0L));
	R27 = BGL_RTL_CAST(obj_t , BINT((BGL_RTL_SUB((long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((0L))))), (1L)))));
	STRUCT_SET((V0), R26, R27);
	return(((((bool_t)1))));
L31:	R13 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V12)))((V12), (V11), (V1), BEOA));
	BGL_RTL_GO(L11);
L32:	R13 = (((bool_t)1));
	BGL_RTL_GO(L11);
L33:	R28 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V11)));
	R29 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R13 = BGL_RTL_EQ((long)((memcmp(R28, R29, (V30)))), (0L));
	BGL_RTL_GO(L11);
L34:	V30 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V11)));
	BGL_RTL_IFNE(L33, BGL_RTL_EQ((V30), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L35:	R13 = (((bool_t)0));
	BGL_RTL_GO(L11);
L36:	BGL_RTL_IFNE(L34, STRINGP((V1)));
L37:	R13 = (((bool_t)0));
	BGL_RTL_GO(L11);
L38:	V31 = (int32_t)((V6));
	R32 = ((int32_t)((V7)));
	R33 = (BGL_RTL_REM((V31), R32));
	R34 = ((long)(R33));
	R8 = (long)(R34);
	BGL_RTL_GO(L6);
L39:	R5 = NEG((V35));
	BGL_RTL_GO(L4);
L40:	V35 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V1), BEOA))));
	BGL_RTL_IFNE(L39, BGL_RTL_LT((V35), (0L)));
L41:	R5 = (V35);
	BGL_RTL_GO(L4);
L42:	R4 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

bool_t BGl_weakzd2oldzd2hashtablezd2removez12zc0zz__weakhashz00(obj_t V0, obj_t V1) {
 long V17;
 long R16;
 int32_t R15;
 int32_t R14;
 int32_t V13;
 obj_t R12;
 obj_t V11;
 int R10;
 obj_t R9;
 long R8;
 long V7;
 long V6;
 long R5;
 long R4;
 obj_t V3;
 obj_t V2;
L0:	V2 = STRUCT_REF((V0), (int)((2L)));
	V3 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L11, PROCEDUREP((V3)));
L1:	BGL_RTL_IFNE(L13, BGL_RTL_EQ((V3), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L2:	R4 = BGl_getzd2hashnumberzd2zz__hashz00((V1));
L3:	R5 = (R4);
L4:	V6 = ((R5));
	V7 = BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V2)));
	BGL_RTL_IFNE(L9, BGL_RTL_EQ((((((V6)) | ((V7))) & -2147483648)), (0L)));
L5:	R8 = BGL_RTL_REM((V6), (V7));
L6:	BGL_RTL_CAST(obj_t , (V2));
	R9 = BGL_RTL_LOADFUN((obj_t) BGl_z62zc3z04anonymousza31623ze3ze5zz__weakhashz00);
	R10 = (int)((2L));
	V11 = MAKE_L_PROCEDURE((function_t) R9, R10);
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V11)), (int)((0L)), BGL_RTL_CAST(obj_t , (V0)));
	PROCEDURE_L_SET(BGL_RTL_CAST(obj_t , (V11)), (int)((1L)), (V1));
	R12 = (BGl_traversezd2bucketszd2zz__weakhashz00((V0), (V2), ((R8)), (V11)));
	BGL_RTL_IFNE(L8, BGL_RTL_EQ(R12, BGL_RTL_CAST(obj_t , (BGl_keepgoingz00zz__weakhashz00))));
L7:	return(((((bool_t)1))));
L8:	return(((((bool_t)0))));
L9:	V13 = (int32_t)((V6));
	R14 = ((int32_t)((V7)));
	R15 = (BGL_RTL_REM((V13), R14));
	R16 = ((long)(R15));
	R8 = (long)(R16);
	BGL_RTL_GO(L6);
L10:	R5 = NEG((V17));
	BGL_RTL_GO(L4);
L11:	V17 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V3)))((V3), (V1), BEOA))));
	BGL_RTL_IFNE(L10, BGL_RTL_LT((V17), (0L)));
L12:	R5 = (V17);
	BGL_RTL_GO(L4);
L13:	R4 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V1));
	BGL_RTL_GO(L3);

}

obj_t BGl_z62zc3z04anonymousza31623ze3ze5zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2, obj_t V3) {
 long V10;
 char * R9;
 char * R8;
 bool_t R7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
L0:	V4 = BGL_RTL_CAST(obj_t , PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((0L))));
	V5 = PROCEDURE_L_REF(BGL_RTL_CAST(obj_t , (V0)), (int)((1L)));
	V6 = STRUCT_REF((V4), (int)((3L)));
	BGL_RTL_IFNE(L7, PROCEDUREP((V6)));
L1:	BGL_RTL_IFNE(L8, BGL_RTL_EQ((V5), (V1)));
L2:	BGL_RTL_IFNE(L12, STRINGP((V5)));
L3:	R7 = (((bool_t)0));
L4:	BGL_RTL_IFNE(L6, (R7));
L5:	return(((BGl_keepgoingz00zz__weakhashz00)));
L6:	return(((BGl_removestopz00zz__weakhashz00)));
L7:	R7 = CBOOL(((obj_t (*)())PROCEDURE_ENTRY((V6)))((V6), (V5), (V1), BEOA));
	BGL_RTL_GO(L4);
L8:	R7 = (((bool_t)1));
	BGL_RTL_GO(L4);
L9:	R8 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V5)));
	R9 = BSTRING_TO_STRING(BGL_RTL_CAST(obj_t , (V1)));
	R7 = BGL_RTL_EQ((long)((memcmp(R8, R9, (V10)))), (0L));
	BGL_RTL_GO(L4);
L10:	V10 = STRING_LENGTH(BGL_RTL_CAST(obj_t , (V5)));
	BGL_RTL_IFNE(L9, BGL_RTL_EQ((V10), STRING_LENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L11:	R7 = (((bool_t)0));
	BGL_RTL_GO(L4);
L12:	BGL_RTL_IFNE(L10, STRINGP((V1)));
L13:	R7 = (((bool_t)0));
	BGL_RTL_GO(L4);

}

obj_t BGl_weakzd2hashtablezd2removez12z12zz__weakhashz00(obj_t V0, obj_t V1) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_weakzd2oldzd2hashtablezd2removez12zc0zz__weakhashz00((V0), (V1))))));
L2:	return((BGL_RTL_CAST(obj_t , BBOOL(BGl_weakzd2keyszd2hashtablezd2removez12zc0zz__weakhashz00((V0), (V1))))));

}

obj_t BGl_z62weakzd2hashtablezd2removez12z70zz__weakhashz00(obj_t V0, obj_t V1, obj_t V2) {
 obj_t R3;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2removez12z12zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1))), (V2)));
L2:	R3 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((29319L))), BGL_RTL_CAST(obj_t , (BGl_string2136z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R3, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00(obj_t V0) {
 obj_t V47;
 obj_t V46;
 int R45;
 obj_t V44;
 obj_t V43;
 obj_t V42;
 obj_t R41;
 obj_t V40;
 obj_t R39;
 obj_t R38;
 int R37;
 long V36;
 obj_t V35;
 long R34;
 obj_t V33;
 obj_t V32;
 long V31;
 long R30;
 long R29;
 long R28;
 int32_t R27;
 long V26;
 int32_t R25;
 long V24;
 int32_t V23;
 obj_t V22;
 long V21;
 obj_t R20;
 int R19;
 long V18;
 obj_t V17;
 long V16;
 long V15;
 obj_t V14;
 bool_t R13;
 obj_t R12;
 obj_t V11;
 obj_t V10;
 obj_t V9;
 obj_t V8;
 long V7;
 bool_t R6;
 long V5;
 long V4;
 bool_t V3;
 obj_t V2;
 obj_t V1;
L0:	BGl_gcz00zz__biglooz00(BGL_RTL_CAST(obj_t , (BTRUE)));
	V1 = STRUCT_REF((V0), (int)((2L)));
	V2 = STRUCT_REF((V0), (int)((1L)));
	V3 = ((((bool_t)0)));
	V4 = ((0L));
	V5 = ((0L));
	BGL_RTL_GO(L13);
L1:	R6 = (V3);
	BGL_RTL_GO(L12);
L2:	V7 = (0L);
	V8 = MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BNIL)));
	V9 = ((V8));
	V10 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V5))));
	BGL_RTL_GO(L9);
L3:	V11 = MAKE_YOUNG_PAIR((CAR(BGL_RTL_CAST(obj_t , (V10)))), BGL_RTL_CAST(obj_t , (BNIL)));
	R12 = BGL_RTL_CAST(obj_t , (V11));
	SET_CDR((V9), R12);
	V9 = ((V11));
	V10 = ((CDR(BGL_RTL_CAST(obj_t , (V10)))));
	BGL_RTL_GO(L9);
L4:	R13 = (((bool_t)0));
	BGL_RTL_GO(L7);
L5:	V14 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V10))))));
	BGL_RTL_IFNE(L4, BGL_RTL_EQ((V14), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L6:	V7 = BGL_RTL_ADD((V7), (1L));
	R13 = CBOOL((V14));
L7:	BGL_RTL_IFNE(L3, (R13));
L8:	V10 = ((CDR(BGL_RTL_CAST(obj_t , (V10)))));
L9:	BGL_RTL_IFEQ(L5, NULLP((V10)));
L10:	BGL_RTL_VSET(BGL_RTL_CAST(obj_t , (V1)), (V5), ((CDR((V8)))));
	V15 = BGL_RTL_ADD((V5), (1L));
	V16 = BGL_RTL_ADD((V4), (V7));
	BGL_RTL_IFNE(L1, (V3));
L11:	R6 = BGL_RTL_GT(((V7)), (long)CINT(BGL_RTL_CAST(obj_t , (V2))));
L12:	V3 = (((R6)));
	V4 = ((V16));
	V5 = ((V15));
L13:	BGL_RTL_IFNE(L2, BGL_RTL_LT((V5), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L14:	BGL_RTL_IFNE(L36, (V3));
L15:	return((BGL_RTL_CAST(obj_t , (BFALSE))));
L16:	V17 = make_vector((V18), BGL_RTL_CAST(obj_t , (BNIL)));
	R19 = (int)((2L));
	R20 = BGL_RTL_CAST(obj_t , (V17));
	STRUCT_SET((V0), R19, R20);
	V21 = ((0L));
	BGL_RTL_GO(L32);
L17:	V22 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V21))));
	BGL_RTL_GO(L30);
L18:	V23 = (int32_t)((V24));
	R25 = ((int32_t)((V26)));
	R27 = (BGL_RTL_REM((V23), R25));
	R28 = ((long)(R27));
	R29 = (long)(R28);
	BGL_RTL_GO(L29);
L19:	R30 = NEG((V31));
	BGL_RTL_GO(L27);
L20:	V31 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V32)))((V32), (V33), BEOA))));
	BGL_RTL_IFNE(L19, BGL_RTL_LT((V31), (0L)));
L21:	R30 = (V31);
	BGL_RTL_GO(L27);
L22:	R34 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V33));
	BGL_RTL_GO(L26);
L23:	V35 = CAR(BGL_RTL_CAST(obj_t , (V22)));
	V33 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (V35)));
	V32 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L20, PROCEDUREP((V32)));
L24:	BGL_RTL_IFNE(L22, BGL_RTL_EQ((V32), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L25:	R34 = BGl_getzd2hashnumberzd2zz__hashz00((V33));
L26:	R30 = (R34);
L27:	V24 = ((R30));
	V26 = (V18);
	BGL_RTL_IFNE(L18, BGL_RTL_EQ((((((V24)) | ((V26))) & -2147483648)), (0L)));
L28:	R29 = BGL_RTL_REM((V24), (V26));
L29:	V36 = (R29);
	BGL_RTL_VSET((V17), (V36), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V35), BGL_RTL_VREF((V17), (V36))))));
	V22 = ((CDR(BGL_RTL_CAST(obj_t , (V22)))));
L30:	BGL_RTL_IFNE(L23, PAIRP((V22)));
L31:	V21 = (BGL_RTL_ADD((V21), (1L)));
L32:	BGL_RTL_IFNE(L17, BGL_RTL_LT((V21), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L33:	R37 = (int)((0L));
	R38 = BGL_RTL_CAST(obj_t , BINT((V4)));
	STRUCT_SET((V0), R37, R38);
	return((BGL_RTL_CAST(obj_t , ((BUNSPEC)))));
L34:	R39 = BGL_RTL_CAST(obj_t , BINT((long)(REAL_TO_DOUBLE(BGL_RTL_CAST(obj_t , (V40))))));
	BGL_RTL_GO(L41);
L35:	R41 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_MUL((long)CINT(BGL_RTL_CAST(obj_t , (V42))), (long)CINT(BGL_RTL_CAST(obj_t , (V43))))));
	BGL_RTL_GO(L39);
L36:	V18 = BGL_RTL_MUL((2L), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V1))));
	V44 = STRUCT_REF((V0), (int)((6L)));
	V42 = STRUCT_REF((V0), (int)((1L)));
	V43 = STRUCT_REF((V0), (int)((7L)));
	BGL_RTL_IFEQ(L38, INTEGERP((V42)));
L37:	BGL_RTL_IFNE(L35, INTEGERP((V43)));
L38:	R41 = BGl_2za2za2zz__r4_numbers_6_5z00((V42), (V43));
L39:	V40 = (R41);
	BGL_RTL_IFNE(L34, REALP((V40)));
L40:	R39 = (V40);
L41:	R45 = (int)((1L));
	STRUCT_SET((V0), R45, ((R39)));
	BGL_RTL_IFNE(L16, BGL_RTL_LT((long)CINT(BGL_RTL_CAST(obj_t , (V44))), (0L)));
L42:	BGL_RTL_IFNE(L16, BGL_RTL_LE((V18), (long)CINT(BGL_RTL_CAST(obj_t , (V44)))));
L43:	V46 = MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , BINT((V18))), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V44), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR(BGL_RTL_CAST(obj_t , BINT((BGl_hashtablezd2siza7ez75zz__hashz00((V0))))), BGL_RTL_CAST(obj_t , (BNIL)))))))));
	V47 = BGl_formatz00zz__r4_output_6_10_3z00((BGl_string2137z00zz__weakhashz00), BGL_RTL_CAST(obj_t , (V46)));
	return((BGl_errorz00zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2138z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (V47)), BGL_RTL_CAST(obj_t , (V0)))));

}

obj_t BGl_weakzd2oldzd2hashtablezd2expandz12zc0zz__weakhashz00(obj_t V0) {
 int R117;
 long V116;
 obj_t V115;
 obj_t V114;
 long R113;
 obj_t V112;
 obj_t V111;
 long V110;
 long R109;
 long R108;
 long R107;
 int32_t R106;
 long V105;
 int32_t R104;
 long V103;
 int32_t V102;
 obj_t V101;
 long V100;
 obj_t R99;
 int R98;
 int R97;
 obj_t V96;
 obj_t V95;
 long V94;
 obj_t V93;
 obj_t V92;
 obj_t V91;
 obj_t R90;
 obj_t V89;
 obj_t R88;
 int R87;
 long V86;
 obj_t V85;
 long R84;
 obj_t V83;
 obj_t V82;
 long V81;
 long R80;
 long R79;
 long R78;
 int32_t R77;
 long V76;
 int32_t R75;
 long V74;
 int32_t V73;
 obj_t V72;
 long V71;
 obj_t R70;
 int R69;
 int R68;
 obj_t V67;
 obj_t V66;
 long V65;
 obj_t V64;
 obj_t V63;
 obj_t V62;
 obj_t R61;
 obj_t V60;
 obj_t R59;
 int R58;
 long V57;
 obj_t V56;
 long R55;
 obj_t V54;
 obj_t V53;
 long V52;
 long R51;
 long R50;
 long R49;
 int32_t R48;
 long V47;
 int32_t R46;
 long V45;
 int32_t V44;
 obj_t V43;
 long V42;
 obj_t R41;
 int R40;
 int R39;
 obj_t V38;
 obj_t V37;
 long V36;
 obj_t V35;
 obj_t V34;
 obj_t V33;
 obj_t R32;
 obj_t V31;
 obj_t R30;
 int R29;
 long V28;
 obj_t V27;
 long R26;
 obj_t V25;
 obj_t V24;
 long V23;
 long R22;
 long R21;
 long R20;
 int32_t R19;
 long V18;
 int32_t R17;
 long V16;
 int32_t V15;
 obj_t V14;
 long V13;
 obj_t R12;
 int R11;
 int R10;
 obj_t R9;
 obj_t V8;
 obj_t R7;
 obj_t V6;
 obj_t V5;
 obj_t V4;
 obj_t V3;
 long V2;
 obj_t V1;
L0:	BGL_RTL_IFNE(L28, BGL_RTL_EQ((1L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L1:	BGL_RTL_IFNE(L56, BGL_RTL_EQ((2L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L2:	BGL_RTL_IFNE(L84, BGL_RTL_EQ((3L), (long)CINT(BGL_RTL_CAST(obj_t , STRUCT_REF((V0), (int)((5L)))))));
L3:	V1 = STRUCT_REF((V0), (int)((2L)));
	V2 = BGL_RTL_MUL((2L), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V1))));
	V3 = make_vector((V2), BGL_RTL_CAST(obj_t , (BNIL)));
	V4 = STRUCT_REF((V0), (int)((0L)));
	V5 = STRUCT_REF((V0), (int)((1L)));
	V6 = STRUCT_REF((V0), (int)((7L)));
	BGL_RTL_IFEQ(L5, INTEGERP((V5)));
L4:	BGL_RTL_IFNE(L112, INTEGERP((V6)));
L5:	R7 = BGl_2za2za2zz__r4_numbers_6_5z00((V5), (V6));
L6:	V8 = (R7);
	BGL_RTL_IFNE(L111, REALP((V8)));
L7:	R9 = (V8);
L8:	R10 = (int)((1L));
	STRUCT_SET((V0), R10, ((R9)));
	R11 = (int)((2L));
	R12 = BGL_RTL_CAST(obj_t , (V3));
	STRUCT_SET((V0), R11, R12);
	V13 = ((0L));
	BGL_RTL_GO(L24);
L9:	V14 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V1)), (V13))));
	BGL_RTL_GO(L22);
L10:	V15 = (int32_t)((V16));
	R17 = ((int32_t)((V18)));
	R19 = (BGL_RTL_REM((V15), R17));
	R20 = ((long)(R19));
	R21 = (long)(R20);
	BGL_RTL_GO(L21);
L11:	R22 = NEG((V23));
	BGL_RTL_GO(L19);
L12:	V23 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V24)))((V24), (V25), BEOA))));
	BGL_RTL_IFNE(L11, BGL_RTL_LT((V23), (0L)));
L13:	R22 = (V23);
	BGL_RTL_GO(L19);
L14:	R26 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V25));
	BGL_RTL_GO(L18);
L15:	V27 = CAR(BGL_RTL_CAST(obj_t , (V14)));
	V25 = CAR(BGL_RTL_CAST(obj_t , (V27)));
	V24 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L12, PROCEDUREP((V24)));
L16:	BGL_RTL_IFNE(L14, BGL_RTL_EQ((V24), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L17:	R26 = BGl_getzd2hashnumberzd2zz__hashz00((V25));
L18:	R22 = (R26);
L19:	V16 = ((R22));
	V18 = (V2);
	BGL_RTL_IFNE(L10, BGL_RTL_EQ((((((V16)) | ((V18))) & -2147483648)), (0L)));
L20:	R21 = BGL_RTL_REM((V16), (V18));
L21:	V28 = (R21);
	BGL_RTL_VSET((V3), (V28), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V27), BGL_RTL_VREF((V3), (V28))))));
	V14 = ((CDR(BGL_RTL_CAST(obj_t , (V14)))));
L22:	BGL_RTL_IFNE(L15, PAIRP((V14)));
L23:	V13 = (BGL_RTL_ADD((V13), (1L)));
L24:	BGL_RTL_IFNE(L9, BGL_RTL_LT((V13), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V1)))));
L25:	R29 = (int)((0L));
	STRUCT_SET((V0), R29, (V4));
	return(((BUNSPEC)));
L26:	R30 = BGL_RTL_CAST(obj_t , BINT((long)(REAL_TO_DOUBLE(BGL_RTL_CAST(obj_t , (V31))))));
	BGL_RTL_GO(L33);
L27:	R32 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_MUL((long)CINT(BGL_RTL_CAST(obj_t , (V33))), (long)CINT(BGL_RTL_CAST(obj_t , (V34))))));
	BGL_RTL_GO(L31);
L28:	V35 = STRUCT_REF((V0), (int)((2L)));
	V36 = BGL_RTL_MUL((2L), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V35))));
	V37 = make_vector((V36), BGL_RTL_CAST(obj_t , (BNIL)));
	V38 = STRUCT_REF((V0), (int)((0L)));
	V33 = STRUCT_REF((V0), (int)((1L)));
	V34 = STRUCT_REF((V0), (int)((7L)));
	BGL_RTL_IFEQ(L30, INTEGERP((V33)));
L29:	BGL_RTL_IFNE(L27, INTEGERP((V34)));
L30:	R32 = BGl_2za2za2zz__r4_numbers_6_5z00((V33), (V34));
L31:	V31 = (R32);
	BGL_RTL_IFNE(L26, REALP((V31)));
L32:	R30 = (V31);
L33:	R39 = (int)((1L));
	STRUCT_SET((V0), R39, ((R30)));
	R40 = (int)((2L));
	R41 = BGL_RTL_CAST(obj_t , (V37));
	STRUCT_SET((V0), R40, R41);
	V42 = ((0L));
	BGL_RTL_GO(L52);
L34:	V43 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V35)), (V42))));
	BGL_RTL_GO(L50);
L35:	V38 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_SUB(((long)CINT(BGL_RTL_CAST(obj_t , (V38)))), (1L))));
	BGL_RTL_GO(L49);
L36:	V44 = (int32_t)((V45));
	R46 = ((int32_t)((V47)));
	R48 = (BGL_RTL_REM((V44), R46));
	R49 = ((long)(R48));
	R50 = (long)(R49);
	BGL_RTL_GO(L48);
L37:	R51 = NEG((V52));
	BGL_RTL_GO(L46);
L38:	V52 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V53)))((V53), (V54), BEOA))));
	BGL_RTL_IFNE(L37, BGL_RTL_LT((V52), (0L)));
L39:	R51 = (V52);
	BGL_RTL_GO(L46);
L40:	R55 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V54));
	BGL_RTL_GO(L45);
L41:	V56 = CAR(BGL_RTL_CAST(obj_t , (V43)));
	V54 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V56))))));
	BGL_RTL_IFNE(L35, BGL_RTL_EQ((V54), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L42:	V53 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L38, PROCEDUREP((V53)));
L43:	BGL_RTL_IFNE(L40, BGL_RTL_EQ((V53), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L44:	R55 = BGl_getzd2hashnumberzd2zz__hashz00((V54));
L45:	R51 = (R55);
L46:	V45 = ((R51));
	V47 = (V36);
	BGL_RTL_IFNE(L36, BGL_RTL_EQ((((((V45)) | ((V47))) & -2147483648)), (0L)));
L47:	R50 = BGL_RTL_REM((V45), (V47));
L48:	V57 = (R50);
	BGL_RTL_VSET((V37), (V57), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V56), BGL_RTL_VREF((V37), (V57))))));
L49:	V43 = ((CDR(BGL_RTL_CAST(obj_t , (V43)))));
L50:	BGL_RTL_IFNE(L41, PAIRP((V43)));
L51:	V42 = (BGL_RTL_ADD((V42), (1L)));
L52:	BGL_RTL_IFNE(L34, BGL_RTL_LT((V42), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V35)))));
L53:	R58 = (int)((0L));
	STRUCT_SET((V0), R58, ((V38)));
	return(((BUNSPEC)));
L54:	R59 = BGL_RTL_CAST(obj_t , BINT((long)(REAL_TO_DOUBLE(BGL_RTL_CAST(obj_t , (V60))))));
	BGL_RTL_GO(L61);
L55:	R61 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_MUL((long)CINT(BGL_RTL_CAST(obj_t , (V62))), (long)CINT(BGL_RTL_CAST(obj_t , (V63))))));
	BGL_RTL_GO(L59);
L56:	V64 = STRUCT_REF((V0), (int)((2L)));
	V65 = BGL_RTL_MUL((2L), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V64))));
	V66 = make_vector((V65), BGL_RTL_CAST(obj_t , (BNIL)));
	V67 = STRUCT_REF((V0), (int)((0L)));
	V62 = STRUCT_REF((V0), (int)((1L)));
	V63 = STRUCT_REF((V0), (int)((7L)));
	BGL_RTL_IFEQ(L58, INTEGERP((V62)));
L57:	BGL_RTL_IFNE(L55, INTEGERP((V63)));
L58:	R61 = BGl_2za2za2zz__r4_numbers_6_5z00((V62), (V63));
L59:	V60 = (R61);
	BGL_RTL_IFNE(L54, REALP((V60)));
L60:	R59 = (V60);
L61:	R68 = (int)((1L));
	STRUCT_SET((V0), R68, ((R59)));
	R69 = (int)((2L));
	R70 = BGL_RTL_CAST(obj_t , (V66));
	STRUCT_SET((V0), R69, R70);
	V71 = ((0L));
	BGL_RTL_GO(L80);
L62:	V72 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V64)), (V71))));
	BGL_RTL_GO(L78);
L63:	V67 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_SUB(((long)CINT(BGL_RTL_CAST(obj_t , (V67)))), (1L))));
	BGL_RTL_GO(L77);
L64:	V73 = (int32_t)((V74));
	R75 = ((int32_t)((V76)));
	R77 = (BGL_RTL_REM((V73), R75));
	R78 = ((long)(R77));
	R79 = (long)(R78);
	BGL_RTL_GO(L76);
L65:	R80 = NEG((V81));
	BGL_RTL_GO(L74);
L66:	V81 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V82)))((V82), (V83), BEOA))));
	BGL_RTL_IFNE(L65, BGL_RTL_LT((V81), (0L)));
L67:	R80 = (V81);
	BGL_RTL_GO(L74);
L68:	R84 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V83));
	BGL_RTL_GO(L73);
L69:	V85 = CAR(BGL_RTL_CAST(obj_t , (V72)));
	BGL_RTL_IFNE(L63, BGL_RTL_EQ((bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CDR(BGL_RTL_CAST(obj_t , (V85))))))), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L70:	V83 = CAR(BGL_RTL_CAST(obj_t , (V85)));
	V82 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L66, PROCEDUREP((V82)));
L71:	BGL_RTL_IFNE(L68, BGL_RTL_EQ((V82), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L72:	R84 = BGl_getzd2hashnumberzd2zz__hashz00((V83));
L73:	R80 = (R84);
L74:	V74 = ((R80));
	V76 = (V65);
	BGL_RTL_IFNE(L64, BGL_RTL_EQ((((((V74)) | ((V76))) & -2147483648)), (0L)));
L75:	R79 = BGL_RTL_REM((V74), (V76));
L76:	V86 = (R79);
	BGL_RTL_VSET((V66), (V86), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V85), BGL_RTL_VREF((V66), (V86))))));
L77:	V72 = ((CDR(BGL_RTL_CAST(obj_t , (V72)))));
L78:	BGL_RTL_IFNE(L69, PAIRP((V72)));
L79:	V71 = (BGL_RTL_ADD((V71), (1L)));
L80:	BGL_RTL_IFNE(L62, BGL_RTL_LT((V71), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V64)))));
L81:	R87 = (int)((0L));
	STRUCT_SET((V0), R87, ((V67)));
	return(((BUNSPEC)));
L82:	R88 = BGL_RTL_CAST(obj_t , BINT((long)(REAL_TO_DOUBLE(BGL_RTL_CAST(obj_t , (V89))))));
	BGL_RTL_GO(L89);
L83:	R90 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_MUL((long)CINT(BGL_RTL_CAST(obj_t , (V91))), (long)CINT(BGL_RTL_CAST(obj_t , (V92))))));
	BGL_RTL_GO(L87);
L84:	V93 = STRUCT_REF((V0), (int)((2L)));
	V94 = BGL_RTL_MUL((2L), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V93))));
	V95 = make_vector((V94), BGL_RTL_CAST(obj_t , (BNIL)));
	V96 = STRUCT_REF((V0), (int)((0L)));
	V91 = STRUCT_REF((V0), (int)((1L)));
	V92 = STRUCT_REF((V0), (int)((7L)));
	BGL_RTL_IFEQ(L86, INTEGERP((V91)));
L85:	BGL_RTL_IFNE(L83, INTEGERP((V92)));
L86:	R90 = BGl_2za2za2zz__r4_numbers_6_5z00((V91), (V92));
L87:	V89 = (R90);
	BGL_RTL_IFNE(L82, REALP((V89)));
L88:	R88 = (V89);
L89:	R97 = (int)((1L));
	STRUCT_SET((V0), R97, ((R88)));
	R98 = (int)((2L));
	R99 = BGL_RTL_CAST(obj_t , (V95));
	STRUCT_SET((V0), R98, R99);
	V100 = ((0L));
	BGL_RTL_GO(L109);
L90:	V101 = ((BGL_RTL_VREF(BGL_RTL_CAST(obj_t , (V93)), (V100))));
	BGL_RTL_GO(L107);
L91:	V96 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_SUB(((long)CINT(BGL_RTL_CAST(obj_t , (V96)))), (1L))));
	BGL_RTL_GO(L106);
L92:	V102 = (int32_t)((V103));
	R104 = ((int32_t)((V105)));
	R106 = (BGL_RTL_REM((V102), R104));
	R107 = ((long)(R106));
	R108 = (long)(R107);
	BGL_RTL_GO(L105);
L93:	R109 = NEG((V110));
	BGL_RTL_GO(L103);
L94:	V110 = (long)CINT(BGL_RTL_CAST(obj_t , (((obj_t (*)())PROCEDURE_ENTRY((V111)))((V111), (V112), BEOA))));
	BGL_RTL_IFNE(L93, BGL_RTL_LT((V110), (0L)));
L95:	R109 = (V110);
	BGL_RTL_GO(L103);
L96:	R113 = BGl_getzd2hashnumberzd2persistentz00zz__hashz00((V112));
	BGL_RTL_GO(L102);
L97:	V114 = CAR(BGL_RTL_CAST(obj_t , (V101)));
	V112 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CAR(BGL_RTL_CAST(obj_t , (V114))))));
	V115 = bgl_weakptr_data(BGL_RTL_CAST(obj_t , (CDR(BGL_RTL_CAST(obj_t , (V114))))));
	BGL_RTL_IFNE(L91, BGL_RTL_EQ((V112), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L98:	BGL_RTL_IFNE(L91, BGL_RTL_EQ((V115), BGL_RTL_CAST(obj_t , (BUNSPEC))));
L99:	V111 = STRUCT_REF((V0), (int)((4L)));
	BGL_RTL_IFNE(L94, PROCEDUREP((V111)));
L100:	BGL_RTL_IFNE(L96, BGL_RTL_EQ((V111), BGL_RTL_CAST(obj_t , (BGl_symbol2129z00zz__weakhashz00))));
L101:	R113 = BGl_getzd2hashnumberzd2zz__hashz00((V112));
L102:	R109 = (R113);
L103:	V103 = ((R109));
	V105 = (V94);
	BGL_RTL_IFNE(L92, BGL_RTL_EQ((((((V103)) | ((V105))) & -2147483648)), (0L)));
L104:	R108 = BGL_RTL_REM((V103), (V105));
L105:	V116 = (R108);
	BGL_RTL_VSET((V95), (V116), BGL_RTL_CAST(obj_t , (MAKE_YOUNG_PAIR((V114), BGL_RTL_VREF((V95), (V116))))));
L106:	V101 = ((CDR(BGL_RTL_CAST(obj_t , (V101)))));
L107:	BGL_RTL_IFNE(L97, PAIRP((V101)));
L108:	V100 = (BGL_RTL_ADD((V100), (1L)));
L109:	BGL_RTL_IFNE(L90, BGL_RTL_LT((V100), BGL_RTL_VLENGTH(BGL_RTL_CAST(obj_t , (V93)))));
L110:	R117 = (int)((0L));
	STRUCT_SET((V0), R117, ((V96)));
	return(((BUNSPEC)));
L111:	R9 = BGL_RTL_CAST(obj_t , BINT((long)(REAL_TO_DOUBLE(BGL_RTL_CAST(obj_t , (V8))))));
	BGL_RTL_GO(L8);
L112:	R7 = BGL_RTL_CAST(obj_t , BINT(BGL_RTL_MUL((long)CINT(BGL_RTL_CAST(obj_t , (V5))), (long)CINT(BGL_RTL_CAST(obj_t , (V6))))));
	BGL_RTL_GO(L6);

}

obj_t BGl_weakzd2hashtablezd2expandz12z12zz__weakhashz00(obj_t V0) {
L0:	BGL_RTL_IFNE(L2, BGl_hashtablezd2weakzd2keyszf3zf3zz__hashz00((V0)));
L1:	return((BGL_RTL_CAST(obj_t , BGl_weakzd2oldzd2hashtablezd2expandz12zc0zz__weakhashz00((V0)))));
L2:	return((BGl_weakzd2keyszd2hashtablezd2expandz12zc0zz__weakhashz00((V0))));

}

obj_t BGl_z62weakzd2hashtablezd2expandz12z70zz__weakhashz00(obj_t V0, obj_t V1) {
 obj_t R2;
L0:	BGL_RTL_IFEQ(L2, STRUCTP((V1)));
L1:	return(BGl_weakzd2hashtablezd2expandz12z12zz__weakhashz00((BGL_RTL_CAST(obj_t , (V1)))));
L2:	R2 = BGl_typezd2errorzd2zz__errorz00(BGL_RTL_CAST(obj_t , (BGl_string2117z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , BINT((34257L))), BGL_RTL_CAST(obj_t , (BGl_string2139z00zz__weakhashz00)), BGL_RTL_CAST(obj_t , (BGl_string2119z00zz__weakhashz00)), (V1));
	BGL_RTL_FAIL(R2, BGL_RTL_CAST(obj_t , (BFALSE)), BGL_RTL_CAST(obj_t , (BFALSE)));

}

obj_t BGl_objectzd2initzd2zz__weakhashz00() {
L0:	return(BGL_RTL_CAST(obj_t , (BUNSPEC)));

}

obj_t BGl_genericzd2initzd2zz__weakhashz00() {
L0:	return(BGL_RTL_CAST(obj_t , (BUNSPEC)));

}

obj_t BGl_methodzd2initzd2zz__weakhashz00() {
L0:	return(BGL_RTL_CAST(obj_t , (BUNSPEC)));

}

obj_t BGl_importedzd2moduleszd2initz00zz__weakhashz00() {
L0:	BGl_modulezd2initializa7ationz75zz__errorz00((88804785L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00)));
	BGl_modulezd2initializa7ationz75zz__r4_symbols_6_4z00((438371116L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00)));
	BGl_modulezd2initializa7ationz75zz__paramz00((453939141L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00)));
	BGl_modulezd2initializa7ationz75zz__hashz00((482391669L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00)));
	BGl_modulezd2initializa7ationz75zz__bexitz00((443005284L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00)));
	BGl_modulezd2initializa7ationz75zz__objectz00((475449627L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00)));
	return(BGl_modulezd2initializa7ationz75zz__threadz00((149516032L), BSTRING_TO_STRING((BGl_string2140z00zz__weakhashz00))));

}


#ifdef __cplusplus
}
#endif
