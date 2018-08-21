/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/patch/src/Clib/bglpatch.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jun  2 08:27:57 2017                          */
/*    Last change :  Tue Aug 21 11:23:12 2018 (serrano)                */
/*    Copyright   :  2017-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo wrapper to SELF-MOD                                       */
/*=====================================================================*/
#ifndef _BGL_PATCH_H
#define _BGL_PATCH_H

/*---------------------------------------------------------------------*/
/*    Patch initialization (mprotect setting) ...                      */
/*---------------------------------------------------------------------*/
#ifdef linux
extern uint8_t __executable_start;
extern uint8_t __etext;
extern void bgl_patch_init( void *, void * );

#  define BGL_INIT_PATCH() bgl_patch_init( &__executable_start, &__etext )
#else
#  define BGL_INIT_PATCH() 
#endif

/*---------------------------------------------------------------------*/
/*    Patch types                                                      */
/*---------------------------------------------------------------------*/
typedef struct patch_descr bgl_patch_descr_t;
typedef int32_t patch_32_type;

/*---------------------------------------------------------------------*/
/*    Patch imports & macros                                           */
/*---------------------------------------------------------------------*/
extern void patch_32( bgl_patch_descr_t *patch, patch_32_type val_32 );

#define BGL_PATCH_DESCR32_OBJ( d, v ) patch_32( d, (patch_32_type)((long)((void *)v)) )
#define BGL_PATCH_DESCR32_PTR( d, v ) patch_32( d, (patch_32_type)((long)((void *)v)) )
#define BGL_PATCH_DESCR32_LONG( d, v ) patch_32( d, (patch_32_type)((long)v) )


/* extern long GLOB;                                                   */
/* extern void * GLOB2;                                                */
/*                                                                     */
/* #if( BGL_SELF_MODIFYING_CODE )                                      */
/* #  include <self-mod.h>                                             */
/*                                                                     */
/* #  ifdef linux                                                      */
/* extern uint8_t __executable_start;                                  */
/* extern uint8_t __etext;                                             */
/*                                                                     */
/* #    define bgl_init_self_mod() init_self_mod( &__executable_start, &__etext ) */
/* #  else                                                             */
/*   extern uint8_t _mh_execute_header;                                */
/* #    define bgl_init_self_mod() init_self_mod( &_mh_execute_header, 0 ) */
/* #  endif                                                            */
/*                                                                     */
/* typedef patch_descr __bgl_patch_descr;                              */
/*                                                                     */
/* extern void bgl_init_patch_32( void *, size_t, patch_descr * );     */
/* extern void bgl_init_patch_64( void *, size_t, patch_descr * );     */
/*                                                                     */
/* #  define _BGL_PATCHABLE_CONSTANT_32( _idx, _gidx, _var ) \         */
/*    asm( "mov %1, %0" : "=g"( _var ) : "i"( PATCHABLE_CONSTANT_32( _idx ) ) ) */
/* #  define BGL_PATCHABLE_CONSTANT_32( _idx, _gidx, _var ) _var = (long)GLOB */
/* #  define BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) _var = GLOB */
/* #  define _____BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) \     */
/*    asm( "movabsq %1, %0" : "=g"( _var ) : "i"( PATCHABLE_CONSTANT_64( _idx ) ) ) */
/* #  define ____BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) \      */
/*    asm( "movabsq %1, %0; mov %0, GLOB(%%rip)" : "=g"( _var ) : "i"( PATCHABLE_CONSTANT_64( _idx ) ) ) */
/* #  define ___BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) \       */
/*    asm( "lea 0(%%rip),%0;mov %0,GLOB2(%%rip);movabsq %1, %0; mov %0, GLOB(%%rip)" : "=g"( _var ) : "i"( PATCHABLE_CONSTANT_64( _idx ) ) ) */
/* #  define _BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) \         */
/*    asm( "lea 0(%%rip),%0;movabsq %1, %0" : "=g"( _var ) : "i"( PATCHABLE_CONSTANT_64( _idx ) ) ) */
/* //#  define BGL_PATCHABLE_CONSTANT_32( _idx, _gidx, _var ) (obj_t)PATCHABLE_CONSTANT_32( idx ) */
/* //#  define BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) (obj_t)PATCHABLE_CONSTANT_64( idx ) */
/*                                                                     */
/* #  define BGL_PATCH_INT32_SET( _tbl, _idx, val ) patch_32( &(_tbl[ _idx ]), val) */
/* #  define BGL_PATCH_INT64_SET( _tbl, _idx, val ) patch_64( &(_tbl[ _idx ]), (long)val) */
/* #else                                                               */
/* typedef long __bgl_patch_descr;                                     */
/* #  define bgl_init_self_mod()                                       */
/*                                                                     */
/* #  define fill_patch_32( f, len, tbl ) memset( tbl, (int)(long)BFALSE, sizeof( int32_t ) * len ) */
/* #  define fill_patch_64( f, len, tbl ) memset( tbl, (int)(long)BFALSE, sizeof( int64_t ) * len ) */
/*                                                                     */
/* #  define bgl_init_patch_32( f, len, tbl ) fill_patch_32( f, len, tbl ) */
/* #  define bgl_init_patch_64( f, len, tbl ) fill_patch_64( f, len, tbl ) */
/*                                                                     */
/* #  define BGL_PATCHABLE_CONSTANT_32( _idx, _gidx, _var ) (_var = __bgl_patches[ _gidx ]) */
/* #  define BGL_PATCHABLE_CONSTANT_64( _idx, _gidx, _var ) (_var = (obj_t)__bgl_patches[ _gidx ]) */
/*                                                                     */
/* #  define BGL_PATCH_INT32_SET( _tbl, _idx, val ) (_tbl[ _idx ] = (val)) */
/* #  define BGL_PATCH_INT64_SET( _tbl, _idx, val ) (_tbl[ _idx ] = ((long)val)) */
/* #endif                                                              */
/*                                                                     */
/* #if( PTR_ALIGNMENT == 3 )                                           */
/* #  define BGL_PATCH_OBJ_SET( _tbl, _idx, val ) \                    */
/*      BGL_PATCH_INT64_SET( _tbl, _idx, (long)val )                   */
/* #else                                                               */
/* #  define BGL_PATCH_OBJ_SET( _tbl, _idx, val ) \                    */
/*      BGL_PATCH_INT32_SET( _tbl, _idx, (long)val )                   */
/* #endif                                                              */

#endif
