/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/patch/src/Clib/bglpatch.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  7 08:29:00 2017                          */
/*    Last change :  Tue Jun 20 18:29:41 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    BGL patch wrapper                                                */
/*=====================================================================*/
#include <stddef.h>
#include <self-mod.h>
#include <bglpatch.h>
#include <bigloo.h>

/* {*---------------------------------------------------------------------*} */
/* {*    int                                                              *} */
/* {*    bgl_patch_mode ...                                               *} */
/* {*---------------------------------------------------------------------*} */
/* int bgl_patch_mode = 0;                                             */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    void                                                             *} */
/* {*    bgl_init_patch_32 ...                                            *} */
/* {*---------------------------------------------------------------------*} */
/* void                                                                */
/* bgl_init_patch_32( void *fn, size_t len, patch_descr *patch_tab ) { */
/*    long i;                                                          */
/*    init_patch_32( fn, len, patch_tab );                             */
/*                                                                     */
/*    fprintf( stderr, "bgl_init_patch32...\n" );                      */
/*    for( i = 0; i < len; i++ ) {                                     */
/*       patch_32( patch_tab + i, 0 );                                 */
/*    }                                                                */
/* }                                                                   */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    void                                                             *} */
/* {*    bgl_init_patch_64 ...                                            *} */
/* {*---------------------------------------------------------------------*} */
/* void                                                                */
/* bgl_init_patch_64( void *fn, size_t len, patch_descr *patch_tab ) { */
/*    init_patch_64( fn, len, patch_tab );                             */
/*    long i;                                                          */
/*                                                                     */
/*    fprintf( stderr, "bgl_init_patch64...%d\n", BFALSE );            */
/*    for( i = 0; i < len; i++ ) {                                     */
/*       patch_64( patch_tab + i, (long)BFALSE );                      */
/*    }                                                                */
/* }                                                                   */

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_patch ...                                               */
/*---------------------------------------------------------------------*/
void
bgl_init_patch( bgl_patch_descr_t *bgl_patch_descr ) {
}
