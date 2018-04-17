/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/ccustom.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Aug 16 16:56:24 1999                          */
/*    Last change :  Tue Apr 17 08:00:50 2018 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The custom management.                                           */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    custom_default_equal ...                                         */
/*---------------------------------------------------------------------*/
static int
custom_default_equal( obj_t obj1, obj_t obj2 ) {
   return obj1 == obj2;
}
   
/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    custom_default_hash ...                                          */
/*---------------------------------------------------------------------*/
static long
custom_default_hash( obj_t obj ) {
   return (long)obj;
}
   
/*---------------------------------------------------------------------*/
/*    static char *                                                    */
/*    custom_default_to_string ...                                     */
/*---------------------------------------------------------------------*/
static char *
custom_default_to_string( obj_t obj, char *buffer, int len ) {
   if( len > 16 ) {
      sprintf( buffer, "<custom:%p>", obj );
      return buffer;
   } else {
      return "<custom>";
   }
}
   
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    custom_default_output ...                                        */
/*---------------------------------------------------------------------*/
static obj_t
custom_default_output( obj_t obj, FILE *file ) {
   fprintf( file, "<custom:%p>", obj );
   return obj;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    create_custom ...                                                */
/*    -------------------------------------------------------------    */
/*    LEN is number of additional bytes to be appended to the standard */
/*    custom structure.                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
create_custom( long len ) {
   obj_t custom;

   custom = GC_MALLOC_ATOMIC( CUSTOM_SIZE + len );

   custom->custom.header = MAKE_HEADER( CUSTOM_TYPE, 0 );
   custom->custom.final = 0L;
   custom->custom.identifier = 0L;
   custom->custom.equal = custom_default_equal;
   custom->custom.hash = custom_default_hash;
   custom->custom.to_string  = custom_default_to_string;
   custom->custom.output = custom_default_output;

   return BREF( custom );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_custom_nil ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_custom_nil() {
   static obj_t custom_nil = 0;
   
   if( !custom_nil ) custom_nil = create_custom( 0 );
   
   return custom_nil;
}
