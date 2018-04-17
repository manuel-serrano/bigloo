/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cforeign.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 08:45:22 1993                          */
/*    Last change :  Tue Apr 17 08:00:01 2018 (serrano)                */
/*    -------------------------------------------------------------    */
/*    La gestion de l'interface etrangere                              */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Importations                                                     */
/*---------------------------------------------------------------------*/
extern obj_t string_to_symbol( char * );

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    cobj_to_foreign ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
cobj_to_foreign( obj_t id, void *cobj ) {
   obj_t handle;

   handle = GC_MALLOC( FOREIGN_SIZE );

   handle->foreign.header = MAKE_HEADER( FOREIGN_TYPE, FOREIGN_SIZE );
   handle->foreign.cobj = (void *)cobj;
   handle->foreign.id = id;

   return BREF( handle );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    void_star_to_obj ...                                             */
/*---------------------------------------------------------------------*/
BGL_EXPORTED_DEF
obj_t
void_star_to_obj( void *cobj ) {
   static obj_t id = BUNSPEC;

   if( !SYMBOLP( id ) )
      id = string_to_symbol( "VOID*" );

   return cobj_to_foreign( id, cobj );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    obj_to_cobj ...                                                  */
/*---------------------------------------------------------------------*/
BGL_EXPORTED_DEF
long
obj_to_cobj( obj_t obj ) {
   if( INTEGERP( obj ) )
      return (long)CINT( obj );
   if( BOOLEANP( obj ) )
      return (long)((long)CBOOL( obj ));
   if( STRINGP( obj ) )
      return (long)BSTRING_TO_STRING( obj );
   if( CHARP( obj ) )
      return (long)((long)CCHAR( obj ));
   if( FOREIGNP( obj ) )
      return (long)FOREIGN_TO_COBJ( obj );
   if( REALP( obj ) )
      return (long)the_failure( string_to_bstring( "obj->cobj" ),
				string_to_bstring( "Can't cast a real to foreign" ),
				obj);
   else
      return (long)the_failure( string_to_bstring( "obj->cobj" ),
				string_to_bstring( "Illegal object type" ),
				obj);
}
 
