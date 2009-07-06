/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bdb/blib/table.c                     */
/*    -------------------------------------------------------------    */
/*    Author      :  unknown                                           */
/*    Creation    :  Wed Jul 28 07:20:32 1999                          */
/*    Last change :  Wed Jul 12 10:39:47 2000 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The mangling/demangling table management.                        */
/*    -------------------------------------------------------------    */
/*    The small C interface moslty used because Bigloo produces an     */
/*    huge C array that it is hard to parse in Scheme.                 */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    The type used by Bigloo for the bdb table.                       */
/*---------------------------------------------------------------------*/
struct bdb_fun_info { char *sname, *cname; };

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bdb_table_to_list ...                                            */
/*    -------------------------------------------------------------    */
/*    This function construct a list of binding declarations from      */
/*    the table that has been build by the compiler in -gbdb mode.     */
/*---------------------------------------------------------------------*/
obj_t
bdb_table_to_list( obj_t bdb ) {
   obj_t cla_info  = BNIL;
   obj_t glo_info  = BNIL;
   obj_t mod_info  = BNIL;
   obj_t init_info = BNIL;
   obj_t lnum_info = BNIL;
   struct bdb_fun_info *table_entry = (struct bdb_fun_info *)bdb;
   obj_t src_info = BNIL;
   long mod_lnum;

   /* we start reading the signature for the table in,     */
   /* just to check that the compiled table format is      */
   /* compatible with this version of blib.                */
   if( !(table_entry->sname == BDB_LIBRARY_MAGIC_NUMBER) ||
       !(table_entry->cname == BDB_LIBRARY_MAGIC_NUMBER) ) {
      fprintf( stderr,
	       "***ERROR: Incompatible versions -- "
	       "Bigloo compiler/Bdb library");
      exit( -1 );
   } else {
      table_entry++;
   }
   
   /* we start fetching the module name and its            */
   /* C initialization function.                           */
   mod_info  = string_to_bstring( table_entry->sname );
   init_info = string_to_bstring( table_entry->cname );
   table_entry++;

   /* and the source files implementing this module.       */
   while( ((int *)table_entry->sname) ) {
      obj_t pair = MAKE_PAIR(string_to_bstring(table_entry->sname), src_info);
      src_info = pair;
      
      table_entry++;
   }
   
   mod_lnum  = (long)table_entry->cname;
   src_info  = MAKE_PAIR( init_info, src_info );
   lnum_info = MAKE_PAIR( BINT( mod_lnum ), src_info );
   mod_info  = MAKE_PAIR( mod_info, lnum_info );
   table_entry++;
   
   /* then we fetch global variables informations         */
   while( *((int *)table_entry) && table_entry->sname ) {
      char *fname, *sname, *cname;
      long lnum;
      obj_t pair = BNIL;
      obj_t entry = BNIL;

      /* we first fetch the source file name and line num */
      fname = table_entry->sname;
      lnum  = (long)table_entry->cname;
      table_entry++;

      /* now, we pickup the global scheme and C names     */
      sname = table_entry->sname;
      cname = table_entry->cname;

      /* is it a global function or a global variable ?   */
      if( !cname ) {
	 /* thie is a global function                     */
	 char *bp_cname;
	 obj_t pair2;

	 table_entry++;

	 cname    = table_entry->sname;
	 bp_cname = table_entry->cname;

	 pair2 = MAKE_PAIR( cname ? string_to_bstring( cname ) : BUNSPEC,
			    BINT( lnum ) );
	 pair2 = MAKE_PAIR( pair2, string_to_bstring( bp_cname ) );
	 table_entry++;
	 
	 /* this is a global function, we are now free    */
	 /* to parse the local variables                  */
	 while( table_entry->sname ) {
	    pair = MAKE_PAIR( string_to_bstring( table_entry->sname ),
			      string_to_bstring( table_entry->cname ) );

	    entry = MAKE_PAIR( pair, entry );

	    table_entry++;
	 }

	 pair2 = MAKE_PAIR( pair2, BNIL );
	 pair  = MAKE_PAIR( string_to_bstring( sname ), pair2 );
      } else {
	 /* this is a global variable.                    */
	 pair = MAKE_PAIR( string_to_bstring( sname ),
			   string_to_bstring( cname ) );
      }
      
      entry = MAKE_PAIR( pair, entry );
      entry = MAKE_PAIR( string_to_bstring( fname ), entry );
	 
      table_entry++;
      glo_info = MAKE_PAIR( entry, glo_info );
   }
   table_entry++;

   /* then we fetch classes information                   */
   while( *((int *)table_entry) && table_entry->sname ) {
      cla_info = MAKE_PAIR( string_to_bstring( table_entry->sname ),
			    cla_info );
      table_entry++;
   }

   /* We now build the returned list to Bdb               */
   {
      obj_t aux;

      aux = MAKE_PAIR( glo_info, cla_info );
      return MAKE_PAIR( mod_info, aux );
   }
}
