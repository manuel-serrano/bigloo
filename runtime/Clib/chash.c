/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/chash.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Nov 26 15:23:07 1993                          */
/*    Last change :  Sun Mar 18 07:19:30 2018 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Le hashage                                                       */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    A static table to compute small hash values                      */
/*---------------------------------------------------------------------*/
static char hash_random_table[] = {
   (char)1, (char)14, (char)110, (char)25, (char)97, (char)174, (char)132,
   (char)119, (char)138, (char)170, (char)125, (char)118, (char)27,
   (char)233, (char)140, (char)51,
   (char)87, (char)197, (char)177, (char)107, (char)234, (char)169,
   (char)56, (char)68, (char)30, (char)7, (char)173, (char)73, (char)188,
   (char)40, (char)36, (char)65,
   (char)49, (char)213, (char)104, (char)190, (char)57, (char)211,
   (char)148, (char)223, (char)48, (char)115, (char)15, (char)2, (char)67,
   (char)186, (char)210, (char)28,
   (char)12, (char)181, (char)103, (char)70, (char)22, (char)58, (char)75,
   (char)78, (char)183, (char)167, (char)238, (char)157, (char)124,
   (char)147, (char)172, (char)144,
   (char)176, (char)161, (char)141, (char)86, (char)60, (char)66, (char)128,
   (char)83, (char)156, (char)241, (char)79, (char)46, (char)168, (char)198,
   (char)41, (char)254,
   (char)178, (char)85, (char)253, (char)237, (char)250, (char)154,
   (char)133, (char)88, (char)35, (char)206, (char)95, (char)116,
   (char)252, (char)192, (char)54, (char)221,
   (char)102, (char)218, (char)255, (char)240, (char)82, (char)106,
   (char)158, (char)201, (char)61, (char)3, (char)89, (char)9, (char)42,
   (char)155, (char)159, (char)93,
   (char)166, (char)80, (char)50, (char)34, (char)175, (char)195, (char)100,
   (char)99, (char)26, (char)150, (char)16, (char)145, (char)4, (char)33,
   (char)8, (char)189,
   (char)121, (char)64, (char)77, (char)72, (char)208, (char)245, (char)130,
   (char)122, (char)143, (char)55, (char)105, (char)134, (char)29,
   (char)164, (char)185, (char)194,
   (char)193, (char)239, (char)101, (char)242, (char)5, (char)171,
   (char)126, (char)11, (char)74, (char)59, (char)137, (char)228,
   (char)108, (char)191, (char)232, (char)139,
   (char)6, (char)24, (char)81, (char)20, (char)127, (char)17, (char)91,
   (char)92, (char)251, (char)151, (char)225, (char)207, (char)21,
   (char)98, (char)113, (char)112,
   (char)84, (char)226, (char)18, (char)214, (char)199, (char)187,
   (char)13, (char)32, (char)94, (char)220, (char)224, (char)212,
   (char)247, (char)204, (char)196, (char)43,
   (char)249, (char)236, (char)45, (char)244, (char)111, (char)182,
   (char)153, (char)136, (char)129, (char)90, (char)217, (char)202,
   (char)19, (char)165, (char)231, (char)71,
   (char)230, (char)142, (char)96, (char)227, (char)62, (char)179,
   (char)246, (char)114, (char)162, (char)53, (char)160, (char)215,
   (char)205, (char)180, (char)47, (char)109,
   (char)44, (char)38, (char)31, (char)149, (char)135, (char)0,
   (char)216, (char)52, (char)63, (char)23, (char)37, (char)69, (char)39,
   (char)117, (char)146, (char)184,
   (char)163, (char)200, (char)222, (char)235, (char)248, (char)243,
   (char)219, (char)10, (char)152, (char)131, (char)123, (char)229,
   (char)203, (char)76, (char)120, (char)209
};

/*---------------------------------------------------------------------*/
/*    unsigned char                                                    */
/*    get_hash_number ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF unsigned char
get_hash_number( char *string ) {
   unsigned char hash = 0;

   while( *string )
      hash = hash_random_table[ hash ^ (unsigned char)(*string++) ];
      
   return hash;
}

/*---------------------------------------------------------------------*/
/*    unsigned char                                                    */
/*    bgl_get_hash_number_len ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF unsigned char
bgl_get_hash_number_len( char *string, int start, int len ) {
   unsigned char hash = 0;
   int i;

   for( i = start; i < len; i++ )
      hash = hash_random_table[ hash ^ (unsigned char)(*string++) ];
      
   return hash;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_hash_power_number ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
get_hash_power_number( char *string, unsigned long power ) {
   char c;
   unsigned long result = 0;

   while( (c = *string++) )
      result += (result << 3) + (long)c;

   return result & ((1 << power) - 1);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_hash_power_number_len ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
get_hash_power_number_len( char *string, unsigned long power, long len ) {
   unsigned long result = 0;

   while( len-- ) {
      result += (result << 3) + (long)(*string++);
   }

   return result & ((1 << power) - 1);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_hash_number_from_int ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
get_hash_number_from_int( unsigned long i ) {
   unsigned char hash = 0;

   while( i > 0 )
   {
      hash = hash_random_table[ hash ^ ((unsigned char)(i & 255)) ];
      i >>= 8;
   }

   return hash;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_hash_number_from_pointer ...                                 */
/*---------------------------------------------------------------------*/
long
get_hash_number_from_pointer( void * i ) {
   return get_hash_number_from_int( (unsigned long)i );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_hash_power_number_from_int ...                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
get_hash_power_number_from_int( unsigned long i, unsigned long power ) {
   unsigned long result = 0;
	
   while( i > 0 ) {
      result += (result << 3) + (long)(i & 255);
      i >>= 8;
   }

   return result & ((1 << power) - 1);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    get_hash_power_number_from_pointer ...                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_pointer_hashnumber( void * i, unsigned long power ) {
   return get_hash_power_number_from_int( (unsigned long)i, power );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_string_hash_number ...                                       */
/*    -------------------------------------------------------------    */
/*    New setting has been provided by Joseph Donaldson on March 2011. */
/*---------------------------------------------------------------------*/
long
bgl_string_hash_number( char *string ) {
   char c;
   long result = 5381;
   char *string0 = string + 1;

   while( c = *string++ ) {
      result += (result << 5) + c;
   }

   // MS, 7sep2020: add string length to distinguish 0-filled strings
   // such as "01" and "001"
   return (result + (string - string0)) & ((1 << 29) - 1);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_string_hash_number ...                                       */
/*---------------------------------------------------------------------*/
long
bgl_string_hash( char *string, int start, int end ) {
   int i;
   long result = 5381;

   for( i = start; i < end; i++ ) {
      result += (result << 5) + (long)string[ i ];
   }

   // MS, 7sep2020, see bgl_string_hash_number
   return (result + (end - start)) & ((1 << 29) - 1);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_symbol_hash_number ...                                       */
/*---------------------------------------------------------------------*/
long
bgl_symbol_hash_number( obj_t s ) {
   return 1 + bgl_string_hash_number(BSTRING_TO_STRING(SYMBOL_TO_STRING(s)));
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_keyword_hash_number ...                                      */
/*---------------------------------------------------------------------*/
long
bgl_keyword_hash_number( obj_t s ) {
   return 2 + bgl_string_hash_number(BSTRING_TO_STRING(KEYWORD_TO_STRING(s)));
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_obj_hash_number ...                                          */
/*---------------------------------------------------------------------*/
long
bgl_obj_hash_number( obj_t obj ) {
   return ((long)((long)(CREF( obj )) >> TAG_SHIFT));
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_foreign_hash_number ...                                      */
/*---------------------------------------------------------------------*/
long
bgl_foreign_hash_number( obj_t obj ) {
   return (long)FOREIGN_TO_COBJ( obj );
}
