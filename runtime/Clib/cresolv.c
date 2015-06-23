/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cresolv.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Jun  6 11:04:39 2015                          */
/*    Last change :  Tue Jun 23 17:50:46 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Resolv library binding (optional)                                */
/*=====================================================================*/
#include <bigloo.h>

#if( BGL_HAVE_RESOLV )
#  include <stdio.h>
#  include <stdlib.h>
#  include <unistd.h>
#  include <netinet/in.h>
#  include <resolv.h>
#  include <netdb.h>
#  include <regex.h>

#  define N 4096
#endif

typedef obj_t (*fmt_t)( ns_msg*, int );

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    nstype ...                                                       */
/*---------------------------------------------------------------------*/
static int
nstype( obj_t name ) {
   char *n = BSTRING_TO_STRING( name );

   if( !strcmp( n, "ns_t_invalid" ) ) { return ns_t_invalid; }
   if( !strcmp( n, "ns_t_a" ) ) { return ns_t_a; }
   if( !strcmp( n, "ns_t_ns" ) ) { return ns_t_ns; }
   if( !strcmp( n, "ns_t_md" ) ) { return ns_t_md; }
   if( !strcmp( n, "ns_t_mf" ) ) { return ns_t_mf; }
   if( !strcmp( n, "ns_t_cname" ) ) { return ns_t_cname; }
   if( !strcmp( n, "ns_t_soa" ) ) { return ns_t_soa; }
   if( !strcmp( n, "ns_t_mb" ) ) { return ns_t_mb; }
   if( !strcmp( n, "ns_t_mg" ) ) { return ns_t_mg; }
   if( !strcmp( n, "ns_t_mr" ) ) { return ns_t_mr; }
   if( !strcmp( n, "ns_t_null" ) ) { return ns_t_null; }
   if( !strcmp( n, "ns_t_wks" ) ) { return ns_t_wks; }
   if( !strcmp( n, "ns_t_ptr" ) ) { return ns_t_ptr; }
   if( !strcmp( n, "ns_t_hinfo" ) ) { return ns_t_hinfo; }
   if( !strcmp( n, "ns_t_minfo" ) ) { return ns_t_minfo; }
   if( !strcmp( n, "ns_t_mx" ) ) { return ns_t_mx; }
   if( !strcmp( n, "ns_t_txt" ) ) { return ns_t_txt; }
   if( !strcmp( n, "ns_t_rp" ) ) { return ns_t_rp; }
   if( !strcmp( n, "ns_t_afsdb" ) ) { return ns_t_afsdb; }
   if( !strcmp( n, "ns_t_x25" ) ) { return ns_t_x25; }
   if( !strcmp( n, "ns_t_isdn" ) ) { return ns_t_isdn; }
   if( !strcmp( n, "ns_t_rt" ) ) { return ns_t_rt; }
   if( !strcmp( n, "ns_t_nsap" ) ) { return ns_t_nsap; }
   if( !strcmp( n, "ns_t_nsap_ptr" ) ) { return ns_t_nsap_ptr; }
   if( !strcmp( n, "ns_t_sig" ) ) { return ns_t_sig; }
   if( !strcmp( n, "ns_t_key" ) ) { return ns_t_key; }
   if( !strcmp( n, "ns_t_px" ) ) { return ns_t_px; }
   if( !strcmp( n, "ns_t_gpos" ) ) { return ns_t_gpos; }
   if( !strcmp( n, "ns_t_aaaa" ) ) { return ns_t_aaaa; }
   if( !strcmp( n, "ns_t_loc" ) ) { return ns_t_loc; }
   if( !strcmp( n, "ns_t_nxt" ) ) { return ns_t_nxt; }
   if( !strcmp( n, "ns_t_eid" ) ) { return ns_t_eid; }
   if( !strcmp( n, "ns_t_nimloc" ) ) { return ns_t_nimloc; }
   if( !strcmp( n, "ns_t_srv" ) ) { return ns_t_srv; }
   if( !strcmp( n, "ns_t_atma" ) ) { return ns_t_atma; }
   if( !strcmp( n, "ns_t_naptr" ) ) { return ns_t_naptr; }
   if( !strcmp( n, "ns_t_kx" ) ) { return ns_t_kx; }
   if( !strcmp( n, "ns_t_cert" ) ) { return ns_t_cert; }
   if( !strcmp( n, "ns_t_a6" ) ) { return ns_t_a6; }
   if( !strcmp( n, "ns_t_dname" ) ) { return ns_t_dname; }
   if( !strcmp( n, "ns_t_sink" ) ) { return ns_t_sink; }
   if( !strcmp( n, "ns_t_opt" ) ) { return ns_t_opt; }
   // if( !strcmp( n, "ns_t_apl" ) ) { return ns_t_apl; }
   if( !strcmp( n, "ns_t_tkey" ) ) { return ns_t_tkey; }
   if( !strcmp( n, "ns_t_tsig" ) ) { return ns_t_tsig; }
   if( !strcmp( n, "ns_t_ixfr" ) ) { return ns_t_ixfr; }
   if( !strcmp( n, "ns_t_axfr" ) ) { return ns_t_axfr; }
   if( !strcmp( n, "ns_t_mailb" ) ) { return ns_t_mailb; }
   if( !strcmp( n, "ns_t_maila" ) ) { return ns_t_maila; }
   if( !strcmp( n, "ns_t_any" ) ) { return ns_t_any; }
   if( !strcmp( n, "ns_t_zxfr" ) ) { return ns_t_zxfr; }
   
   C_SYSTEM_FAILURE( BGL_ERROR, "resolv", "bad query type", name );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    rr_format_mx ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
rr_format_mx( ns_msg *msg, int i ) {
   ns_rr rr;
   char *name;
   int len;
   char *ex;
   obj_t host;
   obj_t pri;
   char dispbuf[ N ];
	     
   ns_parserr( msg, ns_s_an, i, &rr );
   len = ns_sprintrr( msg, &rr, NULL, NULL, dispbuf, sizeof( dispbuf ) );
   ex = rindex( dispbuf, ' ' );

   if( ex ) {
      host = string_to_bstring_len( ex+ 1, len - (ex-dispbuf) - 2 );
      pri = BINT( ns_get16( ns_rr_rdata( rr ) ) );

      return MAKE_PAIR( host, pri );
   } else {
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    rr_format_srv ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
rr_format_srv( ns_msg *msg, int i ) {
   ns_rr rr;
   char *name;
   int len;
   char *host;
   char dispbuf[ N ];
	     
   ns_parserr( msg, ns_s_an, i, &rr );
   len = ns_sprintrr( msg, &rr, NULL, NULL, dispbuf, sizeof( dispbuf ) );
   host = rindex( dispbuf, ' ' );

   if( host ) {
      char *port;

      *host = 0;
      port = rindex( dispbuf, ' ' );	 

      if( port ) {
	 char *weight;
	 
	 *port = 0;
	 weight = rindex( dispbuf, ' ' );

	 if( weight ) {
	    int p = atoi( port + 1 );
	    int w = atoi( weight + 1 );
	    int pri = ns_get16( ns_rr_rdata( rr ) );

	    return MAKE_PAIR(
	       string_to_bstring_len( host + 1, len - (host-dispbuf) - 2 ),
	       MAKE_PAIR( BINT( pri ),
			  MAKE_PAIR( BINT( w ),
				     MAKE_PAIR( BINT( p ), BNIL ) ) ) );
	 } else {
	    return BUNSPEC;
	 } 
      } else {
	 return BUNSPEC;
      }
   } else {
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    rr_format_naptr ...                                              */
/*---------------------------------------------------------------------*/
static obj_t
rr_format_naptr( ns_msg *msg, int i ) {
   ns_rr rr;
   char *name;
   int len;
   char *rep;
   char dispbuf[ N ];
   regex_t re;
   regmatch_t pmatch[ 8 ];
   
   if( regcomp( &re, "[^ \t]*[ \t]*[^ \t]*[ \t]*[^ \t]*[ \t]*[^ \t]*[ \t]*([0-9]*)[ \t]*([0-9]*)[ \t]*\"([^\"]*)\"[ \t]*\"([^\"]*)\"[ \t]*\"([^\"]*)\"[ \t]*([^ \t]*)", REG_EXTENDED ) ) {
       C_SYSTEM_FAILURE( BGL_ERROR, "resolv", "Cannot compile regular expression", BUNSPEC );
   }
	     
   ns_parserr( msg, ns_s_an, i, &rr );
   len = ns_sprintrr( msg, &rr, NULL, NULL, dispbuf, sizeof( dispbuf ) );
   
   if( !regexec( &re, dispbuf, sizeof( pmatch ), pmatch, 0 ) ) {
      regfree( &re );
      return MAKE_PAIR(
	 // replacement
	 string_to_bstring_len( &(dispbuf[ pmatch[ 6 ].rm_so ]), pmatch[ 6 ].rm_eo - pmatch[ 6 ].rm_so - 1),
	 MAKE_PAIR(
	    // regexp
	    string_to_bstring_len( &(dispbuf[ pmatch[ 5 ].rm_so ]), pmatch[ 5 ].rm_eo - pmatch[ 5 ].rm_so ),
	    // service
	    MAKE_PAIR(
	       string_to_bstring_len( &(dispbuf[ pmatch[ 4 ].rm_so ]), pmatch[ 4 ].rm_eo - pmatch[ 4 ].rm_so ),
	       MAKE_PAIR( 
		  // flags
		  string_to_bstring_len( &(dispbuf[ pmatch[ 3 ].rm_so ]), pmatch[ 3 ].rm_eo - pmatch[ 3 ].rm_so ),
		  MAKE_PAIR(
		     // order
		     BINT( atoi( &(dispbuf[ pmatch[ 1 ].rm_so ]) ) ),
		     MAKE_PAIR(
			// preference
			BINT( atoi( &(dispbuf[ pmatch[ 2 ].rm_so ]) ) ),
			BNIL ) ) ) ) ) );
   } else {
      regfree( &re );
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    rr_format_cname ...                                              */
/*---------------------------------------------------------------------*/
static obj_t
rr_format_cname( ns_msg *msg, int i ) {
   ns_rr rr;
   char *name;
   int len;
   char *ex;
   char dispbuf[ N ];
	     
   ns_parserr( msg, ns_s_an, i, &rr );
   len = ns_sprintrr( msg, &rr, NULL, NULL, dispbuf, sizeof( dispbuf ) );
   ex = rindex( dispbuf, ' ' );

   if( ex ) {
      return string_to_bstring_len( ex+ 1, len - (ex-dispbuf) - 2 );
   } else {
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    rr_format_txt ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
rr_format_txt( ns_msg *msg, int i ) {
   ns_rr rr;
   char *name;
   int len;
   char *ex2;
   char dispbuf[ N ];
	     
   ns_parserr( msg, ns_s_an, i, &rr );
   len = ns_sprintrr( msg, &rr, NULL, NULL, dispbuf, sizeof( dispbuf ) );
   ex2 = rindex( dispbuf, '\"' );

   if( ex2 ) {
      char *ex;
      *ex2 = 0;
      ex = rindex( dispbuf, '\"' );
      
      return string_to_bstring_len( ex+ 1, len - (ex-dispbuf) - 2 );
   } else {
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    rr_format_default ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
rr_format_default( ns_msg *msg, int i ) {
   ns_rr rr;
   char *name;
   char dispbuf[ N ];
	     
   ns_parserr( msg, ns_s_an, i, &rr );

   return string_to_bstring( ns_rr_name( rr ) );
}

/*---------------------------------------------------------------------*/
/*    fmt_t                                                            */
/*    get_rr_format ...                                                */
/*---------------------------------------------------------------------*/
fmt_t
static get_rr_format( int nstyp ) {
   switch( nstyp ) {
      case ns_t_mx: return &rr_format_mx;
      case ns_t_srv: return &rr_format_srv;
      case ns_t_naptr: return &rr_format_naptr;
      case ns_t_cname: return &rr_format_cname;
      case ns_t_txt: return &rr_format_txt;
      default: return &rr_format_default;
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_res_query ...                                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_res_query( obj_t name, obj_t nsname ) {
#if( BGL_HAVE_RESOLV )
    u_char nsbuf[ N ];
    ns_msg msg;
    int l;
    int nstyp = nstype( nsname );

    l = res_query( BSTRING_TO_STRING( name ), ns_c_any,
		   nstyp, nsbuf, sizeof( nsbuf ) );
    if( l < 0 ) {
       C_SYSTEM_FAILURE( BGL_ERROR, "resolv", strerror( l ), name );
       return BFALSE;
    } else {
       ns_initparse( nsbuf, l, &msg );
       if( (l = ns_msg_count( msg, ns_s_an )) < 0 ) {
	  C_SYSTEM_FAILURE( BGL_ERROR, "resolv", strerror( l ), name );
       } else {
	  int i;
	  obj_t res = create_vector( l );
	  fmt_t fmt = get_rr_format( nstyp );

	  for( i = 0; i < l; i++ ) {
	     VECTOR_SET( res, i, fmt( &msg, i ) );
	  }

	  return res;
       }
    }
#else
    return create_vector( 0 );
#endif    
}
