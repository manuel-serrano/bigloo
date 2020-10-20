/*=====================================================================*/
/*    /tmp/OFAOT/nan/lib/bigloo/4.3h/bigloo.h                          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar 16 18:48:21 1995                          */
/*    Last change :  Tue Apr 28 10:36:27 2020 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Bigloo's stuff                                                   */
/*=====================================================================*/
#ifndef BIGLOO_H 
#define BIGLOO_H

/*---------------------------------------------------------------------*/
/*    Does someone really wants C++ here?                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus_just_for_emacs_indent
}
#endif

/*---------------------------------------------------------------------*/
/*    Import/export                                                    */
/*    -------------------------------------------------------------    */
/*    These definitions are overriden in bigloo_config.h.              */
/*    They are given here for a documentation purpose.                 */
/*---------------------------------------------------------------------*/
#if( !defined( BGL_IMPORT ) )
#  define BGL_IMPORT extern
#endif
#if( !defined( BGL_EXPORTED_DECL ) )
#  define BGL_EXPORTED_DECL extern
#endif
#if( !defined( BGL_EXPORTED_DEF ) )
#  define BGL_EXPORTED_DEF 
#endif
#if( !defined( BGL_RUNTIME_DECL ) )
#  define BGL_RUNTIME_DECL extern
#endif
#if( !defined( BGL_RUNTIME_DEF ) )
#  define BGL_RUNTIME_DEF
#endif

/*---------------------------------------------------------------------*/
/*    The essential includes                                           */
/*---------------------------------------------------------------------*/
#include <stdio.h>
#include <setjmp.h>
#include <errno.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <time.h>
#if !defined( _MSC_VER ) && !defined( _MINGW_VER )
#  include <unistd.h>
#endif
#include <signal.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef _BGL_WIN32_VER
#  include <sys/socket.h>
#  include <netinet/in.h>
#else
#  include <winsock2.h>
#  include <mswsock.h>
#  include <ws2tcpip.h>   
#endif

/*---------------------------------------------------------------------*/
/*    BIGLOO_MAIN ...                                                  */
/*    -------------------------------------------------------------    */
/*    In order to use a custom C `main' function, defines this         */
/*    macro to another value (e.g. bmain (don't use the _bigloo_main   */
/*    or bigloo_main because they are alread used)). Then in your      */
/*    own `main' function, invoke this one.                            */
/*---------------------------------------------------------------------*/
#if( !defined( BIGLOO_MAIN ) )
#  define BIGLOO_MAIN main
#endif

/*---------------------------------------------------------------------*/
/*    BDB_LIBRARY_MAGIC_NUMBER                                         */
/*    -------------------------------------------------------------    */
/*    This number is used to ensure the compatibility between the      */
/*    compiler and the blib library.                                   */
/*---------------------------------------------------------------------*/
#define BDB_LIBRARY_MAGIC_NUMBER (0x1024)
       
/*---------------------------------------------------------------------*/
/*    BIGLOO_EXIT                                                      */
/*---------------------------------------------------------------------*/
#if( !defined( BIGLOO_EXIT ) )
#  define BIGLOO_EXIT bigloo_exit
#endif

/*---------------------------------------------------------------------*/
/*    Global configuration                                             */
/*---------------------------------------------------------------------*/
#include <bigloo_config.h>

/*---------------------------------------------------------------------*/
/*    constant alignment                                               */
/*---------------------------------------------------------------------*/
#define __CNST_ALIGN double _;   
#define __CNST_FILLER 0.0,

/*---------------------------------------------------------------------*/
/*    Constants pool                                                   */
/*    -------------------------------------------------------------    */
/*    See comptile/Cnst/read-alloc.scm                                 */
/*---------------------------------------------------------------------*/
#define CNST_TABLE_REF( offset ) __cnst[ offset ]
#define CNST_TABLE_SET( offset, value ) BMASSIGN( __cnst[ offset ], value)

/*---------------------------------------------------------------------*/
/*    32 bit tagging:                                                  */
/*    -------------------------------------------------------------    */
/*    - allocated values:                                              */
/*    +--------+--------+--------+--------+                            */
/*    |....signed fixed point value.....??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    - 30 bits immediate values (integers):                           */
/*    +--------+--------+--------+--------+                            */
/*    |....signed fixed point value.....??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    - 6 bits constants (booleans, nil, unspecified, ...):            */
/*    +--------+--------+--------+--------+                            */
/*    |.................|..xxxxxx|mmmmmm??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    - 8 bits immediate values (chars, int8):                         */
/*    +--------+--------+--------+--------+                            */
/*    |.................|xxxxxxxx|mmmmmm??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    - 16 bits immediate values (ucs2, int16):                        */
/*    +--------+--------+--------+--------+                            */
/*    |xxxxxxxx|xxxxxxxx|........|mmmmmm??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    -------------------------------------------------------------    */
/*    64 bit tagging:                                                  */
/*    -------------------------------------------------------------    */
/*    - pointers (vector, cell, real):                                 */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*    |...........................pointer....................... ???|  */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*                                                                     */
/*    - int61:                                                         */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*    |xxxxxxxx|xxxxxxxx|xxxxxxxx|.......|...................... ???|  */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*                                                                     */
/*    -------------------------------------------------------------    */
/*    64 nan tagging:                                                  */
/*    -------------------------------------------------------------    */
/*    - pointers (vector, cell, real):                                 */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*    |x1111111|1111????|.........pointer...........................|  */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*                                                                     */
/*    - int32:                                                         */
/*    +--------+--------+- ... -+--------+--------+--------+--------+  */
/*    |x1111111|1111????|.......|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|  */
/*    +--------+--------+- ... -+--------+--------+--------+--------+  */
/*                                                                     */
/*    -------------------------------------------------------------    */
/*    64 smi tagging:                                                  */
/*    -------------------------------------------------------------    */
/*    - pointers (vector, cell, real):                                 */
/*                                                                     */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*    |...........................pointer....................... ???|  */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*                                                                     */
/*    - int32:                                                         */
/*    +--------+--------+--------+--------|- ... -+--------+--------+  */
/*    |xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|_ ... ________________???|  */
/*    +--------+--------+--------+--------|- ... -+--------+--------+  */
/*                                                                     */
/*---------------------------------------------------------------------*/
#if( BGL_NAN_TAGGING ) /* BGL_NAN_TAGGING */
#  define TAG_MASK (0xffffUL << 48)
#  define TAG_MASKOBJECT (0x7fffUL << 48)
#  define NAN_MASK ((1UL << 48) - 1)
#  define NAN_MASK_SIGNED (NAN_MASK | (1UL <<63))
#  define NAN_TAG ((1UL << 51) + (1UL << 63))
#  define TAG_SHIFT 0

#  define TAG( _v, shift, tag ) \
     ((long)((((unsigned long)(_v) & ~(0xfULL << 48)) | tag)))
#  define UNTAG( _v, shift, tag ) \
     ((long)(((unsigned long)(_v) & NAN_MASK)))

#  define BGL_CNSTP( o, header, shift ) \
     ((((unsigned long)o & ~0xffffffffUL) == ((unsigned long)header)))
#  define BGL_CNST_TO_BCNST( o, mask, header, shift, type )	\
     ((obj_t)(header | ((long)(o & mask))))
#  define BGL_BCNST_TO_CNST( o, mask, shift, type ) \
     ((type)((unsigned long)o & mask))
#else /* !BGL_NAN_TAGGING */
#  define TAG_SHIFT PTR_ALIGNMENT
#  define TAG_MASK ((1 << PTR_ALIGNMENT) - 1)
#  define TAG_MASKOBJECT TAG_MASK

#  define TAG( _v, shift, tag ) \
     ((long)(((unsigned long)(_v) << shift) | tag))
#  define UNTAG( _v, shift, tag ) \
     ((long)((long)(_v) >> shift))

#  define BGL_CNSTP( o, header, shift ) \
     (CNST32P( o ) && (((unsigned long)(o) & (long)(((long)1 << (shift)) -1)) == CCNST_MASK((long)header)) )
#  define BGL_CNST_TO_BCNST( o, mask, header, shift, type )	\
     ((obj_t)(header + ((unsigned long)((type)(o)) << shift)))
#  define BGL_BCNST_TO_CNST( o, mask, shift, type ) \
     ((type)CCNST_MASK((unsigned long)(o) >> shift))  
#endif  /* BGL_NAN_TAGGING */

#define BGL_CNST_SHIFT_CHAR 8
#define BGL_CNST_SHIFT_INT16 16
#define BGL_CNST_SHIFT_UCS2 16

#if( PTR_ALIGNMENT >= 3 )
#  define BGL_CNST_SHIFT_INT32 32
#endif

/*---------------------------------------------------------------------*/
/*    The tagged pointers ...                                          */
/*---------------------------------------------------------------------*/
#if( BGL_NAN_TAGGING )
#  define TAG_QNAN (0x7ff8UL<<48)
#  define TAG_SNAN (0xfff8UL<<48)
#  define TAG_INT (0x7ff9L<<48)       /*  Int tagging       011...1000 */
#  define TAG_STRUCT (0x7ffaUL<<48)   /*  Pointers tagging  011...1001 */
#  define TAG_CNST (0x7ffbUL<<48)     /*  Constants tagging 011...1010 */
#  define TAG_VECTOR (0x7ffcUL<<48)   /*  Vector tagging    011...1011 */
#  define TAG_CELL (0x7ffdUL<<48)     /*  Cell tagging      011...1100 */
#  define TAG_PAIR (0x7ffeUL<<48)     /*  Pair tagging      011...1101 */
#  define TAG_OBJECT (0x7fffUL<<48)   /*  Object tagging    011...1110 */
#  define TAG_NANOBJECT (0xffffUL<<48)/*  Object tagging    111...1110 */
#  define TAG_STRING (0xfff9UL<<48)   /*  Object tagging    111...1000 */
#  define TAG_SYMBOL (0xfffaUL<<48)   /*  Symbol tagging    111...1001 */
#elif( BGL_GC == BGL_SAW_GC )    
#  define TAG_QNAN 0
#  define TAG_INT 0                   /*  Integers tagging      ....00 */
#  define TAG_STRUCT 1                /*  Pointers tagging      ....01 */
#  define TAG_YOUNG 2                 /*  Pointers tagging      ....10 */
#  define TAG_CNST 3                  /*  Constants tagging     ....11 */
#elif( BGL_GC == BGL_BOEHM_GC ) 
#  define TAG_QNAN 0
#  define TAG_INT 0                   /*  Integers tagging      ....00 */
#  define TAG_STRUCT 1                /*  Pointers tagging      ....01 */
#  define TAG_CNST 2                  /*  Constants tagging     ....10 */
#  define TAG_PAIR 3                  /*  Pairs tagging         ....11 */
#elif( BGL_GC == BGL_NO_GC )
#  define TAG_QNAN 0
#  define TAG_INT 0                   /*  Integers tagging      ....00 */
#  define TAG_STRUCT 1                /*  Pointers tagging      ....01 */
#  define TAG_CNST 2                  /*  Constants tagging     ....10 */
#  define TAG_PAIR 3                  /*  Pairs tagging         ....11 */
#else
error "Unknown garbage collector type"
#endif

#if( PTR_ALIGNMENT >= 3 && BGL_GC != BGL_SAW_GC && !BGL_NAN_TAGGING)
#  define TAG_VECTOR 4                /*  Vector tagging        ...100 */
#  define TAG_CELL 5                  /*  Cells tagging         ...101 */
#  define TAG_REAL 6                  /*  Reals tagging         ...110 */
#  define TAG_STRING 7                /*  Strings tagging       ...111 */
//#  define TAG_SYMBOL 7                /*  Symbols tagging       ...111 */
#endif

#if( PTR_ALIGNMENT == 2 && defined( BGL_TAG_CNST32 ) && !BGL_NAN_TAGGING)
#  define TAG_OBJECT TAG_CNST

#  undef BGL_CNST_SHIFT_INT16
#  define BGL_CNST_SHIFT_INT16 8
#  undef BGL_CNST_SHIFT_UCS2
#  define BGL_CNST_SHIFT_UCS2 8
#endif

#if( TAG_YOUNG )
#  define POINTERP( o ) (((((long)BGL_CPTR( o )) & 1) == 0) && o)
#else
#  if( TAG_STRUCT != 0 )
#     define POINTERP( o ) ((((long)BGL_CPTR( o )) & TAG_MASK) == TAG_STRUCT)
#  else
#     define POINTERP( o ) (((((long)BGL_CPTR( o )) & TAG_MASK) == TAG_STRUCT) && BGL_CPTR( o ))
#  endif
#endif

#define BREF( r ) BGL_BPTR( (obj_t)((long)r + TAG_STRUCT) )
#define CREFSLOW( r ) BGL_CPTR( (obj_t)((unsigned long)r & ~(TAG_MASK)) )
#define CREF( r ) BGL_CPTR( (obj_t)((long)r - TAG_STRUCT) )

/*---------------------------------------------------------------------*/
/*    Allocated objects                                                */
/*    -------------------------------------------------------------    */
/*    Header managment:                                                */
/*    The header of generic allocated 32 values is as follows:         */
/*    +--------+--------+--------+--------+                            */
/*    |tttttttt|tttttsss|ssssssss|sssss???|                            */
/*    +--------+--------+--------+--------+                            */
/*      ?: the three least significant bit are ignored                 */
/*      s: 16 bit size                                                 */
/*      t: 12 bit type                                                 */   
/*---------------------------------------------------------------------*/
#define HEADER_SHIFT 3
#define SIZE_MASK ((1 << HEADER_SIZE_BIT_SIZE) - 1)
#define HEADER_SIZE_BIT_SIZE 16
#define TYPE_SHIFT (HEADER_SHIFT + HEADER_SIZE_BIT_SIZE)

#define MAKE_HEADER( _i, _sz ) \
   ((header_t)( (((long)(_i)) << TYPE_SHIFT) | ((_sz & SIZE_MASK) << HEADER_SHIFT )) )

#define HEADER_TYPE( _i ) (((long)(_i)) >> TYPE_SHIFT)

#define HEADER_SIZE( _h ) (((_h) >> HEADER_SHIFT) & SIZE_MASK)

#define TYPE( _o ) HEADER_TYPE( CREF( _o )->header )
       
#define OBJ_SIZE ((long)(sizeof( obj_t )))

#if( TAG_YOUNG )
#  define BYOUNG( r ) ((obj_t)((long)r + TAG_YOUNG))

#  define BYOUNGP( r ) ((((long)r) & TAG_MASK) == TAG_YOUNG)
#  define BOLDP( r ) ((((long)r) & TAG_MASK) == TAG_STRUCT)
#endif

#if( BGL_GC == BGL_SAW_GC )
#  define BASSIGN( field, value, obj ) (bps_bassign( &(field), value, obj), BUNSPEC)
#  define BMASSIGN( field, value ) bps_bmassign( &(field), value)
#  define BBACKPTR( field, value ) BYOUNGP( value ) ? bps_dobackptr( &(field), value ) : 0
#else
#  define BASSIGN( field, value, obj ) (((field) = (value)), BUNSPEC)
#  define BMASSIGN( field, value ) ((field) = (value))
#  define BBACKPTR( field, value )
#endif

/*---------------------------------------------------------------------*/
/*    Type identifiers ...                                             */
/*---------------------------------------------------------------------*/
#define NO_TYPE 0
#define PAIR_TYPE 1
#if( !defined( TAG_STRING ) )
#  define STRING_TYPE 2
#endif   
#define VECTOR_TYPE 3
#define PROCEDURE_TYPE 4
#define UCS2_STRING_TYPE 5
#define OPAQUE_TYPE 6
#define CUSTOM_TYPE 7
#define KEYWORD_TYPE 8
#if( !defined( TAG_SYMBOL ) )
#  define SYMBOL_TYPE 9
#endif       
#define STACK_TYPE 10
#define INPUT_PORT_TYPE 11
#define OUTPUT_PORT_TYPE 12
#define DATE_TYPE 13
#if( !defined( TAG_CELL ) )
#  define CELL_TYPE 14
#endif   
#define SOCKET_TYPE 15
#define STRUCT_TYPE 16
#if( !defined( TAG_REAL ) && !BGL_NAN_TAGGING )
#  define REAL_TYPE 17
#endif   
#define PROCESS_TYPE 18
#define FOREIGN_TYPE 19
#define OUTPUT_STRING_PORT_TYPE 20
#define BINARY_PORT_TYPE 21
#define EPAIR_TYPE 22
#define TVECTOR_TYPE 23
#define TSTRUCT_TYPE 24
#define PROCEDURE_LIGHT_TYPE 25
#define ELONG_TYPE 26
#define LLONG_TYPE 27
#define MUTEX_TYPE 28
#define CONDVAR_TYPE 29
#define MMAP_TYPE 30
#define S8VECTOR_TYPE 31
#define U8VECTOR_TYPE 32
#define S16VECTOR_TYPE 33
#define U16VECTOR_TYPE 34
#define S32VECTOR_TYPE 35
#define U32VECTOR_TYPE 36
#define S64VECTOR_TYPE 37
#define U64VECTOR_TYPE 38
#define F32VECTOR_TYPE 39
#define F64VECTOR_TYPE 40
#define WEAKPTR_TYPE 41
#define OUTPUT_PROCEDURE_PORT_TYPE 42
#define DYNAMIC_ENV_TYPE 43
#define BIGNUM_TYPE 44
#define DATAGRAM_SOCKET_TYPE 45
#define REGEXP_TYPE 46
#define CLASS_TYPE 47
#if( !defined( BGL_CNST_SHIFT_INT32 ) )
#  define INT32_TYPE 48
#  define UINT32_TYPE 49
#endif
#define INT64_TYPE 50
#define UINT64_TYPE 51
#define SEMAPHORE_TYPE 52
       
/* OBJECT must be the last defined type because new classes   */
/* will be allocated TYPE number starting at OBJECT_TYPE + 1. */
#define OBJECT_TYPE 100

#define BGL_FIND_RUNTIME_TYPE( o ) \
   BSTRING_TO_STRING( bgl_find_runtime_type( o ) )

/*---------------------------------------------------------------------*/
/*    Internal Bigloo's types.                                         */
/*---------------------------------------------------------------------*/
typedef long header_t;
typedef int bool_t;
typedef uint16_t ucs2_t;
typedef union scmobj *obj_t;

#define BGL_CPTR( _o ) ((obj_t)(_o))
#define BGL_BPTR( _o ) ((obj_t)(_o))

#include <bigloo_saw.h>
#include <bigloo_pair.h>
#include <bigloo_semaphore.h>
#include <bigloo_int.h>
#include <bigloo_real.h>
#include <bigloo_bignum.h>
#include <bigloo_vector.h>
#include <bigloo_string.h>
#include <bigloo_struct.h>
#include <bigloo_cell.h>
#include <bigloo_object.h>
#include <bigloo_exit.h>

/* stream (input and output) */
typedef union bgl_stream {                    
   void *channel;
   FILE *file;
   int fd;
} bgl_stream_t;

#define BGL_STREAM_TYPE_FD 1
#define BGL_STREAM_TYPE_FILE 2
#define BGL_STREAM_TYPE_CHANNEL 3

/* debug traces */
struct bgl_dframe {              
   obj_t name;                 
   obj_t location;
   struct bgl_dframe *link;
};

/* bigloo polymorphic type */
union scmobj {
   /* integer */
   long integer;
   
   /* common header, repeated for every type allocated object */
   header_t header;
   
   /* pairs */
   struct bgl_pair pair;
   struct bgl_epair epair;
   
   /* strings */
   struct bgl_string string;

   /* ucs2/utf16 strings */
   struct bgl_ucs2_string ucs2_string; 

   /* vectors */
   struct bgl_vector vector;
   struct bgl_tvector tvector;
   struct bgl_hvector hvector;

   /* procedure (closures) */
   struct procedure {
      header_t header;    
      union scmobj *(*entry)();
      union scmobj *(*va_entry)();
      union scmobj *attr;
      int arity;
      union scmobj *obj0;
   } procedure;

   /* light procedures (results of the CFA optimization) */
   struct procedure_light {
      union scmobj *(*entry)();
      union scmobj  *obj0;
   } procedure_light;

   /* symbols and keywords */
   struct symbol {
#if( !defined( TAG_SYMBOL ) )
      header_t header;
#endif      
      union scmobj *string;
      union scmobj *cval;
   } symbol;

   struct keyword {
      header_t header;
      union scmobj *string;
      union scmobj *cval;
   } keyword;
   
   /* common ports structure (output, input, procedure, gzip) */
   struct port {
      header_t header;
      /* kindof = console | file | pipe | string */ 
      union scmobj *kindof;
      /* port name */
      union scmobj *name;
      /* OS stream descriptor */
      bgl_stream_t stream;
       /* an optional close hook (procedure) */
      union scmobj *chook;
      /* an optional timeout structure */
      void *timeout;
      /* a user date (see SSL sockets) */
      void *userdata;
      /* OS close primitive */
      int (*sysclose)();
   } port;

   /* output ports */
   struct output_port {
      /* the common port */
      struct port port;
      /* streamtype = FD | FILE | CHANNEL */
      int stream_type;
      /* the buffer (bgl_string) */
      union scmobj *buf;
      /* next char pos */
      char *ptr;
      /* end of buffer */
      char *end;
      /* buffering mode */
      int bufmode;
      /* OS write primitive */
      ssize_t (*syswrite)();
      /* OS flush primitive */
      union scmobj *(*sysflush)();
      /* OS seek primitive */
      long (*sysseek)();
      /* flush use hook */
      union scmobj *fhook;
      /* flush buffer */
      union scmobj *flushbuf;
      /* last port error */
      long err;
      /* mutex to prevent multiwrite */
      union scmobj *mutex;
   } output_port;

   /* input ports */
   struct input_port {
      /* common port */
      struct port port;
      /* position in file */
      long filepos;
      /* RGC fill barrier */
      long fillbarrier;
      /* OS read primitive */
      long (*sysread)();
      /* OS seek primitive */
      void (*sysseek)();
      /* optional use seek function */
      union scmobj *userseek;
      /* EOF reached */
      bool_t eof;
      /* RGC start of match */
      long matchstart;
      /* RGC end of match */
      long matchstop;
      /* RGC read position */
      long forward;
      /* RGC read offset */
      long bufpos;
      /* buffer (a bgl_string) */
      union scmobj *buf;
      /* the last read char before fill */
      int lastchar;
      /* the number of chars in that port */
      long length;
   } input_port;

   /* string input ports */
   struct input_string_port {
      /* common port */
      struct input_port iport;
      /* offset */
      long offset;
   } input_string_port;
	 
   /* procedure input ports */
   struct input_procedure_port {
      /* common port */
      struct input_port iport;
      /* buffer used to store procedure results */
      union scmobj *pbuffer;
      /* index in pbuffer */
      long pbufpos;
      /* the procedure */
      union scmobj *proc;
   } input_procedure_port;
   
   /* gzipped input ports */
   struct input_gzip_port {
      /* common port */
      struct input_procedure_port iport;
      /* gzipped port */
      union scmobj *gzip;
   } input_gzip_port;

   /* binary ports */
   struct binary_port {
      header_t header;
      /* file name */
      union scmobj *name;
      /* OS file */
      FILE *file;
      /* type 0=input, 1=output, 2=close */
      int io;
   } binary_port;
   
   /* cells (compiler and user values) */	
   struct bgl_cell cell;

   /* structures */
   struct bgl_struct structure;

   /* floating point numbers */
#if( !BGL_NAN_TAGGING )
   struct bgl_real real;
#endif   

   /* call/cc stack */
   struct stack {
      header_t header;
      /* self reference pointer */
      union scmobj *self;
      /* exitd_top chained list head */
      struct exitd *exitd_top;
      /* extd stamp */
      union scmobj *stamp;
      /* stack size */
      long size;
      /* befored pointer */
      struct befored *before_top;
      /* top of stack */
      char *stack_top;
      /* bottom of stack */
      char *stack_bot;
      /* heap allocated copy of the stack */
      void *stack;
   } stack;
   
   /* boxed foreign values */
   struct foreign {
      header_t header;    
      union scmobj *id;
      void *cobj;
   } foreign;
   
   /* exact longs (i.e., boxed C longs) */
   struct elong {
      header_t header;
      long val;
   } elong;

   /* long longs */
   struct llong {
      header_t header;
      BGL_LONGLONG_T val;
   } llong;

#if( !defined( BGL_CNST_SHIFT_INT32 ) )
   /* sint32 */
   struct bgl_sint32 {
      header_t header;
      int32_t val;
   } sint32;
      
   /* uint32 */
   struct bgl_uint32 {
      header_t header;
      uint32_t val;
   } uint32;
#endif
   
   /* sint64 */
   struct bgl_sint64 {
      header_t header;
      int64_t val;
   } sint64;
      
   /* uint64 */
   struct bgl_uint64 {
      header_t header;
      uint64_t val;
   } uint64;
   
   /* arbitrary precision integers */
   struct bgl_bignum bignum;

   /* processes */
   struct process { 
      header_t header;
      /* OS pid */
      int pid;
      /* index in the proc_table */
      int index;
      /* process' ports (in, out, err) */
      union scmobj *stream[ 3 ];
      /* completion mark */
      int exited;
      /* exit status */
      int exit_status;
#if defined( _MSC_VER ) || defined( _MINGW_VER )
      /* win32 process handle */
      void *hProcess;
#endif
   } process;

   /* TCP sockets */
   struct socket {
      header_t header;
      /* OS port number */
      int portnum;
      /* host name */
      union scmobj *hostname;
      /* host ip */
      union scmobj *hostip;
      /* socket adress family */
      sa_family_t family;
      /* the socket host address */
      union {
	 struct in_addr in_addr;
	 struct in6_addr in6_addr;
      } address;
      /* OS file descriptor */
      int fd;
      /* bigloo input port (or #unspec) */
      union scmobj *input;
      /* bigloo output port (or #unspec) */
      union scmobj *output;
      /* socket type (client/server) */
      int stype;
      /* close hook */
      union scmobj *chook;
      /* the server accept procedure */
      union scmobj *(*accept)();
      /* user data */
      void *userdata; 
   } socket;

   /* UDP sockets */
   struct bgl_datagram_socket {
      header_t header;
      /* OS port number */
      int portnum;
      /* host name */
      union scmobj *hostname;
      /* host ip */
      union scmobj *hostip;
      /* the socket host address */
      union {
	 struct in_addr in_addr;
	 struct in6_addr in6_addr;
      } address;
      /* socket adress family */
      sa_family_t family;
      /* OS file descriptor */
      int fd;
      /*  socket type (client/server) */
      int stype;
      /* the close hook */
      union scmobj *chook;
      /* socket server */
      void *server;
      /* assocated port */
      union scmobj *port;
   } datagram_socket;

   /* regular expressions */
   struct bgl_regexp {
      header_t header;
      /* string source */
      union scmobj *pat;
      /* the native regular expression */
      void *preg;
      /* native matching routines */
      union scmobj *(*match)();
      long (*match_n)();
      union scmobj *(*free)();
#  if( BGL_REGEXP_TYPE == BGL_REGEXP_pcre )
      /* pcre regular expression */
      void *study;
#  endif
      int capturecount;
   } regexp;

   /* custom objects */
   struct custom {
      header_t header;
      /* a C name */
      char *identifier;
      /* finalization function */
      int (*final)();
      /* equality test */
      int (*equal)();
      /* hashing function */
      long (*hash)();
      /* to_string converter */
      char *(*to_string)();
      /* output function */
      union scmobj *(*output)();
   } custom;

   /* dates */
   struct bgl_date {
      header_t header;
      /* the UTC date in seconds */
      time_t time;
      /* the tm structure */
      struct tm tm;
      /* number of nano seconds */
      BGL_LONGLONG_T nsec;
      /* number of seconds of timezone */
#if( !BGL_HAVE_GMTOFF )
      long timezone;
#endif
   } date;

   /* mutexes */
   struct bgl_mutex {
      header_t header;
      /* a name (mainly for debug) */
      union scmobj *name;
      /* OS lock primitive */
      int (*syslock)();
      /* OS try lock primitive */
      int (*systrylock)();
      /* OS timed lock primitive */
      int (*systimedlock)();
      /* OS unlock primitive */
      int (*sysunlock)();
      /* prelock function */
      int (*syslockprelock)();
      union scmobj *(*sysstate)();
      /* the mutex backend (underlying mutex platform) */
      union scmobj *backend;
      /* actual OS mutex */
      void *sysmutex;
   } mutex;

   /* condition variables */
   struct bgl_condvar {
      header_t header;
      /* a name (mainly for debug) */
      union scmobj *name;
      /* OS wait primitive */
      int (*syswait)();
      /* OS timed wait primitive */
      int (*systimedwait)();
      /* OS signal primitive */
      int (*syssignal)();
      /* OS broadcast primitive */
      int (*sysbroadcast)();
      /* actual OS condition variable */
      void *condvar;
   } condvar;

   /* memory mapped IOs */
   struct bgl_mmap {
      header_t header;
      /* the file name */
      union scmobj *name;
      /* OS file descriptor */
      int fd;
      /* length of the mapped file */
      long length;
      /* read position */
      long rp;
      /* write position */
      long wp;
      /* OS mmap */
      unsigned char *map;
#if !BGL_HAVE_MMAP
      /* alternate file descriptor */
      int afd;
      /* alternate read position */ 
      long ar;
      /* alternate write position */ 
      long aw;
#endif
   } mmap;
   
   /* weak pointers */
   struct bgl_weakptr {
      header_t header;
      union scmobj *data;
   } weakptr;

   /* classes */
   struct bgl_class {
      header_t header;
      /* class name */
      union scmobj *name;
      /* allocation functions */
      union scmobj *alloc_fun;
      union scmobj *new_fun;
      /* class hash number */
      long hash;
      /* nil instance */
      union scmobj *nil_fun;
      union scmobj *nil;
      /* user class constructor */
      union scmobj *constructor;
      /* the class virtual getters and setters */
      union scmobj *virtual_fields;
      union scmobj *shrink;
      /* class fields */
      union scmobj *direct_fields;
      union scmobj *all_fields;
      /* the module defining the class */
      union scmobj *module;
      /* a unique index number */
      long index;
      /* depth in the inheritance tree */
      long depth;
      /* eval private data */
      union scmobj *evdata;
      /* the class inheritance */
      union scmobj *super;
      union scmobj *subclasses;
      union scmobj *ancestor0;
   } class;

   /* thread dynamic environment */
   struct bgl_dynamic_env {
      header_t header;
      /* global IO ports */
      union scmobj *current_output_port;
      union scmobj *current_input_port;
      union scmobj *current_error_port;
      /* global display */
      union scmobj *current_display;
      /* multiple values */
      int mvalues_number;
      union scmobj *mvalues[ 16 ];
      /* exceptions and call/cc */
      char *stack_bottom;
      union scmobj *exit_value;
      struct exitd *exitd_top;
      struct exitd *exitd_bottom;
      union scmobj *exitd_stamp;
      struct befored *befored_top;
      union scmobj *exitd_val;
      /* error handling */
      union scmobj *error_handler;
      union scmobj *error_notifiers;
      union scmobj *uncaught_exception_handler;
      /* interrupt notification */
      union scmobj *interrupt_notifier;
      /* the debug information */
      union scmobj *debug_alist;
      /* stack traces */
      struct bgl_dframe top;
      struct bgl_dframe *top_of_frame;
      union scmobj *exit_traces;
      /* current thread */
      void *current_thread;
      /* thread lexical stack */
      union scmobj *lexical_stack;
      /* eval parameter */
      union scmobj *evstate;
      union scmobj *module;
      union scmobj *abase;
      /* parameters list */
      union scmobj *parameters;
      /* signal handlers */
      union scmobj *sig_handlers[ 32 ];
      /* thread backend */
      union scmobj *thread_backend;
      /* saw specific */
#if( BGL_SAW == 1 ) 
      bgl_saw_frame_header_t *saw_sp;
#endif      
      /* user per thread data */
      union scmobj *user_data;
   } dynamic_env;

   /* semaphores */
   struct bgl_semaphore semaphore;
};

/* function type */
typedef obj_t (*function_t)();

/*---------------------------------------------------------------------*/
/*    The garbage collector                                            */
/*---------------------------------------------------------------------*/
#include <bigloo_gc.h>       

/*---------------------------------------------------------------------*/
/*    Constants                                                        */
/*---------------------------------------------------------------------*/
#if( TAG_CNST != 0 )
#  define BGL_TAG_CNSTP( o ) ((((unsigned long)o) & TAG_MASK) == TAG_CNST)
#else
#  define BGL_TAG_CNSTP( o ) ((o) && ((((long)o) & TAG_MASK) == TAG_CNST))
#endif

#if( BGL_NAN_TAGGING ) /* BGL_NAN_TAGGING */
#  define BGL_TAG_BCNST( c ) (obj_t)TAG( c << 32, TAG_SHIFT, TAG_CNST )
#  define BGL_TAG_CCNST( c ) (long)UNTAG( (unsigned long)c >> 32, TAG_SHIFT, TAG_CNST )
#else  /* !BGL_NAN_TAGGING */
#  define BGL_TAG_BCNST( c ) (obj_t)TAG( c, TAG_SHIFT, TAG_CNST )
#  define BGL_TAG_CCNST( c ) (long)UNTAG( c, TAG_SHIFT, TAG_CNST )
#endif  /* BGL_NAN_TAGGING */

#if( defined( BGL_TAG_CNST32 ) ) /* BGL_TAG_CNST32 */
#  define CNSTP( o ) \
    (BGL_TAG_CNSTP( o ) && ((((unsigned long)o) >> 24) == 0xff))
#  define CNST32P( o ) \
    ((((unsigned long)o) >> 24) == 0xff)
#  define BCNST( o ) \
    ((obj_t)(((unsigned long)BGL_TAG_BCNST( o )) + (0xffUL << 24)))
#  define CCNST( o ) \
    ((long)(((unsigned long)BGL_TAG_CCNST( o )) & 0xffff))
#  define CCNST_MASK( o ) \
    (o & 0xffff)
#else  /* !BGL_TAG_CNST32 */
#  define CNSTP( o ) BGL_TAG_CNSTP( o )
#  define CNST32P( o ) 1
#  define BCNST( o ) BGL_TAG_BCNST( o )
#  define CCNST( o ) BGL_TAG_CCNST( o )
#  define CCNST_MASK( o ) (o)
#endif  /* BGL_TAG_CNST32 */

/* arrange nil and unspec to have a shared unique lower bit pattern */
/* this will enable the JavaScript runtime to test with a single    */
/* bitop NIL and UNSPEC (as needed when compiling x==null)          */
/* practically NIL and UNSPEC are the only two ODD constants.       */
#define BNIL BCNST( 1L )
#define BUNSPEC BCNST( 3L )
   
#define BFALSE BCNST( 2L )
#define BTRUE BCNST( 4L )
   
#define BCHARH ((unsigned long)BCNST( 6L ))
#define BUCS2H ((unsigned long)BCNST( 8L ))

#define BINT8H ((unsigned long)BCNST( 10L ))
#define BUINT8H ((unsigned long)BCNST( 12L ))

#define BINT16H ((unsigned long)BCNST( 14L ))
#define BUINT16H ((unsigned long)BCNST( 16L ))

#if( PTR_ALIGNMENT >= 3 )
#  define BINT32H ((unsigned long)BCNST( 18L ))
#  define BUINT32H ((unsigned long)BCNST( 20L ))
#endif

#define BEOF BCNST( 22L )
#define BEOA BCNST( 24L )

#define BOPTIONAL BCNST( 26L )
#define BREST BCNST( 28L )
#define BKEY BCNST( 30L )

#if( !BGL_NAN_TAGGING )
#  define BGL_NULL_OR_UNSPECIFIEDP( obj ) \
   ((((long)(obj)) & ((TAG_MASK << 1) + 1)) == (long)BNIL)
#else
#  define BGL_NULL_OR_UNSPECIFIEDP( obj ) \
   ((obj) == BNIL || ((obj) == BUNSPEC))
#endif

/*---------------------------------------------------------------------*/
/*    Booleans                                                         */
/*---------------------------------------------------------------------*/
#define BOOLEANP( o ) (((long)o == (long)BTRUE) || ((long)o == (long)BFALSE))

#define BBOOL( i ) (i ? BTRUE : BFALSE)
#define CBOOL( o ) (o != BFALSE)

#define NOT( o ) (!o)   

#define TRUEP( c ) ((bool_t)(c != BFALSE))

/*---------------------------------------------------------------------*/
/*    Characters (bytes)                                               */
/*---------------------------------------------------------------------*/
#define CHARP( o ) \
   BGL_CNSTP( o, BCHARH, BGL_CNST_SHIFT_CHAR )
#define BCHAR( c ) \
   BGL_CNST_TO_BCNST( c, 0xffL, BCHARH, BGL_CNST_SHIFT_CHAR, unsigned char )
#define CCHAR( o ) \
   BGL_BCNST_TO_CNST( o, 0xffL, BGL_CNST_SHIFT_CHAR, unsigned char )

/*---------------------------------------------------------------------*/
/*    UCS2/UTF16 characters                                            */
/*---------------------------------------------------------------------*/
#define UCS2P( o ) \
   BGL_CNSTP( o, BUCS2H, BGL_CNST_SHIFT_UCS2 )
#define BUCS2( u ) \
   BGL_CNST_TO_BCNST( u, 0xffffL, BUCS2H, BGL_CNST_SHIFT_UCS2, int )
#define CUCS2( o ) \
   BGL_BCNST_TO_CNST( o, 0xffffL, BGL_CNST_SHIFT_UCS2, int )

#define BGL_INT_TO_UCS2( _i ) ((ucs2_t)(_i))

/*---------------------------------------------------------------------*/
/*    Regular procedures                                               */
/*---------------------------------------------------------------------*/
#define DEFINE_EXPORT_BGL_PROCEDURE( n, na, p, vp, at, nb_args ) \
   static struct { __CNST_ALIGN header_t header; \
                   obj_t (*entry)(); \
                   obj_t (*va_entry)(); \
                   obj_t attr; \
                   int arity; } \
      na = { __CNST_FILLER MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
	     (obj_t (*)())p, \
	     (obj_t (*)())vp, \
             at, \
	     nb_args }; \
      BGL_EXPORTED_DEF const obj_t n = BREF( &(na.header) )

#define DEFINE_STATIC_BGL_PROCEDURE( n, na, p, vp, at, nb_args ) \
   static struct { __CNST_ALIGN header_t header; \
                   obj_t (*entry)(); \
                   obj_t (*va_entry)(); \
                   obj_t attr; \
                   int arity; } \
      na = { __CNST_FILLER MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
             (obj_t (*)())p, \
	     (obj_t (*)())vp, \
             at, \
	     nb_args }; \
      static const obj_t n = BREF( &(na.header) )

#define PROCEDURE_SIZE (sizeof( struct procedure ))

#define PROCEDURE( o ) CREF( o )->procedure
   
#define PROCEDURE_ENTRY( fun ) (obj_t)(PROCEDURE( fun ).entry)
#define PROCEDURE_VA_ENTRY( fun ) (obj_t)(PROCEDURE( fun ).va_entry)

#define PROCEDUREP( fun ) \
   (POINTERP( fun ) && (TYPE( fun ) == PROCEDURE_TYPE))

#define PROCEDURE_ARITY( fun ) (PROCEDURE( fun ).arity)
#define PROCEDURE_LENGTH( fun ) (HEADER_SIZE( CREF( fun )->header ))
   
#define PROCEDURE_ATTR( fun ) (PROCEDURE( fun ).attr)
#define PROCEDURE_ATTR_SET( fun, _v ) (BASSIGN( PROCEDURE( fun ).attr, (_v), fun ), (_v))

#define VA_PROCEDUREP( fun ) \
   ( PROCEDURE_ARITY( fun ) < 0 )
#define OPT_PROCEDUREP( fun ) \
   ( PROCEDURE_ATTR( fun ) == BFALSE )
   
#define PROCEDURE_CORRECT_ARITYP( fun, num )           \
        ( (PROCEDURE_ARITY( fun ) == num) ||           \
	  (VA_PROCEDUREP( fun ) &&                     \
	   ((-num - 1) <= (PROCEDURE_ARITY( fun )))) )
		  
#define PROCEDURE_ENV( p ) (&(PROCEDURE( p ).obj0))

#define PROCEDURE_REF( p, i )    (PROCEDURE_ENV( p ))[ i ]
#define PROCEDURE_SET( p, i, o ) BASSIGN( PROCEDURE_REF( p, i ), o, p )

#define MAKE_FX_PROCEDURE( entry, arity, size ) \
   make_fx_procedure( (function_t)entry, arity, size )

#define MAKE_VA_PROCEDURE( entry, arity, size ) \
   make_va_procedure( (function_t)entry, arity, size )

#define BGL_MAKE_FX_PROCEDURE_STACK( tmp, entry, arity, size ) \
   bgl_init_fx_procedure( (obj_t)(&tmp), entry, arity, size )

#define BGL_PROCEDURE_BYTE_SIZE( size ) \
   (PROCEDURE_SIZE + ((size-1) * OBJ_SIZE))
	 
#define BGL_ALLOC_STACK_FX_PROCEDURE( size ) \
   char[ PROCEDURE_SIZE + ((size-1) * OBJ_SIZE) ]
	 
/*---------------------------------------------------------------------*/
/*    Light procedures                                                 */
/*---------------------------------------------------------------------*/
#define DEFINE_BGL_L_PROCEDURE( n, na, e ) \
   static const struct { __CNST_ALIGN ; \
                   union scmobj *(*entry)(); } \
      na = { __CNST_FILLER (obj_t (*)())e }; \
      static const obj_t n = BLIGHT( &(na.entry) )
   
#define BLIGHT( l ) BPAIR( l )
#define CLIGHT( l ) CPAIR( l )

#define PROCEDURE_L_SIZE (sizeof( struct procedure_light ))

#define PROCEDURE_L( _o_ ) (CLIGHT( _o_ )->procedure_light)

#define PROCEDURE_L_ENTRY( fun ) (PROCEDURE_L( fun ).entry)

#define PROCEDURE_L_ENV( fun ) (&(PROCEDURE_L( fun ).obj0))

#define PROCEDURE_L_REF( p, _i )    (PROCEDURE_L_ENV( p )[ _i ])
#define PROCEDURE_L_SET( p, _i, o ) BASSIGN( PROCEDURE_L_REF( p, _i ), o, p )

#if( defined( __GNUC__ ) )
#  define MAKE_L_PROCEDURE_ALLOC( ALLOC, _entry, _size ) \
      ( { obj_t an_object; \
	  an_object = ALLOC( PROCEDURE_L_SIZE + ((_size-1) * OBJ_SIZE) ); \
	  (an_object->procedure_light).entry = _entry; \
          ( BLIGHT( an_object ) ); } )
#else
#  define MAKE_L_PROCEDURE_ALLOC( ALLOC, _entry, _size ) \
      (   an_object = ALLOC( PROCEDURE_L_SIZE + ((_size-1) * OBJ_SIZE) ), \
	  (an_object->procedure_light).entry = _entry, \
          ( BLIGHT( an_object ) ) )
#endif

#define MAKE_L_PROCEDURE( _entry, _size )   \
   MAKE_L_PROCEDURE_ALLOC( GC_MALLOC, ((obj_t (*)())_entry), _size )

/*---------------------------------------------------------------------*/
/*    Extra-light procedures                                           */
/*---------------------------------------------------------------------*/
#define MAKE_EL_PROCEDURE( size ) \
   (( !size ) ? BUNSPEC : GC_MALLOC( size * OBJ_SIZE ))

#define PROCEDURE_EL_REF( p, i ) (((obj_t *)p)[ i ])
#define PROCEDURE_EL_SET( p, i, o ) BASSIGN( PROCEDURE_EL_REF( p, i ), o, p )

/*---------------------------------------------------------------------*/
/*    Generic functions                                                */
/*---------------------------------------------------------------------*/
#define DEFINE_EXPORT_BGL_GENERIC( n, na, p, vp, at, nb_args ) \
   static struct { __CNST_ALIGN header_t header; \
                   obj_t (*entry)(); \
                   obj_t (*va_entry)(); \
                   obj_t attr; \
                   int arity; \
		   obj_t env0; \
		   obj_t env1; \
		   obj_t env2; } \
      na = { __CNST_FILLER MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
	     (obj_t (*)())p, \
	     (obj_t (*)())vp, \
             at, \
	     nb_args, \
	     BFALSE, \
	     BFALSE, \
	     BUNSPEC }; \
      BGL_EXPORTED_DEF const obj_t n = BREF( &(na.header) )

#define DEFINE_STATIC_BGL_GENERIC( n, na, p, vp, at, nb_args ) \
   static struct { __CNST_ALIGN header_t header; \
                   obj_t (*entry)(); \
                   obj_t (*va_entry)(); \
                   obj_t attr; \
                   int arity; \
		   obj_t env0; \
		   obj_t env1; \
		   obj_t env2; } \
      na = { __CNST_FILLER MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
             (obj_t (*)())p, \
	     (obj_t (*)())vp, \
             at, \
	     nb_args, \
	     BFALSE, \
	     BFALSE, \
	     BUNSPEC }; \
      static const obj_t n = BREF( &(na.header) )

/*---------------------------------------------------------------------*/
/*    Symbols                                                          */
/*---------------------------------------------------------------------*/
#if( defined( TAG_SYMBOL ) )
#  define SYMBOLP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_SYMBOL)))
#  define BSYMBOL( p ) ((obj_t)((long)p + TAG_SYMBOL))
#  define CSYMBOL( p ) ((obj_t)((long)p - TAG_SYMBOL))
#else   
#  define SYMBOLP( o ) (POINTERP( o ) && (TYPE( o ) == SYMBOL_TYPE))
#  define BSYMBOL( p ) BREF( p )
#  define CSYMBOL( p ) CREF( p )
#endif   

#define SYMBOL( o ) (CSYMBOL( o )->symbol)
   
#define SYMBOL_SIZE (sizeof( struct symbol ))

#define SYMBOL_TO_STRING( o ) \
   (SYMBOL( o ).string ? SYMBOL( o ).string : bgl_symbol_genname( o, "g" ) )

#define GET_SYMBOL_PLIST( o ) (SYMBOL( o ).cval)

#define SET_SYMBOL_PLIST( o, v ) BASSIGN( GET_SYMBOL_PLIST( o ), v, o )

/*---------------------------------------------------------------------*/
/*    Keywords                                                         */
/*---------------------------------------------------------------------*/
#define KEYWORDP( o ) (POINTERP( o ) && (TYPE( o ) == KEYWORD_TYPE))

#define KEYWORD( o ) (CREF( o )->keyword)
   
#define KEYWORD_SIZE (sizeof( struct keyword ))

#define KEYWORD_TO_STRING( o ) KEYWORD( o ).string

#define GET_KEYWORD_PLIST( o ) (KEYWORD( o ).cval)

#define SET_KEYWORD_PLIST( o, v ) BASSIGN( GET_KEYWORD_PLIST( o ), v, o )

/*---------------------------------------------------------------------*/
/*    Array bound checking                                             */
/*---------------------------------------------------------------------*/
#if( TAG_SHIFT <= LONG_MAX )
#  define BOUND_CHECK( o, v ) ((unsigned long)o < (unsigned long)v)
#else
#  define BOUND_CHECK( o, v ) (((long)o >= 0) && ((long)o < (long)v))
#endif

/*---------------------------------------------------------------------*/
/*    Ports                                                            */
/*---------------------------------------------------------------------*/
#define PORT( o ) CREF( o )->port
   
#define PORT_CHOOK( o ) (PORT( o ).chook)
#define PORT_CHOOK_SET( o, v ) BASSIGN( PORT( o ).chook, (v), o )
   
#define PORT_FILE( o ) (PORT( o ).stream.file)
#define PORT_FD( o ) (PORT( o ).stream.fd)
#define PORT_CHANNEL( o ) (PORT( o ).stream.channel)

/*---------------------------------------------------------------------*/
/*    Output-ports                                                     */
/*---------------------------------------------------------------------*/
#define OUTPUT_PORT_SIZE (sizeof( struct output_port ))

#define OUTPUT_PORT( o ) (CREF( o )->output_port)

#define OUTPUT_PORTP( o ) \
   (POINTERP( o ) && (TYPE( o ) == OUTPUT_PORT_TYPE))

#define BGL_OUTPUT_STRING_PORTP( o ) \
   (OUTPUT_PORTP( o ) && (PORT( o ).kindof == KINDOF_STRING))
   
#define BGL_OUTPUT_PROCEDURE_PORTP( o ) \
   (OUTPUT_PORTP( o ) && (PORT( o ).kindof == KINDOF_PROCEDURE))
   
#define OUTPUT_PORT_TO_FILE( o ) \
   (PORT_FILE( o ))

#define FILE_TO_OUTPUT_PORT( f )		\
   (bgl_file_to_output_port( f, make_string_sans_fill( 8192 ) ))

#define BGL_INT_EOFP( i ) \
   ((i) == EOF)

#define BGL_OUTPUT_PORT_CNT( _p ) \
   (OUTPUT_PORT( _p ).end - OUTPUT_PORT( _p ).ptr)
   
#define BGL_OUTPUT_PORT_FILEPOS( _p ) \
   (bgl_output_port_filepos( _p ))
   
#define BGL_OUTPUT_PORT_FILEPOS_SET( _p, _i ) \
   (bgl_output_port_seek( _p, _i ))

#define BGL_OUTPUT_PORT_FHOOK( o ) (OUTPUT_PORT( o ).fhook)
#define BGL_OUTPUT_PORT_FHOOK_SET( o, v ) BASSIGN( OUTPUT_PORT( o ).fhook, v, o )
   
#define BGL_OUTPUT_PORT_FLUSHBUF( o ) (OUTPUT_PORT( o ).flushbuf)
#define BGL_OUTPUT_PORT_FLUSHBUF_SET( o, v ) BASSIGN( OUTPUT_PORT( o ).flushbuf, v, o )
   
#define BGL_OUTPUT_PORT_BUFFER( o ) \
   (OUTPUT_PORT( o ).buf)
   
/* output ports buffering mode */
#define BGL_IONB  0 /* unubuffered */
#define BGL_IOLBF 1 /* line buffered */
#define BGL_IOFBF 2 /* fully buffered */
#define BGL_IOEBF 3 /* extensible buffered */

/*---------------------------------------------------------------------*/
/*    Les OUTPUT_STRING_PORTs                                          */
/*---------------------------------------------------------------------*/
#define OUTPUT_STRING_PORT_SIZE (sizeof( struct output_string_port ))

#define OUTPUT_STRING_PORT( o ) CREF( o )->output_string_port

#define OUTPUT_STRING_PORT_BUFFER_SIZE 128

#define END_OF_STRING_PORTP( o ) \
   ( OUTPUT_STRING_PORT( o ).offset == OUTPUT_STRING_PORT( o ).size )

/*---------------------------------------------------------------------*/
/*    OUTPUT_PROCEDURE_PORT                                            */
/*---------------------------------------------------------------------*/
#define PORT_ON_PROCEDUREP( o ) ( TYPE( o ) == OUTPUT_PROCEDURE_PORT_TYPE )

#define OUTPUT_PROCEDURE_PORT_SIZE (sizeof( struct output_procedure_port ))

#define OUTPUT_PROCEDURE_PORT( o ) CREF( o )->output_procedure_port

/*---------------------------------------------------------------------*/
/*    RGC ports                                                        */
/*---------------------------------------------------------------------*/
#define KINDOF_FILE      BINT( 1 )
#define KINDOF_CONSOLE   BINT( 2 )
#define KINDOF_SOCKET    BINT( 3 )
#define KINDOF_PROCPIPE  BINT( 4 )
#define KINDOF_PIPE      BINT( 5 )
#define KINDOF_VIRTUAL   BINT( 6 )
#define KINDOF_STRING    BINT( 7 )
#define KINDOF_CLOSED    BINT( 8 )
#define KINDOF_PROCEDURE BINT( 9 )
#define KINDOF_GZIP      BINT( 10 )
#define KINDOF_BZIP2     BINT( 11 )
#define KINDOF_TIMEOUT   BINT( 12 )
#define KINDOF_DATAGRAM  BINT( 13 )
#define KINDOF_UNUSED    BINT( 20 )

#define PORT_IS_OS( o ) (PORT( o ).kindof < KINDOF_VIRTUAL)
#define PORT_IS_VIRTUAL( o ) (PORT( o ).kindof > KINDOF_VIRTUAL)
   
#define INPUT_PORT_SIZE (sizeof( struct input_port ))
#define INPUT_PROCEDURE_PORT_SIZE (sizeof( struct input_procedure_port ))
#define INPUT_GZIP_PORT_SIZE (sizeof( struct input_gzip_port ))
#define INPUT_STRING_PORT_SIZE (sizeof( struct input_string_port ))

#define INPUT_GZIP_PORT( o ) CREF( o )->input_gzip_port
#define INPUT_PROCEDURE_PORT( o ) CREF( o )->input_procedure_port

#define INPUT_PORT( o ) CREF( o )->input_port

#define INPUT_PORTP( o ) (POINTERP( o ) && (TYPE( o ) == INPUT_PORT_TYPE))

#define INPUT_STRING_PORTP( o ) (INPUT_PORTP(o) && INPUT_PORT_ON_STRINGP(o))
#define INPUT_PROCEDURE_PORTP( o ) (INPUT_PORTP(o) && INPUT_PORT_ON_PROCP(o))
#define INPUT_GZIP_PORTP( o ) (INPUT_PORTP(o) && INPUT_PORT_ON_GZIPP(o))

#define EOF_OBJECTP( o ) (o == BEOF)

#define OUTPUT_PORT_NAME( o ) (PORT( o ).name)
#define OUTPUT_PORT_NAME_SET( o, v ) (PORT( o ).name = v)
#define INPUT_PORT_NAME( o ) (PORT( o ).name)
#define INPUT_PORT_NAME_SET( o, v ) (PORT( o ).name = v)

#define INPUT_PORT_FILEPOS( o ) (INPUT_PORT( o ).filepos)

#define INPUT_PORT_TOKENPOS( o ) \
    (INPUT_PORT_FILEPOS( o ) - RGC_BUFFER_MATCH_LENGTH( o ))

#define INPUT_PORT_ON_FILEP( o ) (PORT( o ).kindof == KINDOF_FILE)

#define INPUT_PORT_ON_STRINGP( o ) (PORT( o ).kindof == KINDOF_STRING)
#define INPUT_PORT_ON_PROCP( o ) (PORT( o ).kindof == KINDOF_PROCEDURE)
#define INPUT_PORT_ON_GZIPP( o ) (PORT( o ).kindof == KINDOF_GZIP)
#define INPUT_PORT_ON_SOCKETP( o ) (PORT( o ).kindof == KINDOF_SOCKET)

#define BGL_INPUT_GZIP_PORT_INPUT_PORT( o ) ((obj_t)(INPUT_GZIP_PORT( o ).gzip))
   
#define INPUT_PORT_FILLBARRIER( o ) \
   (INPUT_PORT( o ).fillbarrier)
#define INPUT_PORT_FILLBARRIER_SET( o, v ) \
   (INPUT_PORT_FILLBARRIER( o ) = (v - RGC_BUFFER_AVAILABLE( o )))

#define INPUT_PORT_CLOSEP( o ) \
    (PORT( o ).kindof == KINDOF_CLOSED)

#define OUTPUT_PORT_CLOSEP( o ) \
    (PORT( o ).kindof == KINDOF_CLOSED)

#define BGL_INPUT_PORT_BUFFER( o ) \
   (INPUT_PORT( o ).buf)
   
#define BGL_INPUT_PORT_BUFSIZ( o ) \
   (STRING_LENGTH( BGL_INPUT_PORT_BUFFER( o ) ))

#define BGL_INPUT_PORT_LENGTH( o ) \
   (INPUT_PORT( o ).length)

#define BGL_INPUT_PORT_LENGTH_SET( o, v ) \
   (INPUT_PORT( o ).length = (v))

#define BGL_INPUT_PORT_USEEK( o ) \
   INPUT_PORT( o ).userseek
   
#define BGL_INPUT_PORT_USEEK_SET( o, p ) \
   BASSIGN( INPUT_PORT( o ).userseek, p, o )
   
/*---------------------------------------------------------------------*/
/*    Binary ports                                                     */
/*---------------------------------------------------------------------*/
#define BINARY_PORT_SIZE (sizeof( struct binary_port ))

#define BINARY_PORT( o ) CREF( o )->binary_port

#define BINARY_PORTP( o ) \
   ( POINTERP( o ) && (TYPE( o ) == BINARY_PORT_TYPE) )

#define BINARY_PORT_IN   ((bool_t)0)
#define BINARY_PORT_OUT  ((bool_t)1)

#define BINARY_PORT_INP( p ) (BINARY_PORT( o ).io == BINARY_PORT_IN)

#define BINARY_PORT_TO_FILE( p ) (BINARY_PORT( p ).file)
   
#define BGL_OUTPUT_CHAR( p, c ) \
   (fputc( (c), BINARY_PORT( (p) ).file ), BUNSPEC)

#define BGL_INPUT_CHAR( p ) \
   (fgetc( BINARY_PORT( (p) ).file))

/*---------------------------------------------------------------------*/
/*    MMAP                                                             */
/*---------------------------------------------------------------------*/
#define BGL_MMAP_SIZE (sizeof( struct bgl_mmap ))

#define BGL_MMAP( o ) CREF( o )->mmap

#define BGL_MMAPP( o ) (POINTERP( o ) && (TYPE( o ) == MMAP_TYPE))

#define BGL_MMAP_TO_STRING( s ) ((char *)(BGL_MMAP( s ).map))
   
#define BGL_MMAP_LENGTH( s ) BGL_MMAP( s ).length
#define BGL_MMAP_NAME( s ) BGL_MMAP( s ).name

#define BGL_MMAP_RP_GET( s ) (BGL_MMAP( s ).rp)
#define BGL_MMAP_RP_SET( s, p ) (BGL_MMAP_RP_GET( s ) = p)
   
#define BGL_MMAP_WP_GET( s ) (BGL_MMAP( s ).wp)
#define BGL_MMAP_WP_SET( s, p ) (BGL_MMAP_WP_GET( s ) = p)
   
#if HAVE_MMAP   
#  define BGL_MMAP_REF( s, i ) (BGL_MMAP( s ).map[ i ])
#  define BGL_MMAP_SET( s, i, c ) (BGL_MMAP_REF( s, i ) = c, BUNSPEC)
#else
#  define BGL_MMAP_REF( s, i ) bgl_mmap_nommap_ref( s, i )
#  define BGL_MMAP_SET( s, i, c ) bgl_mmap_nommap_set( s, i, c )
#endif

/*---------------------------------------------------------------------*/
/*    WEAKPTR                                                          */
/*---------------------------------------------------------------------*/
#define WEAKPTR( o ) CREF( o )->weakptr

#define WEAKPTR_SIZE (sizeof(struct bgl_weakptr))

#define BGL_WEAKPTRP( v ) \
 (POINTERP( v ) && (TYPE( v ) == WEAKPTR_TYPE))

BGL_RUNTIME_DECL obj_t make_weakptr( obj_t );
BGL_RUNTIME_DECL obj_t weakptr_data( obj_t );
BGL_RUNTIME_DECL void weakptr_data_set( obj_t , obj_t  );

/*---------------------------------------------------------------------*/
/*    Process handling                                                 */
/*---------------------------------------------------------------------*/
#define PROCESSP( o ) (POINTERP( o ) && (TYPE( o ) == PROCESS_TYPE))
#define PROCESS_SIZE (sizeof( struct process ))
#define PROCESS( o ) (CREF( o )->process)
#define PROCESS_PID( o ) (PROCESS( o ).pid)
#define PROCESS_INPUT_PORT( o ) (PROCESS( o ).stream[ 0 ])
#define PROCESS_OUTPUT_PORT( o ) (PROCESS( o ).stream[ 1 ])
#define PROCESS_ERROR_PORT( o ) (PROCESS( o ).stream[ 2 ])

/*---------------------------------------------------------------------*/
/*    Socket handling                                                  */
/*---------------------------------------------------------------------*/
#define SOCKETP( o ) (POINTERP( o ) && (TYPE( o ) == SOCKET_TYPE))
#define SOCKET_SIZE (sizeof( struct socket ))
#define SOCKET( o ) (CREF( o )->socket)
#define SOCKET_HOSTNAME( o ) bgl_socket_hostname( o )
#define SOCKET_HOSTIP( o ) (SOCKET( o ).hostip)
#define SOCKET_PORT( o ) (SOCKET( o ).portnum)
#define SOCKET_DOWNP( o ) (SOCKET( o ).fd == -1)
   
#define SOCKET_INPUT( o ) \
   (INPUT_PORTP( SOCKET( o ).input) ? \
    (SOCKET( o ).input) \
    : C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, \
			"socket-input", \
			"socket servers have no port", \
			o ))
#define SOCKET_OUTPUT( o ) \
   (OUTPUT_PORTP( SOCKET( o ).output ) ? \
    (SOCKET( o ).output) \
    : C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, \
			"socket-output", \
			"socket servers have no port", \
			o ))
   
#define BGL_SOCKET_SERVER 22
#define BGL_SOCKET_CLIENT 23
#define BGL_SOCKET_UNIX 24

#define BGL_SOCKET_SERVERP( o ) \
   (SOCKETP( o ) && (SOCKET( o ).stype == BGL_SOCKET_SERVER))
#define BGL_SOCKET_CLIENTP( o ) \
   (SOCKETP( o ) && (SOCKET( o ).stype >= BGL_SOCKET_CLIENT))
#define BGL_SOCKET_UNIXP( o ) \
   (SOCKETP( o ) && (SOCKET( o ).stype == BGL_SOCKET_UNIX))

#define SOCKET_CHOOK( o ) \
   (SOCKET( o ).chook)

/*---------------------------------------------------------------------*/
/*    Datagram Socket ...                                              */
/*---------------------------------------------------------------------*/
#define BGL_DATAGRAM_SOCKETP( o ) \
   (POINTERP( o ) && (TYPE( o ) == DATAGRAM_SOCKET_TYPE))
#define BGL_DATAGRAM_SOCKET_SIZE (sizeof( struct bgl_datagram_socket ))
#define BGL_DATAGRAM_SOCKET( o ) (CREF( o )->datagram_socket)

#define BGL_DATAGRAM_SOCKET_HOSTNAME( o ) bgl_datagram_socket_hostname( o )
#define BGL_DATAGRAM_SOCKET_HOSTIP( o ) (BGL_DATAGRAM_SOCKET( o ).hostip)
#define BGL_DATAGRAM_SOCKET_PORTNUM( o ) (BGL_DATAGRAM_SOCKET( o ).portnum)
#define BGL_DATAGRAM_SOCKET_PORT( o ) (BGL_DATAGRAM_SOCKET( o ).port)

#define BGL_DATAGRAM_SOCKET_SERVERP( o ) \
   (BGL_DATAGRAM_SOCKETP( o ) \
    && (BGL_DATAGRAM_SOCKET( o ).stype == BGL_SOCKET_SERVER))
#define BGL_DATAGRAM_SOCKET_CLIENTP( o ) \
   (BGL_DATAGRAM_SOCKETP( o ) \
    && (BGL_DATAGRAM_SOCKET( o ).stype >= BGL_SOCKET_CLIENT))
   
/*---------------------------------------------------------------------*/
/*    Regexp ...                                                       */
/*---------------------------------------------------------------------*/
#define BGL_REGEXPP( o ) \
   (POINTERP( o ) && (TYPE( o ) == REGEXP_TYPE))
#define BGL_REGEXP_SIZE (sizeof( struct bgl_regexp ))
#define BGL_REGEXP( o ) (CREF( o )->regexp)
#define BGL_REGEXP_PREG( o ) (BGL_REGEXP( o ).preg)   
#define BGL_REGEXP_PREG_SET( o, v ) (BGL_REGEXP_PREG( o ) = (v))
#define BGL_REGEXP_PAT( o ) (BGL_REGEXP( o ).pat)
#define BGL_REGEXP_CAPTURE_COUNT( o ) (BGL_REGEXP( o ).capturecount)

#define BGL_REGEXP_MATCH( o, string, stringp, beg, len ) \
   BGL_REGEXP( o ).match( o, string, stringp, beg, len )
#define BGL_REGEXP_MATCH_N( o, string, vres, beg, len ) \
   BGL_REGEXP( o ).match_n( o, string, vres, beg, len )
#define BGL_REGEXP_FREE( o ) \
   BGL_REGEXP( o ).free( o )
   
/*---------------------------------------------------------------------*/
/*    opaque                                                           */
/*---------------------------------------------------------------------*/
#define OPAQUEP( o ) (POINTERP( o ) && (TYPE( o ) == OPAQUE_TYPE))
#define BGL_OPAQUE( f ) CREF( f )
		 
BGL_RUNTIME_DECL header_t bgl_opaque_nil;
#define BGL_OPAQUE_NIL() BREF( &bgl_opaque_nil )
   
/*---------------------------------------------------------------------*/
/*    Custom management                                                */
/*---------------------------------------------------------------------*/
#define CUSTOMP( o ) (POINTERP( o ) && (TYPE( o ) == CUSTOM_TYPE))

#define CUSTOM_SIZE (sizeof( struct custom ))
#define CUSTOM( f ) CREF( f )->custom

#define CUSTOM_FINAL( f ) CUSTOM( f ).final
#define CUSTOM_SERIAL( f ) CUSTOM( f ).serial
#define CUSTOM_EQUAL( f ) CUSTOM( f ).equal
#define CUSTOM_HASH( f ) CUSTOM( f ).hash
#define CUSTOM_TO_STRING( f ) CUSTOM( f ).to_string
#define CUSTOM_OUTPUT( f ) CUSTOM( f ).output

#define CUSTOM_CMP( c1, c2 ) CUSTOM_EQUAL( c1 )( c1, c2 )

#define CUSTOM_IDENTIFIER( f ) CUSTOM( f ).identifier
#define CUSTOM_IDENTIFIER_SET( f, v ) (CUSTOM( f ).identifier = v, BUNSPEC)
#define CUSTOM_HASH_NUMBER( f ) (CUSTOM_HASH( f )( f ))

/*---------------------------------------------------------------------*/
/*    Date                                                             */
/*---------------------------------------------------------------------*/
#define BGL_DATEP( o ) (POINTERP( o ) && (TYPE( o ) == DATE_TYPE))

#define BGL_DATE_SIZE (sizeof( struct bgl_date ) )
#define BGL_DATE( f ) CREF( f )->date

#define BGL_DATE_NANOSECOND( f ) (BGL_DATE( f ).nsec)
#define BGL_DATE_MILLISECOND( f ) (BGL_DATE( f ).nsec / 1000000)
#if( BGL_HAVE_GMTOFF )
#  define BGL_DATE_TIMEZONE( f ) (BGL_DATE( f ).tm.tm_gmtoff)
#else
#  define BGL_DATE_TIMEZONE( f ) (BGL_DATE( f ).timezone)
#endif
   
#define BGL_DATE_ISGMT( f ) (HEADER_SIZE( CREF( f )->header) > 0)

#define BGL_DATE_ISDST( f ) (BGL_DATE( f ).tm.tm_isdst)
#define BGL_DATE_SECOND( f ) (BGL_DATE( f ).tm.tm_sec)
#define BGL_DATE_MINUTE( f ) (BGL_DATE( f ).tm.tm_min)
#define BGL_DATE_HOUR( f ) (BGL_DATE( f ).tm.tm_hour)
#define BGL_DATE_DAY( f ) (BGL_DATE( f ).tm.tm_mday)
#define BGL_DATE_WDAY( f ) (BGL_DATE( f ).tm.tm_wday + 1)
#define BGL_DATE_YDAY( f ) (BGL_DATE( f ).tm.tm_yday + 1)
#define BGL_DATE_MONTH( f ) (BGL_DATE( f ).tm.tm_mon + 1)
#define BGL_DATE_YEAR( f ) (BGL_DATE( f ).tm.tm_year + 1900)
#define BGL_DATE_TIME( f ) (BGL_DATE( f ).time)

#define BGL_DATE_UPDATE_MILLISECOND( f, ms ) (BGL_DATE( f ).nsec = ((BGL_LONGLONG_T)(ms * 1000000)))
#define BGL_DATE_UPDATE_SECOND( f, sec ) (BGL_DATE( f ).tm.tm_sec = sec)
#define BGL_DATE_UPDATE_MINUTE( f, min ) (BGL_DATE( f ).tm.tm_min = min)
#define BGL_DATE_UPDATE_TIME( f, tm ) (BGL_DATE( f ).time = tm)

/*---------------------------------------------------------------------*/
/*    Mutexes and condition variables                                  */
/*---------------------------------------------------------------------*/
#define BGL_MUTEXP( o ) (POINTERP( o ) && (TYPE( o ) == MUTEX_TYPE))
   
#define BGL_MUTEX( o )  (CREF( o )->mutex)
#define BGL_MUTEX_SIZE (sizeof( struct bgl_mutex ))

#define BGL_MUTEX_LOCK( o ) \
   (BGL_MUTEX( o ).syslock( BGL_MUTEX_SYSMUTEX( o ) ))

#define BGL_MUTEX_TRYLOCK( o ) \
   (BGL_MUTEX( o ).systrylock( BGL_MUTEX_SYSMUTEX( o ) ))

#define BGL_MUTEX_TIMED_LOCK( o, to ) \
   (BGL_MUTEX( o ).systimedlock( BGL_MUTEX_SYSMUTEX( o ), to ))

#define BGL_MUTEX_UNLOCK( o ) \
   (BGL_MUTEX( o ).sysunlock( BGL_MUTEX_SYSMUTEX( o ) ))
   
#define BGL_MUTEX_LOCK_PRELOCK( o, l ) \
   (BGL_MUTEX( o ).syslockprelock( BGL_MUTEX_SYSMUTEX( o ), l ))

#define BGL_MUTEX_STATE( o ) \
   (BGL_MUTEX( o ).sysstate( BGL_MUTEX_SYSMUTEX( o ) ))
   
#define BGL_MUTEX_NAME( o ) BGL_MUTEX( o ).name

#define BGL_MUTEX_BACKEND( o ) BGL_MUTEX( o ).backend

#if( defined( BGL_INLINE_MUTEX ) )   
#  define BGL_MUTEX_SYSMUTEX( o ) &(BGL_MUTEX( o ).sysmutex)
#else
#  define BGL_MUTEX_SYSMUTEX( o ) BGL_MUTEX( o ).sysmutex
#endif
   
#define BGL_CONDVARP( o ) (POINTERP( o ) && (TYPE( o ) == CONDVAR_TYPE))
#define BGL_CONDVAR( o ) (CREF( o )->condvar)
#define BGL_CONDVAR_SIZE (sizeof( struct bgl_condvar ))

#define BGL_CONDVAR_NAME( o ) (BGL_CONDVAR( o ).name)
   
#define BGL_CONDVAR_WAIT( o, cv ) (BGL_CONDVAR( o ).syswait( o, cv ))
#define BGL_CONDVAR_TIMED_WAIT( o, cv, to ) (BGL_CONDVAR( o ).systimedwait( o, cv, to ))
#define BGL_CONDVAR_BROADCAST( o ) (BGL_CONDVAR( o ).sysbroadcast( o ))
#define BGL_CONDVAR_SIGNAL( o ) (BGL_CONDVAR( o ).syssignal( o ))
   
/*---------------------------------------------------------------------*/
/*    Foreign management                                               */
/*---------------------------------------------------------------------*/
#define FOREIGNP( o ) (POINTERP( o ) && (TYPE( o ) == FOREIGN_TYPE))

#define FOREIGN_TYPE_NAME( o ) "_"
   
#define FOREIGN_SIZE (sizeof( struct foreign ))
#define FOREIGN( f ) CREF( f )->foreign

#define FOREIGN_NULL( obj ) ((obj == BFALSE) ? 0L : obj)
#define FOREIGN_TO_COBJ( f ) (FOREIGN( f ).cobj)
#define FOREIGN_NULLP( f )   ((bool_t)(!FOREIGN_TO_COBJ( f )))
#define FOREIGN_EQP( o1,o2 ) \
  ((bool_t)(FOREIGN_TO_COBJ( o1 ) == (FOREIGN_TO_COBJ( o2 ))))
#define FOREIGN_PTR_NULL( _p ) (_p == 0)
   
#define FOREIGN_ID( f ) FOREIGN( f ).id

#define C_STRUCT_REF( o, type, slot )             \
   (((type)o)->slot)
#define C_STRUCT_SET( o, type, slot, value )      \
   (C_STRUCT_REF( o, type, slot ) = value, BUNSPEC)

#define C_STRUCT_REF_ADDR( o, type, slot )        \
   (&(((type)o)->slot))
#define C_STRUCT_SET_ADDR( o, type, slot, value ) \
   (((type)o)->slot = *value, BUNSPEC)

#define C_POINTER_REF( o, type, i )               \
   (((type *)o)[ i ])
#define C_POINTER_SET( o, type, i, v )            \
   (C_POINTER_REF( o, type, i ) = v, BUNSPEC)

#define C_POINTER_REF_ADDR( o, type, i )          \
   (&(o)[ i ])
#define C_POINTER_SET_ADDR( o, type, i, v )       \
   (o[ i ] = *v, BUNSPEC)

#define C_FUNCTION_CALL_0( F ) \
   F()
#define C_FUNCTION_CALL_1( F,a ) \
   F( a )
#define C_FUNCTION_CALL_2( F,a,b ) \
   F( a,b )
#define C_FUNCTION_CALL_3( F,a,b,c ) \
   F( a,b,c )
#define C_FUNCTION_CALL_4( F,a,b,c,d ) \
   F( a,b,c,d )
#define C_FUNCTION_CALL_5( F,a,b,c,d,e ) \
   F( a,b,c,d,e )
#define C_FUNCTION_CALL_6( F,a,b,c,d,e,f ) \
   F( a,b,c,d,e,f )
#define C_FUNCTION_CALL_7( F,a,b,c,d,e,f,h ) \
   F( a,b,c,d,e,f,h )
#define C_FUNCTION_CALL_8( F,a,b,c,d,e,f,h,i ) \
   F( a,b,c,d,e,f,h,i )
#define C_FUNCTION_CALL_9( F,a,b,c,d,e,f,h,i,j ) \
   F( a,b,c,d,e,f,h,i,j )
#define C_FUNCTION_CALL_10( F,a,b,c,d,e,f,h,i,j,k ) \
   F( a,b,c,d,e,f,h,i,j,k )
#define C_FUNCTION_CALL_11( F,a,b,c,d,e,f,h,i,j,k,l ) \
   F( a,b,c,d,e,f,h,i,j,k,l )
#define C_FUNCTION_CALL_12( F,a,b,c,d,e,f,h,i,j,k,l,m ) \
   F( a,b,c,d,e,f,h,i,j,k,l,m )
#define C_FUNCTION_CALL_13( F,a,b,c,d,e,f,h,i,j,k,l,m,n ) \
   F( a,b,c,d,e,f,h,i,j,k,l,m,n )
#define C_FUNCTION_CALL_14( F,a,b,c,d,e,f,h,i,j,k,l,m,n,o ) \
   F( a,b,c,d,e,f,h,i,j,k,l,m,n,o )
#define C_FUNCTION_CALL_15( F,a,b,c,d,e,f,h,i,j,k,l,m,n,o,p ) \
   F( a,b,c,d,e,f,h,i,j,k,l,m,n,o,p )
#define C_FUNCTION_CALL_16( F,a,b,c,d,e,f,h,i,j,k,l,m,n,o,p,q ) \
   F( a,b,c,d,e,f,h,i,j,k,l,m,n,o,p,q )

#define CENUM_TO_FOREIGN( _1, _2 ) cobj_to_foreign( _1, (void *)_2 )

/*---------------------------------------------------------------------*/
/*    Internal structure representing per thread dynamic environment.  */
/*    -------------------------------------------------------------    */
/*    This is *not* a Bigloo type hence it does not need any           */
/*    dynamic type information.                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL BGL_THREAD_DECL obj_t single_thread_denv;
BGL_RUNTIME_DECL void bgl_init_dynamic_env();
BGL_RUNTIME_DECL obj_t make_dynamic_env();
BGL_RUNTIME_DECL obj_t bgl_dup_dynamic_env( obj_t );
BGL_RUNTIME_DECL obj_t (*bgl_multithread_dynamic_denv)();

#define BGL_DYNAMIC_ENVP( c ) \
   (POINTERP( c ) && (TYPE( c ) == DYNAMIC_ENV_TYPE))

#define BGL_DYNAMIC_ENV( env ) (CREF( env )->dynamic_env)

#if( BGL_HAS_THREAD_LOCALSTORAGE )
#  define BGL_CURRENT_DYNAMIC_ENV() \
     single_thread_denv
#else   
#  define BGL_CURRENT_DYNAMIC_ENV() \
     (single_thread_denv ? single_thread_denv : bgl_multithread_dynamic_denv())
#endif   

/* The set of macros for accessing dynamic environments */
#define BGL_ENV_USER_DATA( env ) \
   (BGL_DYNAMIC_ENV( env ).user_data)
#define BGL_ENV_USER_DATA_SET( env, _l ) \
   (BGL_DYNAMIC_ENV( env ).user_data = (_l))
   
#define BGL_ENV_THREAD_BACKEND( env ) \
   (BGL_DYNAMIC_ENV( env ).thread_backend)
#define BGL_ENV_THREAD_BACKEND_SET( env, _l ) \
   (BGL_DYNAMIC_ENV( env ).thread_backend = (_l))
   
#define BGL_ENV_PARAMETERS( env ) \
  (BGL_DYNAMIC_ENV( env ).parameters)
#define BGL_ENV_PARAMETERS_SET( env, _l ) \
  (BGL_DYNAMIC_ENV( env ).parameters = (_l))
   
#define BGL_ENV_SIG_HANDLERS( env ) \
  (BGL_DYNAMIC_ENV( env ).sig_handlers)
   
#define BGL_ENV_CURRENT_OUTPUT_PORT( env ) \
   (BGL_DYNAMIC_ENV( env ).current_output_port)
#define BGL_ENV_CURRENT_OUTPUT_PORT_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).current_output_port = (_1), BUNSPEC)
   
#define BGL_ENV_CURRENT_ERROR_PORT( env ) \
   (BGL_DYNAMIC_ENV( env ).current_error_port)
#define BGL_ENV_CURRENT_ERROR_PORT_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).current_error_port = (_1), BUNSPEC)
   
#define BGL_ENV_CURRENT_INPUT_PORT( env ) \
   (BGL_DYNAMIC_ENV( env ).current_input_port)
#define BGL_ENV_CURRENT_INPUT_PORT_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).current_input_port = (_1), BUNSPEC)
   
#define BGL_ENV_STACK_BOTTOM( env ) \
   (BGL_DYNAMIC_ENV( env ).stack_bottom)
#define BGL_ENV_STACK_BOTTOM_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).stack_bottom = (_1), BUNSPEC)
   
#define BGL_ENV_EXIT_VALUE( env ) \
   (BGL_DYNAMIC_ENV( env ).exit_value)
#define BGL_ENV_EXIT_VALUE_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).exit_value = (_1), BUNSPEC)
   
#define BGL_ENV_EXIT_TRACES( env ) \
   (BGL_DYNAMIC_ENV( env ).exit_traces)
#define BGL_ENV_EXIT_TRACES_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).exit_traces = (_1), BUNSPEC)
   
#define BGL_ENV_EXITD_BOTTOM( env ) \
   (BGL_DYNAMIC_ENV( env ).exitd_bottom)
   
#define BGL_ENV_EXITD_TOP( env ) \
   (BGL_DYNAMIC_ENV( env ).exitd_top)
#define BGL_ENV_EXITD_TOP_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).exitd_top = (_1), BUNSPEC)
   
#define BGL_ENV_EXITD_VAL( env ) \
   (BGL_DYNAMIC_ENV( env ).exitd_val)
#define BGL_ENV_EXITD_VAL_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).exitd_val = (_1), BUNSPEC)
   
#define BGL_ENV_EXITD_STAMP( env ) \
   (BGL_DYNAMIC_ENV( env ).exitd_stamp = \
      BINT( 1 + CINT( BGL_DYNAMIC_ENV( env ).exitd_stamp )), \
    BGL_DYNAMIC_ENV( env ).exitd_stamp)
   
#define BGL_ENV_BEFORED_TOP( env ) \
   (BGL_DYNAMIC_ENV( env ).befored_top)
#define BGL_ENV_BEFORED_TOP_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).befored_top = (_1), BUNSPEC)
   
#define BGL_ENV_MVALUES_NUMBER( env ) \
   (BGL_DYNAMIC_ENV( env ).mvalues_number)
#define BGL_ENV_MVALUES_NUMBER_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).mvalues_number = (_1))
   
#define BGL_ENV_MVALUES_VAL( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).mvalues[ _1 ])
#define BGL_ENV_MVALUES_VAL_SET( env, _1, _2 ) \
   (BGL_DYNAMIC_ENV( env ).mvalues[ _1 ] = (_2), BUNSPEC)

#define BGL_ENV_ERROR_HANDLER_GET( env ) \
  (BGL_DYNAMIC_ENV( env ).error_handler)
#define BGL_ENV_ERROR_HANDLER_SET( env, _hdl ) \
  (BGL_DYNAMIC_ENV( env ).error_handler = (_hdl))
   
#define BGL_ENV_UNCAUGHT_EXCEPTION_HANDLER_GET( env ) \
  (BGL_DYNAMIC_ENV( env ).uncaught_exception_handler)
#define BGL_ENV_UNCAUGHT_EXCEPTION_HANDLER_SET( env, _hdl ) \
  (BGL_DYNAMIC_ENV( env ).uncaught_exception_handler = (_hdl))
   
#define BGL_ENV_ERROR_NOTIFIERS_GET( env ) \
  (BGL_DYNAMIC_ENV( env ).error_notifiers)
#define BGL_ENV_ERROR_NOTIFIERS_SET( env , _not ) \
  (BGL_DYNAMIC_ENV( env ).error_notifiers = (_not))
   
#define BGL_ENV_INTERRUPT_NOTIFIER_GET( env ) \
  (BGL_DYNAMIC_ENV( env ).interrupt_notifier)
#define BGL_ENV_INTERRUPT_NOTIFIER_SET( env, _hdl ) \
  (BGL_DYNAMIC_ENV( env ).interrupt_notifier = (_hdl))
   
#define BGL_ENV_DEBUG_ALIST_GET( env ) \
  (BGL_DYNAMIC_ENV( env ).debug_alist)
#define BGL_ENV_DEBUG_ALIST_SET( env, _l ) \
  (BGL_DYNAMIC_ENV( env ).debug_alist = (_l))
   
#define BGL_ENV_LEXICAL_STACK( env ) \
  (BGL_DYNAMIC_ENV( env ).lexical_stack)
#define BGL_ENV_LEXICAL_STACK_SET( env, _l ) \
  (BGL_DYNAMIC_ENV( env ).lexical_stack = (_l))
   
#define BGL_ENV_EVSTATE( env ) \
   (BGL_DYNAMIC_ENV( env ).evstate)
#define BGL_ENV_EVSTATE_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).evstate = (_1), BUNSPEC)
   
#define BGL_ENV_MODULE( env ) \
   (BGL_DYNAMIC_ENV( env ).module)
#define BGL_ENV_MODULE_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).module = (_1), BUNSPEC)

#define BGL_ENV_ABASE( env ) \
   (BGL_DYNAMIC_ENV( env ).abase)
#define BGL_ENV_ABASE_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).abase = (_1), BUNSPEC)

#define BGL_ENV_SAW_SP( env ) \
   (BGL_DYNAMIC_ENV( env ).saw_sp)
#define BGL_ENV_SAW_SP_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).saw_sp = (_1))

#define BGL_ENV_SAW_NURSERY( env ) \
   (BGL_DYNAMIC_ENV( env ).saw_nursery)
#define BGL_ENV_SAW_NURSERY_SET( env, _1 ) \
   (BGL_DYNAMIC_ENV( env ).saw_nursery = (_1))

/*--- old interface ---------------------------------------------------*/
#define BGL_PARAMETERS() \
   BGL_ENV_PARAMETERS( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_PARAMETERS_SET( _l ) \
   BGL_ENV_PARAMETERS_SET( BGL_CURRENT_DYNAMIC_ENV(), _l )
   
#define BGL_SIG_HANDLERS() \
   BGL_ENV_SIG_HANDLERS( BGL_CURRENT_DYNAMIC_ENV() )
   
#define BGL_THREAD_BACKEND() \
   BGL_ENV_THREAD_BACKEND( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_THREAD_BACKEND_SET( _v ) \
   BGL_ENV_THREAD_BACKEND_SET( BGL_CURRENT_DYNAMIC_ENV(), _v )
   
#define BGL_EXIT_VALUE() \
   BGL_ENV_EXIT_VALUE( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_EXIT_VALUE_SET( _1 ) \
   BGL_ENV_EXIT_VALUE_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )
   
#define BGL_EXITD_TOP() \
   BGL_ENV_EXITD_TOP( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_EXITD_TOP_SET( _1 ) \
   BGL_ENV_EXITD_TOP_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )

#define BGL_EXITD_BOTTOM() \
   BGL_ENV_EXITD_BOTTOM( BGL_CURRENT_DYNAMIC_ENV() )

#define BGL_EXITD_TOP_AS_OBJ() \
   ((obj_t)BGL_EXITD_TOP())

#define BGL_EXITD_VAL() \
   BGL_ENV_EXITD_VAL( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_EXITD_VAL_SET( _1 ) \
   BGL_ENV_EXITD_VAL_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )
   
#define BGL_EXITD_STAMP() \
   BGL_ENV_EXITD_STAMP( BGL_CURRENT_DYNAMIC_ENV() )
   
#define BGL_BEFORED_TOP() \
   BGL_ENV_BEFORED_TOP( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_BEFORED_TOP_SET( _1 ) \
   BGL_ENV_BEFORED_TOP_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )
   
#define BGL_MVALUES_NUMBER() \
   BGL_ENV_MVALUES_NUMBER( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_MVALUES_NUMBER_SET( _1 ) \
   BGL_ENV_MVALUES_NUMBER_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )
   
#define BGL_MVALUES_VAL( _1 ) \
   BGL_ENV_MVALUES_VAL( BGL_CURRENT_DYNAMIC_ENV(), _1 )
#define BGL_MVALUES_VAL_SET( _1, _2 ) \
   BGL_ENV_MVALUES_VAL_SET( BGL_CURRENT_DYNAMIC_ENV(), _1, _2 )

#define BGL_ERROR_HANDLER_GET() \
   BGL_ENV_ERROR_HANDLER_GET( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_ERROR_HANDLER_SET( _hdl ) \
   BGL_ENV_ERROR_HANDLER_SET( BGL_CURRENT_DYNAMIC_ENV(), _hdl )
   
#define BGL_UNCAUGHT_EXCEPTION_HANDLER_GET() \
   BGL_ENV_UNCAUGHT_EXCEPTION_HANDLER_GET( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_UNCAUGHT_EXCEPTION_HANDLER_SET( _hdl ) \
   BGL_ENV_UNCAUGHT_EXCEPTION_HANDLER_SET( BGL_CURRENT_DYNAMIC_ENV(), _hdl )

#define BGL_ERROR_NOTIFIERS_GET() \
   BGL_ENV_ERROR_NOTIFIERS_GET( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_ERROR_NOTIFIERS_SET( _not ) \
   BGL_ENV_ERROR_NOTIFIERS_SET( BGL_CURRENT_DYNAMIC_ENV(), _not )
   
#define BGL_INTERRUPT_NOTIFIER_GET() \
   BGL_ENV_INTERRUPT_NOTIFIER_GET( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_INTERRUPT_NOTIFIER_SET( _hdl ) \
   BGL_ENV_INTERRUPT_NOTIFIER_SET( BGL_CURRENT_DYNAMIC_ENV(), _hdl )
   
#define BGL_DEBUG_ALIST_GET() \
   BGL_ENV_DEBUG_ALIST_GET( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_DEBUG_ALIST_SET( _l ) \
   BGL_ENV_DEBUG_ALIST_SET( BGL_CURRENT_DYNAMIC_ENV(), _l )
   
#define BGL_LEXICAL_STACK() \
   BGL_ENV_LEXICAL_STACK( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_LEXICAL_STACK_SET( _l ) \
   BGL_ENV_LEXICAL_STACK_SET( BGL_CURRENT_DYNAMIC_ENV(), _l )
   
#define BGL_MODULE() \
   BGL_ENV_MODULE( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_MODULE_SET( _1 ) \
   BGL_ENV_MODULE_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )
   
#define BGL_ABASE() \
   BGL_ENV_ABASE( BGL_CURRENT_DYNAMIC_ENV() )
#define BGL_ABASE_SET( _1 ) \
   BGL_ENV_ABASE_SET( BGL_CURRENT_DYNAMIC_ENV(), _1 )
   
/*---------------------------------------------------------------------*/
/*    The debugging strack traces                                      */
/*---------------------------------------------------------------------*/
#define BGL_ENV_GET_TOP_OF_FRAME( env ) \
   (BGL_DYNAMIC_ENV( env ).top_of_frame)
#define BGL_ENV_SET_TOP_OF_FRAME( env, _top ) \
   (BGL_ENV_GET_TOP_OF_FRAME( env ) = (_top))

#define BGL_ENV_PUSH_TRACE( env, nm, loc ) \
   struct bgl_dframe bgl_dframe; \
   struct bgl_dframe *bgl_link; \
    \
   bgl_dframe.name = nm; \
   bgl_dframe.location = loc; \
   bgl_link = bgl_dframe.link = BGL_ENV_GET_TOP_OF_FRAME( env ); \
   BGL_ENV_SET_TOP_OF_FRAME( env, &bgl_dframe );      
   
#define BGL_ENV_PUSH_TRACE_NAME( env, name ) \
   BGL_ENV_PUSH_TRACE( env, name, BUNSPEC )
   
#define BGL_ENV_POP_TRACE( env ) \
   BGL_ENV_SET_TOP_OF_FRAME( env, bgl_link );

#define BGL_ENV_SET_TRACE_NAME( env, nm ) \
   (BGL_ENV_GET_TOP_OF_FRAME( env )->name = nm)

#define BGL_ENV_SET_TRACE_LOCATION( env, loc ) \
   (BGL_ENV_GET_TOP_OF_FRAME( env )->location = loc)

#define BGL_PUSH_TRACE( nm, loc ) \
   obj_t bgl_denv = BGL_CURRENT_DYNAMIC_ENV(); \
   BGL_ENV_PUSH_TRACE( bgl_denv, nm, loc )

#define BGL_PUSH_TRACE_LOCATION PUSH_TRACE
   
#define BGL_POP_TRACE() \
   BGL_ENV_POP_TRACE( bgl_denv )

/* after a bind-exit, we must reset the current trace */
/* See cgen/emit-cop.scm and SawC/code.scm            */
/* MS 21apr2010: these macros cannot use a local var  */
/* because longjmp does not restore them. Hence, the  */
/* top_of_stack value has to be stored in the env.    */   
#define BGL_ENV_STORE_TRACE( env ) \
   BGL_ENV_EXIT_TRACES_SET( env, \
     MAKE_PAIR( (obj_t)BGL_ENV_GET_TOP_OF_FRAME( env ), BGL_ENV_EXIT_TRACES( env ) ) )

#define BGL_ENV_RESTORE_TRACE( env ) \
   BGL_ENV_SET_TOP_OF_FRAME( env, (struct bgl_dframe *)CAR( BGL_ENV_EXIT_TRACES( env ) ) ); \
   BGL_ENV_EXIT_TRACES_SET( env, CDR( BGL_ENV_EXIT_TRACES( env ) ) );
   
/* after a bind-exit, we must reset the current trace */
/* See cgen/emit-cop.scm and SawC/code.scm            */
#define BGL_STORE_TRACE() \
   BGL_ENV_STORE_TRACE( BGL_CURRENT_DYNAMIC_ENV() )

#define BGL_RESTORE_TRACE() \
   BGL_ENV_RESTORE_TRACE( BGL_CURRENT_DYNAMIC_ENV() )
   
/*---------------------------------------------------------------------*/
/*    Failures                                                         */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_BIGLOO_ABORT
#  define __BIGLOO_ABORT( _x ) (BINT( bigloo_abort( CINT( _x ) ) ))
#else   
#  define __BIGLOO_ABORT( _x ) (_x)
#endif
   
#define FAILURE( p, m, o ) \
   exit( (bigloo_exit( __BIGLOO_ABORT( the_failure( p, m, o ) ) ), 0) )

#define C_FAILURE( p, m, o ) \
   FAILURE( string_to_bstring( p ), string_to_bstring( m ), o )

#define SYSTEM_FAILURE( sysno, proc, msg, obj ) \
   bigloo_exit( __BIGLOO_ABORT( bgl_system_failure( sysno, proc, msg, obj ) ) )

#define C_SYSTEM_FAILURE( sn, p, m, o ) \
   SYSTEM_FAILURE( sn, string_to_bstring( p ), string_to_bstring( m ), (o) )

#define BGL_ERROR                     1
#define BGL_LOCATION_ERROR            2
   
#define BGL_TYPE_ERROR               10
#define BGL_TYPENAME_ERROR           11
#define BGL_INDEX_OUT_OF_BOUND_ERROR 12
   
#define BGL_IO_ERROR                 20   
#define BGL_IO_PORT_ERROR            21
   
#define BGL_IO_READ_ERROR            31
#define BGL_IO_WRITE_ERROR           32
#define BGL_IO_CLOSED_ERROR          33
#define BGL_IO_FILE_NOT_FOUND_ERROR  34
#define BGL_IO_UNKNOWN_HOST_ERROR    35
#define BGL_IO_PARSE_ERROR           36
#define BGL_IO_MALFORMED_URL_ERROR   37
#define BGL_IO_SIGPIPE_ERROR         38
#define BGL_IO_TIMEOUT_ERROR         39
#define BGL_IO_CONNECTION_ERROR      40
   
#define BGL_PROCESS_EXCEPTION        50
   
/*---------------------------------------------------------------------*/
/*    BGL_MKDIR                                                        */
/*---------------------------------------------------------------------*/
#define BGL_MKDIR( a, b ) (mkdir( a, (mode_t)b ))
   
/*---------------------------------------------------------------------*/
/*    RGC macros                                                       */
/*---------------------------------------------------------------------*/
#define RGC_DEBUG 2
#undef RGC_DEBUG

#if defined( RGC_DEBUG )
#  include <assert.h>
#else
#undef assert   
#  define assert( exp ) ;
#endif

#define RGC_BUFFER_REF( p, o ) \
   STRING_REF( BGL_INPUT_PORT_BUFFER( p ), o )
#define RGC_BUFFER_SET( p, o, c ) \
   STRING_SET( BGL_INPUT_PORT_BUFFER( p ), o, c )

#define RGC_BUFFER_GET_CHAR( i, offset ) \
   ((int)(RGC_BUFFER_REF( i, offset )))
   
#define RGC_BUFFER_PEEK_CHAR( i ) \
   ((unsigned int)(RGC_BUFFER_REF( i, INPUT_PORT( i ).forward )))
   
#define RGC_BUFFER_BYTE_REF( i, offset ) \
   ((int)(RGC_BUFFER_REF( i, INPUT_PORT( i ).matchstart + offset )))
   
#define RGC_BUFFER_BYTE( i ) \
   ((int)(RGC_BUFFER_BYTE_REF( i, 0 )))
   
#define RGC_BUFFER_CHARACTER( i ) \
   ((char)(RGC_BUFFER_BYTE( i )))
   
#define RGC_SET_FILEPOS( p ) \
   (INPUT_PORT( p ).filepos += RGC_BUFFER_MATCH_LENGTH( p ))

#define RGC_START_MATCH( p )  \
   (INPUT_PORT( p ).forward = \
     INPUT_PORT( p ).matchstart = \
      INPUT_PORT( p ).matchstop)

#define RGC_STOP_MATCH( p, forward ) \
   (INPUT_PORT( p ).matchstop = (forward))

#define RGC_MATCH_FAIL( p ) \
   (INPUT_PORT( p ).matchstart++)

#define RGC_FAILING_CHAR( p ) \
   ((unsigned int)(RGC_BUFFER_REF( p, INPUT_PORT( p ).matchstart ]))

#define RGC_BUFFER_AVAILABLE( o ) \
   (INPUT_PORT( o ).bufpos - INPUT_PORT( o ).matchstop)
   
#define RGC_BUFFER_MATCH_LENGTH( p ) \
   (INPUT_PORT( p ).matchstop - INPUT_PORT( p ).matchstart)

#define RGC_BUFFER_POSITION( p, forward ) \
   (forward - INPUT_PORT( p ).matchstart)

#define RGC_BUFFER_FORWARD( p ) \
   (INPUT_PORT( p ).forward)

#define RGC_BUFFER_BUFPOS( p ) \
   (INPUT_PORT( p ).bufpos)

#define RGC_MATCHSTOP( p ) \
   (INPUT_PORT( p ).matchstop)

/*---------------------------------------------------------------------*/
/*    `exit' machinery                                                 */
/*---------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
/*    The interperter locations                                        */
/*---------------------------------------------------------------------*/
#define __EVMEANING_ADDRESS( x ) \
   BREF( &(x) )
#define __EVMEANING_ADDRESS_REF( x ) \
   (*((obj_t *)CREF( x )))
#define __EVMEANING_ADDRESS_SET( x, y ) \
   (__EVMEANING_ADDRESS_REF( x ) = (obj_t)y, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Call/cc stuff.                                                   */
/*---------------------------------------------------------------------*/
#define STACK_SIZE (sizeof( struct stack ))
   
#define STACK( _o_ ) CREF( _o_ )->stack

#define STACKP( _s_ ) (POINTERP( _s_ ) && (TYPE( _s_ ) == STACK_TYPE))

#define MAKE_STACK( _size_, aux )                  \
   ( aux = GC_MALLOC( STACK_SIZE + (long)_size_ ), \
     aux->header = MAKE_HEADER( STACK_TYPE, 0 ),   \
     ( BREF( aux ) ) )

/*---------------------------------------------------------------------*/
/*    Hash table sizes                                                 */
/*---------------------------------------------------------------------*/
#define SYMBOL_HASH_TABLE_SIZE_SHIFT  12
#define SYMBOL_HASH_TABLE_SIZE        (1 << SYMBOL_HASH_TABLE_SIZE_SHIFT)

#define KEYWORD_HASH_TABLE_SIZE_SHIFT 6
#define KEYWORD_HASH_TABLE_SIZE       (1 << KEYWORD_HASH_TABLE_SIZE_SHIFT)

/*---------------------------------------------------------------------*/
/*    Dynamic loading default entry init symbol.                       */
/*---------------------------------------------------------------------*/
#define BGL_DYNAMIC_LOAD_INIT "bigloo_dlopen_init"

/*---------------------------------------------------------------------*/
/*    The external declarations                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t bigloo_exit( obj_t );

BGL_RUNTIME_DECL obj_t va_generic_entry( obj_t, ... );
BGL_RUNTIME_DECL obj_t bgl_va_stack_entry( obj_t, ... );
BGL_RUNTIME_DECL obj_t opt_generic_entry( obj_t, ... );
BGL_RUNTIME_DECL obj_t apply( obj_t, obj_t );

BGL_RUNTIME_DECL void bgl_init_module_debug_start( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_string( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_library( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_import( char *, char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_object( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_end( char * );
   
BGL_RUNTIME_DECL obj_t the_failure( obj_t, obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_system_failure( int, obj_t, obj_t, obj_t );

BGL_RUNTIME_DECL obj_t bgl_make_procedure( obj_t, int, int );
BGL_RUNTIME_DECL obj_t make_fx_procedure( function_t, int, int );
BGL_RUNTIME_DECL obj_t make_va_procedure( function_t, int, int );
BGL_RUNTIME_DECL obj_t bgl_init_fx_procedure( obj_t, function_t, int, int );
BGL_RUNTIME_DECL obj_t bgl_dup_procedure( obj_t );

BGL_RUNTIME_DECL obj_t bgl_time( obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_procedure_entry_to_string( obj_t ); 
BGL_RUNTIME_DECL obj_t bgl_string_to_procedure_entry( obj_t );

BGL_RUNTIME_DECL obj_t make_belong( long );
BGL_RUNTIME_DECL obj_t make_bllong( BGL_LONGLONG_T );

#if( !defined( BGL_CNST_SHIFT_INT32 ) )   
BGL_RUNTIME_DECL obj_t bgl_make_bint32( int32_t );
BGL_RUNTIME_DECL obj_t bgl_make_buint32( uint32_t );
#endif
   
BGL_RUNTIME_DECL obj_t bgl_make_bint64( int64_t );
BGL_RUNTIME_DECL obj_t bgl_make_buint64( uint64_t );

BGL_RUNTIME_DECL obj_t bgl_make_output_port( obj_t, bgl_stream_t, int, obj_t, obj_t, ssize_t (*)(), long (*)(), int (*)() );
BGL_RUNTIME_DECL void bgl_output_port_buffer_set( obj_t, obj_t );   
BGL_RUNTIME_DECL obj_t bgl_close_output_port( obj_t );
BGL_RUNTIME_DECL obj_t get_output_string( obj_t );
BGL_RUNTIME_DECL obj_t bgl_file_to_output_port( FILE *, obj_t );
BGL_RUNTIME_DECL obj_t bgl_open_output_file( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_append_output_file( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_open_output_string( obj_t );
BGL_RUNTIME_DECL obj_t bgl_open_output_procedure( obj_t, obj_t, obj_t, obj_t );
BGL_RUNTIME_DECL long bgl_output_port_filepos( obj_t );
BGL_RUNTIME_DECL obj_t bgl_output_port_seek( obj_t, long );
BGL_RUNTIME_DECL obj_t bgl_reset_output_string_port( obj_t );
BGL_RUNTIME_DECL bool_t bgl_output_port_truncate( obj_t, long );
BGL_RUNTIME_DECL bool_t bgl_port_isatty( obj_t );
BGL_RUNTIME_DECL obj_t bgl_reset_output_port_error( obj_t );
					
BGL_RUNTIME_DECL obj_t ucs2_string_to_utf8_string( obj_t );
BGL_RUNTIME_DECL obj_t make_ucs2_string( int, ucs2_t );
   
BGL_RUNTIME_DECL obj_t bgl_find_runtime_type( obj_t );
   
BGL_RUNTIME_DECL obj_t cobj_to_foreign( obj_t, void * );
BGL_RUNTIME_DECL long obj_to_cobj( obj_t );
BGL_RUNTIME_DECL int _bigloo_main( int, char *[], char *[], obj_t (*)(obj_t), int (*)(int, char *[], char *[]), long );
#if BGL_HAVE_BIGLOO_ABORT
extern long bigloo_abort( long );
#endif

BGL_RUNTIME_DECL obj_t string_to_symbol( char * );
BGL_RUNTIME_DECL obj_t bstring_to_symbol( obj_t );
BGL_RUNTIME_DECL obj_t bgl_symbol_genname( obj_t, char * );
   
BGL_RUNTIME_DECL obj_t string_to_keyword( char * );
   
BGL_RUNTIME_DECL obj_t bgl_open_input_string( obj_t, long );
BGL_RUNTIME_DECL obj_t bgl_open_input_substring( obj_t, long, long );
BGL_RUNTIME_DECL obj_t bgl_open_input_substring_bang( obj_t, long, long );
   
BGL_RUNTIME_DECL long bgl_file_size( char * );
BGL_RUNTIME_DECL obj_t create_custom( long );

BGL_RUNTIME_DECL void bgl_input_port_buffer_set( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_reset_output_string_port( obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_display_string( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_display_substring( obj_t, long, long, obj_t );
BGL_RUNTIME_DECL obj_t bgl_display_symbol( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_display_fixnum( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_display_char( char, obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_flush_output_port( obj_t );
BGL_RUNTIME_DECL obj_t bgl_write( obj_t, unsigned char *, size_t );
   
BGL_RUNTIME_DECL obj_t bgl_make_date( BGL_LONGLONG_T, int, int, int, int, int, int, long, bool_t, int );
   
BGL_RUNTIME_DECL obj_t bgl_make_condvar( obj_t );
BGL_RUNTIME_DECL obj_t bgl_make_mutex( obj_t );
BGL_RUNTIME_DECL obj_t bgl_make_spinlock( obj_t );
   
BGL_RUNTIME_DECL bool_t (*bgl_mutex_lock)( obj_t );
BGL_RUNTIME_DECL bool_t (*bgl_mutex_timed_lock)( obj_t, long );
BGL_RUNTIME_DECL bool_t (*bgl_mutex_unlock)( obj_t );
BGL_RUNTIME_DECL obj_t (*bgl_mutex_state)( obj_t );

BGL_RUNTIME_DECL bool_t (*bgl_condvar_wait)( obj_t, obj_t );
BGL_RUNTIME_DECL bool_t (*bgl_condvar_timed_wait)( obj_t, obj_t, long );
BGL_RUNTIME_DECL bool_t (*bgl_condvar_signal)( obj_t );
BGL_RUNTIME_DECL bool_t (*bgl_condvar_broadcast)( obj_t );

BGL_RUNTIME_DECL obj_t bgl_open_mmap( obj_t, bool_t, bool_t );
BGL_RUNTIME_DECL obj_t bgl_string_to_mmap( obj_t, bool_t, bool_t );
BGL_RUNTIME_DECL obj_t bgl_close_mmap( obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_make_class( obj_t, obj_t, long,
				       obj_t, obj_t,
				       obj_t, long,
				       obj_t, obj_t,
				       obj_t, obj_t, obj_t, obj_t, obj_t,
				       long, obj_t );

BGL_RUNTIME_DECL obj_t bgl_getgroups( void );
   
#if !HAVE_MMAP   
BGL_RUNTIME_DECL unsigned char bgl_mmap_nommap_ref( obj_t, long );
BGL_RUNTIME_DECL obj_t bgl_mmap_nommap_set( obj_t, long, unsigned char );
#endif

#define STRTOD( x ) strtod( x, 0L )
   
#if !BGL_HAVE_STRTOLL
BGL_RUNTIME_DECL BGL_LONGLONG_T bgl_strtoll( const char *, char **, int );
BGL_RUNTIME_DECL BGL_LONGLONG_T bgl_strtoull( const char *, char **, int );
#endif

#if( BGL_GC_HAVE_BLOCKING )
BGL_RUNTIME_DECL void (*bgl_gc_start_blocking )( void );
BGL_RUNTIME_DECL void (*bgl_gc_stop_blocking )( void );
#endif
   
#if( BGL_GC_HAVE_DO_BLOCKING )
BGL_RUNTIME_DECL void *(*bgl_gc_do_blocking )( void (*fun)(), void * );
#endif
   
BGL_RUNTIME_DECL obj_t bgl_make_client_socket( obj_t, int, int, obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_make_server_socket( obj_t, int, int, bool_t );
BGL_RUNTIME_DECL obj_t bgl_socket_accept( obj_t, bool_t, obj_t, obj_t );
BGL_RUNTIME_DECL long bgl_socket_accept_many( obj_t, bool_t, obj_t, obj_t, obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_gethostname( void );
BGL_RUNTIME_DECL obj_t bgl_socket_hostname( obj_t );
BGL_RUNTIME_DECL obj_t bgl_datagram_socket_hostname( obj_t );
BGL_RUNTIME_DECL obj_t bgl_getsockopt( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_setsockopt( obj_t, obj_t, obj_t );
   
BGL_RUNTIME_DECL void bgl_init_trace( void );

BGL_RUNTIME_DECL long bgl_rgc_blit_string( obj_t, char *, long, long );

BGL_RUNTIME_DECL void bgl_restore_signal_handlers( void );
extern void bps_bassign(obj_t *field, obj_t value, obj_t obj);
extern void bps_bmassign(obj_t *field, obj_t value);

BGL_RUNTIME_DECL obj_t bgl_make_regexp( obj_t pat );

/* memory profiling */
BGL_RUNTIME_DECL void bmem_set_allocation_type( long, long );

#ifdef __cplusplus
}
#endif

#endif

