/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Include/bigloo.h             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar 16 18:48:21 1995                          */
/*    Last change :  Fri Sep  4 09:34:57 2015 (serrano)                */
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
#define BDB_LIBRARY_MAGIC_NUMBER ((char *)0x1024)
       
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
#define __CNST_FILLER 0.0

/*---------------------------------------------------------------------*/
/*    Constants pool                                                   */
/*    -------------------------------------------------------------    */
/*    See comptile/Cnst/read-alloc.scm                                 */
/*---------------------------------------------------------------------*/
#define CNST_TABLE_SET( offset, value ) \
   ( __cnst[ offset ] = value, BUNSPEC )

#define CNST_TABLE_REF( offset ) __cnst[ offset ]

/*---------------------------------------------------------------------*/
/*    32 bit tagging:                                                  */
/*    -------------------------------------------------------------    */
/*    allocated values:                                                */
/*    +--------+--------+--------+--------+                            */
/*    |....signed fixed point value.....??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    30 bits immediate values (integers):                             */
/*    +--------+--------+--------+--------+                            */
/*    |....signed fixed point value.....??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    6 bits constants (booleans, nil, unspecified, ...):              */
/*    +--------+--------+--------+--------+                            */
/*    |.................|..xxxxxx|mmmmmm??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    8 bits immediate values (chars, int8):                           */
/*    +--------+--------+--------+--------+                            */
/*    |.................|xxxxxxxx|mmmmmm??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    16 bits immediate values (ucs2, int16):                          */
/*    +--------+--------+--------+--------+                            */
/*    |xxxxxxxx|xxxxxxxx|........|mmmmmm??|                            */
/*    +--------+--------+--------+--------+                            */
/*                                                                     */
/*    64 bit tagging:                                                  */
/*    -------------------------------------------------------------    */
/*    pointers (vector, cell, real):                                   */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*    |...........................pointer....................... ???|  */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*                                                                     */
/*    int32:                                                           */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*    |xxxxxxxx|xxxxxxxx|xxxxxxxx|.......|...................... ???|  */
/*    +--------+--------+--------+- ... -+--------+--------+--------+  */
/*                                                                     */
/*---------------------------------------------------------------------*/
#define TAG_SHIFT PTR_ALIGNMENT
#define TAG_MASK ((1 << PTR_ALIGNMENT) - 1)

#define TAG( val, shift, tag ) ((long)(((long)(val) << shift) | tag))
#define UNTAG( val, shift, tag ) ((long)((long)(val) >> shift))

#define BGL_CNST_SHIFT_CHAR 8
#define BGL_CNST_SHIFT_INT16 16
#define BGL_CNST_SHIFT_UCS2 16

#if( PTR_ALIGNMENT >= 3 )
#  define BGL_CNST_SHIFT_INT32 32
#endif
   
/*---------------------------------------------------------------------*/
/*    The tagged pointers ...                                          */
/*---------------------------------------------------------------------*/
#if( BGL_GC == BGL_BOEHM_GC ) 
#   define TAG_STRUCT    0     /*  Pointers tagging         ....00     */
#   define TAG_INT       1     /*  Integers tagging         ....01     */
#   define TAG_CNST      2     /*  Constants tagging        ....10     */
#   define TAG_PAIR      3     /*  Pairs tagging            ....11     */
#else
#   if( BGL_GC == BGL_NO_GC )
#      define TAG_STRUCT 0     /*  Pointers tagging         ....00     */
#      define TAG_INT    1     /*  Integers tagging         ....01     */
#      define TAG_CNST   2     /*  Constants tagging        ....10     */
#      define TAG_PAIR   3     /*  Pairs tagging            ....11     */
#   else
       error "Unknown garbage collector type"
#   endif
#endif

#if( PTR_ALIGNMENT >= 3 )
#   define TAG_VECTOR    4     /*  vector tagging           ...100     */
#   define TAG_CELL      5     /*  Cells tagging            ...101     */
#   define TAG_REAL      6     /*  Reals tagging            ...110     */
#   define TAG_SYMBOL    7     /*  Strings tagging          ...111     */
#endif

#if( TAG_STRUCT != 0 )
#   define POINTERP( o ) ((((long)o) & TAG_MASK) == TAG_STRUCT)
#else
#   define POINTERP( o ) (((((long)o) & TAG_MASK) == TAG_STRUCT) && o)
#endif

#if( TAG_CNST != 0 )
#   define CNSTP( o ) ((((long)o) & TAG_MASK) == TAG_CNST)
#else
#   define CNSTP( o ) ((o) && ((((long)o) & TAG_MASK) == TAG_CNST))
#endif

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
   ((header_t)TAG( (_i), TYPE_SHIFT, (_sz & SIZE_MASK) << HEADER_SHIFT ))

#define HEADER_TYPE( _i ) (long)UNTAG( (_i), TYPE_SHIFT, 0 )

#define HEADER_SIZE( _h ) (((_h) >> HEADER_SHIFT) & SIZE_MASK)

#define TYPE( _o ) HEADER_TYPE( CREF( _o )->header )
       
#define OBJ_SIZE ((long)(sizeof( obj_t )))

#if( TAG_STRUCT == 0 )
#   define BREF( r ) ((obj_t)(r))
#   define CREF( r ) ((obj_t)(r))
#else
#   define BREF( r ) ((obj_t)((long)r + TAG_STRUCT))
#   define CREF( r ) ((obj_t)((long)r - TAG_STRUCT))
#endif

/*---------------------------------------------------------------------*/
/*    Type identifiers ...                                             */
/*---------------------------------------------------------------------*/
#define PAIR_TYPE 0
#if( !defined( TAG_STRING ) )
#  define STRING_TYPE 1
#endif   
#define VECTOR_TYPE 2
#define PROCEDURE_TYPE 3
#define UCS2_STRING_TYPE 4
#define OPAQUE_TYPE 5
#define CUSTOM_TYPE 6
#define KEYWORD_TYPE 7
#if( !defined( TAG_SYMBOL ) )
#  define SYMBOL_TYPE 8
#endif       
#define STACK_TYPE 9
#define INPUT_PORT_TYPE 10
#define OUTPUT_PORT_TYPE 11
#define DATE_TYPE 12
#if( !defined( TAG_CELL ) )
#  define CELL_TYPE 13
#endif   
#define SOCKET_TYPE 14
#define STRUCT_TYPE 15
#if( !defined( TAG_REAL ) )
#  define REAL_TYPE 16
#endif   
#define PROCESS_TYPE 17
#define FOREIGN_TYPE 18
#define OUTPUT_STRING_PORT_TYPE 19
#define BINARY_PORT_TYPE 20
#define EXTENDED_PAIR_TYPE 21
#define TVECTOR_TYPE 22
#define TSTRUCT_TYPE 23
#define PROCEDURE_LIGHT_TYPE 24
#define ELONG_TYPE 25
#define LLONG_TYPE 26
#define MUTEX_TYPE 27
#define CONDVAR_TYPE 28
#define MMAP_TYPE 29
#define S8VECTOR_TYPE 30
#define U8VECTOR_TYPE 31
#define S16VECTOR_TYPE 32
#define U16VECTOR_TYPE 33
#define S32VECTOR_TYPE 34
#define U32VECTOR_TYPE 35
#define S64VECTOR_TYPE 36
#define U64VECTOR_TYPE 37
#define F32VECTOR_TYPE 38
#define F64VECTOR_TYPE 39
#define WEAKPTR_TYPE 40
#define OUTPUT_PROCEDURE_PORT_TYPE 41
#define DYNAMIC_ENV_TYPE 42
#define BIGNUM_TYPE 43
#define DATAGRAM_SOCKET_TYPE 44
#define REGEXP_TYPE 45
#define CLASS_TYPE 46
#if( !defined( BGL_CNST_SHIFT_INT32 ) )
#  define INT32_TYPE 47
#  define UINT32_TYPE 48
#endif
#define INT64_TYPE 49
#define UINT64_TYPE 50
       
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
   union scmobj *name;                 
   union scmobj *location;                 
   struct bgl_dframe *link;
};

/* bigloo polymorphic type */
typedef union scmobj {
   /* integer */
   long integer;
   
   /* common header, repeated for every type allocated object */
   header_t header;

   /* pairs */
   struct pair {
#if( !(defined( TAG_PAIR )) )
      /* the header, unless pairs are tagged */
      header_t header;
#endif      
      union scmobj *car;
      union scmobj *cdr;
   } pair_t;

   /* extended pairs */
   struct extended_pair {
#if( !(defined( TAG_PAIR )) )
      header_t header; 
#endif 
      union scmobj *car;       
      union scmobj *cdr;
      /* extended header type */
      union scmobj *eheader;
      /* extended slot */
      union scmobj *cer;
   } extended_pair_t;                     

   /* strings */
   struct string {
#if( !defined( TAG_STRING ) )
      header_t header;
#endif		
      long length;
      unsigned char char0[ 1 ];
   } string_t;

   /* ucs2/utf16 strings */
   struct ucs2_string {
      header_t header;
      long length;
      ucs2_t char0;
   } ucs2_string_t; 

   /* vectors */
   struct vector {
#if( !defined( TAG_VECTOR ) )
      header_t header;
#endif
      /* 24 bit long length (see VECTOR_LENGTH) */
      long length;
      union scmobj *obj0;
   } vector_t;             

   /* typed vectors */
   struct tvector {
      header_t header;
      long length;
      union scmobj *descr;
   } tvector_t;

   /* procedure (closures) */
   struct procedure {
      header_t header;    
      union scmobj *(*entry)();
      union scmobj *(*va_entry)();
      union scmobj *attr;
      int arity;
      union scmobj *obj0;
   } procedure_t;

   /* light procedures (results of the CFA optimization) */
   struct procedure_light {
      union scmobj *(*entry)();
      union scmobj  *obj0;
   } procedure_light_t;

   /* symbols and keywords */
   struct symbol {
#if( !defined( TAG_SYMBOL ) )
      header_t header;
#endif      
      union scmobj *string;
      union scmobj *cval;
   } symbol_t;
   
   struct keyword {
      header_t header;
      union scmobj *string;
      union scmobj *cval;
   } keyword_t;
   
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
   } port_t;

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
   } output_port_t;

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
   } input_port_t;

   /* string input ports */
   struct input_string_port {
      /* common port */
      struct input_port iport;
      /* offset */
      long offset;
   } input_string_port_t;
	 
   /* procedure input ports */
   struct input_procedure_port {
      /* common port */
      struct input_port iport;
      /* buffer used to store procedure results */
      union scmobj *pbuffer;
      /* index in pbuffer */
      long pbufpos;
   } input_procedure_port_t;
   
   /* gzipped input ports */
   struct input_gzip_port {
      /* common port */
      struct input_procedure_port iport;
      /* gzipped port */
      union scmobj *gzip;
   } input_gzip_port_t;

   /* binary ports */
   struct binary_port {
      header_t header;
      /* file name */
      union scmobj *name;
      /* OS file */
      FILE *file;
      /* type 0=input, 1=output, 2=close */
      int io;
   } binary_port_t;
   
   /* cells (compiler and user values) */	
   struct cell {
#if( !defined( TAG_CELL ) )
      header_t header;
#endif
      /* the value pointed to by the cell */
      union scmobj *val;        
   } cell_t;

   /* structures */
   struct structure {
#if( !defined( TAG_STRUCTURE ) )
      header_t header;
#endif
      /* the key (i.e., the name) of the structure */
      union scmobj *key;
      /* the length (i.e., the slot number) */
      long length;
      /* the packed slots */
      union scmobj *obj0;
   } struct_t;
   
   /* floating point numbers */
   struct real {
#if( !defined( TAG_REAL ) )
      header_t header;
#endif		
      double real;      
   } real_t;                     

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
   } stack_t;
   
   /* boxed foreign values */
   struct foreign {
      header_t header;    
      union scmobj *id;
      void *cobj;
   } foreign_t;
   
   /* exact longs (i.e., boxed C longs) */
   struct elong {
      header_t header;
      long elong;
   } elong_t;

   /* long longs */
   struct llong {
      header_t header;
      BGL_LONGLONG_T llong;
   } llong_t;

#if( !defined( BGL_CNST_SHIFT_INT32 ) )
   /* sint32 */
   struct bgl_sint32 {
      header_t header;
      int32_t val;
   } sint32_t;
      
   /* uint32 */
   struct bgl_uint32 {
      header_t header;
      uint32_t val;
   } uint32_t;
#endif
   
   /* sint64 */
   struct bgl_sint64 {
      header_t header;
      int64_t val;
   } sint64_t;
      
   /* uint64 */
   struct bgl_uint64 {
      header_t header;
      uint64_t val;
   } uint64_t;
   
   /* arbitrary precision integers */
   struct bignum {
      header_t header;
#if( BGL_HAVE_GMP )
      /* from gmp.h */
      __mpz_struct mpz;
#else
      union scmobj *u16vect;
#endif
   } bignum_t;

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
   } process_t;

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
   } socket_t;

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
   } datagram_socket_t;

   /* regular expressions */
   struct bgl_regexp {
      header_t header;
      /* string source */
      union scmobj *pat;
#if( BGL_REGEXP_TYPE == BGL_REGEXP_regex )
      /* posix regular expression */
      regex_t preg;
#else
#  if( BGL_REGEXP_TYPE == BGL_REGEXP_pcre )
      /* pcre regular expression */
      void *preg;
      void *study;
      int capturecount;
#  else
      union scmobj *preg;
#  endif
#endif      
   } regexp_t;

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
   } custom_t;

   /* dates */
   struct bgl_date {
      header_t header;
      /* number of nanoRseconds */
      BGL_LONGLONG_T nsec;
      /* number of seconds [0..59] */
      int sec;
      /* number of minutes [0..59] */
      int min;
      /* number of hour [0..23] */
      int hour;
      /* day of month [0..30] */
      int mday;
      /* month number [0..11] */
      int mon;
      /* year number [0..20xx] */
      int year;
      /* day of week [0..6] */ 
      int wday;
      /* day of year [0..365] */
      int yday;
      /* number of seconds of timezone */
      long timezone;
      /* daylight savings? [-1/0/1] */   
      int isdst;                 
   } date_t;

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
   } mutex_t;

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
   } condvar_t;

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
   } mmap_t;
   
   /* srfi4 homovectors */
   struct bgl_hvector {
      header_t header;
      unsigned int length;
   } hvector_t;
   
   /* weak pointers */
   struct bgl_weakptr {
      header_t header;
      union scmobj *data;
   } weakptr_t;

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
   } class_t;

   /* Thread dynamic environment */
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
      /* user per thread data */
      union scmobj *user_data;
   } dynamic_env_t;

} *obj_t;

/* function type */
typedef obj_t (*function_t)();

/* old name mangling framework */
typedef struct __object_bgl {
   header_t header;
   obj_t widening;
} *object_bglt;

/* bootstrap configuration */   
typedef struct __bgl__object_00_bgl {
   header_t header;
   obj_t widening;
} *bgl__object_00_bglt;
   
/* new name mangling framework */   
typedef struct BgL__object_00_bgl {
   header_t header;
   obj_t widening;
} *BgL__object_00_bglt;
   
typedef struct BgL_objectz00_bgl {
   header_t header;
   obj_t widening;
} *BgL_objectz00_bglt;

/*---------------------------------------------------------------------*/
/*    The garbage collector                                            */
/*---------------------------------------------------------------------*/
#include <bigloo_gc.h>       

/*---------------------------------------------------------------------*/
/*    Constants                                                        */
/*---------------------------------------------------------------------*/
#define BCNST( c ) (obj_t)TAG( c, TAG_SHIFT, TAG_CNST )
#define CCNST( c ) (long)UNTAG( c, TAG_SHIFT, TAG_CNST )

#define BNIL BCNST( 0 )
#define BUNSPEC BCNST( 3 )
   
#define BFALSE BCNST( 1 )
#define BTRUE BCNST( 2 )
   
#define BEOF BCNST( 0x100 )
#define BEOA BCNST( 0x101 )
   
#define BOPTIONAL BCNST( 0x102 )
#define BREST BCNST( 0x103 )
#define BKEY BCNST( 0x106 )

#define BCHARH ((long)BCNST( 4 ))
#define BUCS2H ((long)BCNST( 5 ))

#define BINT8H ((long)BCNST( 6 ))
#define BUINT8H ((long)BCNST( 7 ))

#define BINT16H ((long)BCNST( 8 ))
#define BUINT16H ((long)BCNST( 9 ))

#if( PTR_ALIGNMENT >= 3 )
#  define BINT32H ((long)BCNST( 10 ))
#  define BUINT32H ((long)BCNST( 11 ))
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
   (((long)(o) & (long)((1 << (BGL_CNST_SHIFT_CHAR)) -1)) == (long)BCHARH)

#define BCHAR( c ) \
   ((obj_t)(BCHARH + ((unsigned char)(c) << BGL_CNST_SHIFT_CHAR)))
#define CCHAR( o ) \
   ((unsigned char)((unsigned long)(o) >> BGL_CNST_SHIFT_CHAR))

/*---------------------------------------------------------------------*/
/*    UCS2/UTF16 characters                                            */
/*---------------------------------------------------------------------*/
#define UCS2P( o ) \
   (((long)(o) & (long)((1 << (BGL_CNST_SHIFT_UCS2)) -1)) == (long)BUCS2H)

#define BUCS2( u ) \
   ((obj_t)(BUCS2H + ((long)((ucs2_t)(u) << BGL_CNST_SHIFT_UCS2))))
#define CUCS2( o ) \
   ((ucs2_t)((unsigned long)(o) >> BGL_CNST_SHIFT_UCS2))

#define BGL_INT_TO_UCS2( _i ) ((ucs2_t)(_i))

/*---------------------------------------------------------------------*/
/*    Integers                                                         */
/*---------------------------------------------------------------------*/
#define INTEGERP( o ) ((((long)o) & TAG_MASK) == TAG_INT)

#define BINT( i ) (obj_t)TAG( i, TAG_SHIFT, TAG_INT )
#define CINT( o ) (long)UNTAG( o, TAG_SHIFT, TAG_INT )

#define ODDP_FX( i )  ((i) & 0x1 )
#define EVENP_FX( i ) (!ODDP_FX( i ))

/*---------------------------------------------------------------------*/
/*    Pairs                                                            */
/*---------------------------------------------------------------------*/
#if( defined( TAG_PAIR ) )
#   define BPAIR( p ) ((obj_t)((long)p + TAG_PAIR))
#   define CPAIR( p ) ((obj_t)((long)p - TAG_PAIR))
#   if( TAG_PAIR == 0 )
#      define PAIRP( c ) ((c && ((((long)c) & TAG_MASK) == TAG_PAIR)))
#   else
#      define PAIRP( c ) ((((long)c) & TAG_MASK) == TAG_PAIR)
#   endif
#else
#   define BPAIR( p ) BREF( p )
#   define CPAIR( p ) CREF( p )
#   define PAIRP( c ) (POINTERP( c ) && (TYPE( c ) == PAIR_TYPE))
#endif

#define PAIR( o ) (CPAIR( o )->pair_t)
#define EPAIR( o ) (CPAIR( o )->extended_pair_t)

#define PAIR_SIZE (sizeof( struct pair ))
#define EXTENDED_PAIR_SIZE (sizeof( struct extended_pair ))

#if( (BGL_GC == BGL_BOEHM_GC) && BGL_GC_CUSTOM )
#   define MAKE_PAIR( a, d ) make_pair( a, d )
#   define MAKE_EXTENDED_PAIR( a, d, e ) make_extended_pair( a, d, e )
#else
#   if( defined( TAG_PAIR ) )
#     if( defined( __GNUC__ ) )
#      define MAKE_PAIR( a, d ) \
        ({ obj_t an_object; \
           an_object = GC_MALLOC( PAIR_SIZE ); \
	   an_object->pair_t.car = a; \
	   an_object->pair_t.cdr = d; \
           ( BPAIR( an_object ) ); }) 
#      define MAKE_EXTENDED_PAIR( a, d, e ) \
        ( { obj_t an_object; \
	    an_object = GC_MALLOC( EXTENDED_PAIR_SIZE ); \
   	    an_object->extended_pair_t.car = a; \
	    an_object->extended_pair_t.cdr = d; \
	    an_object->extended_pair_t.cer = e; \
	    an_object->extended_pair_t.eheader = BINT( EXTENDED_PAIR_TYPE ); \
	    ( BPAIR( an_object ) ); } )
#     else
#      define MAKE_PAIR( a, d ) make_pair( a, d )
#      define MAKE_EXTENDED_PAIR( a, d, e ) make_extended_pair( a, d ,e )
#     endif
#   else
#     if( defined( __GNUC__ ) )
#      define MAKE_PAIR( a, d ) \
        ({ obj_t an_object; \
           an_object = GC_MALLOC( PAIR_SIZE ); \
   	   an_object->pair_t.header = MAKE_HEADER( PAIR_TYPE, 0 ); \
	   an_object->pair_t.car = a; \
	   an_object->pair_t.cdr = d; \
           ( BPAIR( an_object ) ); }) 
#      define MAKE_EXTENDED_PAIR( a, d, e ) \
        ( { obj_t an_object; \
	    an_object = GC_MALLOC( EXTENDED_PAIR_SIZE ); \
	    an_object->extended_pair_t.header  = MAKE_HEADER( PAIR_TYPE,0 ); \
	    an_object->extended_pair_t.car = a; \
	    an_object->extended_pair_t.cdr = d; \
  	    an_object->extended_pair_t.cer = e; \
	    an_object->extended_pair_t.eheader = BINT( EXTENDED_PAIR_TYPE ); \
	    ( BPAIR( an_object ) ); } )
#     else
#      define MAKE_PAIR( a, d ) make_pair( a, d )
#      define MAKE_EXTENDED_PAIR( a, d, e ) make_extended_pair( a, d ,e )
#     endif
#   endif
#endif

#if( BGL_GC == BGL_BOEHM_GC )
#   define EXTENDED_PAIRP( c ) \
      ( PAIRP( c ) && \
        (((long)GC_size( BPAIR( c ) )) >= EXTENDED_PAIR_SIZE) && \
        (EPAIR( c ).eheader == BINT( EXTENDED_PAIR_TYPE ) ) )
#else
#   define EXTENDED_PAIRP( c ) \
      ( PAIRP( c ) && (EPAIR( c ).eheader == BINT( EXTENDED_PAIR_TYPE ) ) )
#endif

#define NULLP( c ) ((long)(c) == (long)BNIL)

#define CAR( c ) (PAIR( c ).car)
#define CDR( c ) (PAIR( c ).cdr)
#define CER( c ) (EPAIR( c ).cer)

#define SET_CAR( c, v ) ((CAR(c) = v), BUNSPEC)
#define SET_CDR( c, v ) ((CDR(c) = v), BUNSPEC)
#define SET_CER( c, v ) ((CER(c) = v), BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Cells                                                            */
/*---------------------------------------------------------------------*/
#if( defined( TAG_CELL ) )
#   define BCELL( p ) ((obj_t)((long)p + TAG_CELL))
#   define CCELL( p ) ((obj_t)((long)p - TAG_CELL))
#   define CELLP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_CELL)))
#else
#   define BCELL( p ) BREF( p )
#   define CCELL( p ) CREF( p )
#   define CELLP( c ) (POINTERP( c ) && (TYPE( c ) == CELL_TYPE))
#endif

#define CELL( o ) CCELL( o )->cell_t

#define CELL_SIZE (sizeof( struct cell ))

#if( (BGL_GC == BGL_BOEHM_GC) && BGL_GC_CUSTOM )
#   define MAKE_CELL( v ) make_cell( v )
#else
#  if( defined( TAG_CELL ) )
#     if( defined( __GNUC__ ) )
#        define MAKE_CELL( _val_ ) \
         ( { obj_t an_object; \
	     an_object = GC_MALLOC( CELL_SIZE ); \
  	     an_object->cell_t.val = (_val_); \
	     ( BCELL( an_object ) ); } )
#     else
#        define MAKE_CELL( _val_ ) make_cell( _val_ )
#     endif

#  else
#     if( defined( __GNUC__ ) )
#        define MAKE_CELL( _val_ ) \
         ( { obj_t an_object; \
	     an_object = GC_MALLOC( CELL_SIZE ); \
	     an_object->cell_t.header = MAKE_HEADER( CELL_TYPE, 0 ); \
  	     an_object->cell_t.val = (_val_); \
	     ( BCELL( an_object ) ); } )
#     else
#        define MAKE_CELL( _val_ ) make_cell( _val_ )
#     endif
#  endif
#endif		 

#define CELL_REF( c ) ((CCELL( c )->cell_t).val)
#define CELL_SET( c, v ) (CELL_REF( c ) = v, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Strings                                                          */
/*---------------------------------------------------------------------*/
/* When producing C code for a compiler that is unable to    */
/* accept large splitted string, Bigloo emits a declaration  */
/* of a C characters array. This requires 2 macros, one for  */
/* starting the declaration and one for ending it. The       */
/* array itself, is inserted in between the two macros by    */
/* bigloo such as:                                           */
/*        DEFINE_STRING_START( f, a, 2 ),                    */
/*          {45,46,0},                                       */
/*        DEFINE_STRING_STOP( f, a, 2 );                     */
#if( defined( TAG_STRING ) )
#   define BSTRING( p ) ((obj_t)((long)p + TAG_STRING))
#   define CSTRING( p ) ((obj_t)((long)p - TAG_STRING))
#   define DEFINE_STRING( name, aux, str, len ) \
      static struct { __CNST_ALIGN long length; \
                      char string[ len + 1 ]; } \
         aux = { __CNST_FILLER, len, str }; \
         static obj_t name = BSTRING( &(aux.length) )
#   define DEFINE_STRING_START( name, aux, len ) \
      static struct { __CNST_ALIGN long length; \
                      char string[ len + 1 ]; } \
         aux = { __CNST_FILLER, len
#   define DEFINE_STRING_STOP( name, aux ) \
        }; static obj_t name = BSTRING( &(aux.length) 
#   define STRINGP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_STRING)))
#else
#   define BSTRING( p ) BREF( p )
#   define CSTRING( p ) CREF( p )
#   define DEFINE_STRING( name, aux, str, len ) \
      static struct { __CNST_ALIGN header_t header; \
                      long length; \
                      char string[ len + 1 ]; } \
         aux = { __CNST_FILLER, MAKE_HEADER( STRING_TYPE, 0 ), len, str }; \
         static obj_t name = BSTRING( &(aux.header) )
#   define DEFINE_STRING_START( name, aux, len ) \
      static struct { __CNST_ALIGN header_t header; \
                      long length; \
                      char string[ len + 1]; } \
         aux = { __CNST_FILLER, MAKE_HEADER( STRING_TYPE, 0 ), len 
#   define DEFINE_STRING_STOP( name, aux ) \
        }; static obj_t name = BSTRING( &(aux.header) )
#   define STRINGP( c ) (POINTERP( c ) && (TYPE( c ) == STRING_TYPE))
#endif

#define STRING( o ) (CSTRING( o )->string_t)

#define STRING_SIZE (sizeof( struct string ))

#define STRING_LENGTH( s ) STRING( s ).length
#define INVERSE_STRING_LENGTH( s ) \
   ((STRING_LENGTH( s ) = (-STRING_LENGTH( s ))), BUNSPEC)
   
#define BSTRING_TO_STRING( s ) ((char *)(&(STRING( s ).char0)))

#define STRING_REF( v, i ) (((unsigned char *)BSTRING_TO_STRING( v ))[ i ])
#define STRING_SET( s, i, c ) (STRING_REF( s, i ) = c, BUNSPEC)
#define STRING_PTR_NULL( _p ) (_p == 0)

#if( defined( TAG_STRING ) )
#  define STRING_ASCII_SENTINEL( s ) 0
#  define STRING_ASCII_SENTINEL_SET( s, i ) s
#else
#  define STRING_ASCII_SENTINEL( s ) \
     HEADER_SIZE( CREF( s )->header )
#  define STRING_ASCII_SENTINEL_SET( s, i ) \
   (CREF( s )->header = MAKE_HEADER( STRING_TYPE, i ), s)
#endif   
   
/*---------------------------------------------------------------------*/
/*    UCS2/UTF16 Strings                                               */
/*---------------------------------------------------------------------*/
#define BUCS2STRING( p ) BREF( p )
#define CUCS2STRING( p ) CREF( p )

#define UCS2_STRINGP( c ) (POINTERP( c ) && (TYPE( c ) == UCS2_STRING_TYPE))

#define UCS2_STRING( o )  (CUCS2STRING( o )->ucs2_string_t)

#define UCS2_STRING_SIZE (sizeof( struct ucs2_string ))

#define UCS2_STRING_LENGTH( s ) UCS2_STRING( s ).length
#define INVERSE_UCS2_STRING_LENGTH( s ) \
   ((UCS2_STRING_LENGTH( s ) = (-UCS2_STRING_LENGTH( s ))), BUNSPEC)

#define BUCS2_STRING_TO_UCS2_STRING( s ) (&(UCS2_STRING( s ).char0))

#define UCS2_STRING_REF( v, i ) (BUCS2_STRING_TO_UCS2_STRING( v )[ i ])
#define UCS2_STRING_SET( s, i, c ) (UCS2_STRING_REF( s, i ) = c, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Regular procedures                                               */
/*---------------------------------------------------------------------*/
#define DEFINE_EXPORT_BGL_PROCEDURE( n, na, p, vp, at, nb_args ) \
   static struct { __CNST_ALIGN header_t header; \
                   obj_t (*entry)(); \
                   obj_t (*va_entry)(); \
                   obj_t attr; \
                   int arity; } \
      na = { __CNST_FILLER, MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
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
      na = { __CNST_FILLER, MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
             (obj_t (*)())p, \
	     (obj_t (*)())vp, \
             at, \
	     nb_args }; \
      static const obj_t n = BREF( &(na.header) )

#define PROCEDURE_SIZE (sizeof( struct procedure ))

#define PROCEDURE( o ) CREF( o )->procedure_t
   
#define PROCEDURE_ENTRY( fun ) (obj_t)(PROCEDURE( fun ).entry)
#define PROCEDURE_VA_ENTRY( fun ) (obj_t)(PROCEDURE( fun ).va_entry)

#define PROCEDUREP( fun ) \
   (POINTERP( fun ) && (TYPE( fun ) == PROCEDURE_TYPE))

#define PROCEDURE_ARITY( fun ) (PROCEDURE( fun ).arity)
#define PROCEDURE_LENGTH( fun ) (HEADER_SIZE( BREF( fun )->header ))
   
#define PROCEDURE_ATTR( fun ) (PROCEDURE( fun ).attr)
#define PROCEDURE_ATTR_SET( fun, _v ) (PROCEDURE( fun ).attr = (_v), (_v))

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
#define PROCEDURE_SET( p, i, o ) (PROCEDURE_REF( p, i ) = o, BUNSPEC)

#define MAKE_FX_PROCEDURE( entry, arity, size ) \
   make_fx_procedure( (function_t)entry, arity, size )

#define MAKE_VA_PROCEDURE( entry, arity, size ) \
   make_va_procedure( (function_t)entry, arity, size )

/*---------------------------------------------------------------------*/
/*    Light procedures                                                 */
/*---------------------------------------------------------------------*/
#define DEFINE_BGL_L_PROCEDURE( n, na, e ) \
   static const struct { __CNST_ALIGN ; \
                   union scmobj *(*entry)(); } \
      na = { __CNST_FILLER, (obj_t (*)())e }; \
      static const obj_t n = BLIGHT( &(na.entry) )
   
#define BLIGHT( l ) BPAIR( l )
#define CLIGHT( l ) CPAIR( l )

#define PROCEDURE_L_SIZE (sizeof( struct procedure_light ))

#define PROCEDURE_L( _o_ ) (CLIGHT( _o_ )->procedure_light_t)

#define PROCEDURE_L_ENTRY( fun ) (PROCEDURE_L( fun ).entry)

#define PROCEDURE_L_ENV( fun ) (&(PROCEDURE_L( fun ).obj0))

#define PROCEDURE_L_REF( p, _i )    PROCEDURE_L_ENV( p )[ _i ] 
#define PROCEDURE_L_SET( p, _i, o ) (PROCEDURE_L_REF( p, _i ) = o, BUNSPEC)

#if( defined( __GNUC__ ) )
#   define MAKE_L_PROCEDURE_ALLOC( ALLOC, _entry, _size ) \
      ( { obj_t an_object; \
	  an_object = ALLOC( PROCEDURE_L_SIZE + ((_size-1) * OBJ_SIZE) ); \
	  (an_object->procedure_light_t).entry = _entry; \
          ( BLIGHT( an_object ) ); } )
#else
#   define MAKE_L_PROCEDURE_ALLOC( ALLOC, _entry, _size ) \
      (   an_object = ALLOC( PROCEDURE_L_SIZE + ((_size-1) * OBJ_SIZE) ), \
	  (an_object->procedure_light_t).entry = _entry, \
          ( BLIGHT( an_object ) ) )
#endif

#define MAKE_L_PROCEDURE( _entry, _size )   \
   MAKE_L_PROCEDURE_ALLOC( GC_MALLOC, ((obj_t (*)())_entry), _size )

/*---------------------------------------------------------------------*/
/*    Extra-light procedures                                           */
/*---------------------------------------------------------------------*/
#define MAKE_EL_PROCEDURE( size ) \
   (( !size ) ? BUNSPEC : GC_MALLOC( size * OBJ_SIZE ))

#define PROCEDURE_EL_REF( p, i ) ((obj_t *)p)[ i ]
#define PROCEDURE_EL_SET( p, i, o ) (PROCEDURE_EL_REF( p, i ) = o, BUNSPEC)

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
      na = { __CNST_FILLER, MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
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
      na = { __CNST_FILLER, MAKE_HEADER( PROCEDURE_TYPE, 0 ), \
             (obj_t (*)())p, \
	     (obj_t (*)())vp, \
             at, \
	     nb_args, \
	     BFALSE, \
	     BFALSE, \
	     BUNSPEC }; \
      static const obj_t n = BREF( &(na.header) )

/*---------------------------------------------------------------------*/
/*    Vectors                                                          */
/*---------------------------------------------------------------------*/
#if( defined( TAG_VECTOR ) )
#   define BVECTOR( p ) ((obj_t)((long)p + TAG_VECTOR))
#   define CVECTOR( p ) ((obj_t)((long)p - TAG_VECTOR))
#   define VECTORP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_VECTOR)))
#else
#   define BVECTOR( p ) BREF( p )
#   define CVECTOR( p ) CREF( p )
#   define VECTORP( c ) (POINTERP( c ) && (TYPE( c ) == VECTOR_TYPE))
#endif

#define VECTOR( o ) CVECTOR( o )->vector_t

#define VECTOR_SIZE (sizeof( struct vector ))

#define VECTOR_TAG_NB_BIT 8
#define VECTOR_TAG_SIZE ((unsigned int)(1<<VECTOR_TAG_NB_BIT))

#define VECTOR_LENGTH_SHIFT ((sizeof( int ) << 3) - VECTOR_TAG_NB_BIT)

#define VECTOR_LENGTH_MASK \
   (~(unsigned int)((VECTOR_TAG_SIZE -1) << VECTOR_LENGTH_SHIFT))

#define FREE_VECTOR_UNCOLLECTABLE( v ) GC_FREE( CVECTOR( v ) )
   
#define VECTOR_REF( v, i ) (&(VECTOR( v ).obj0))[ i ]
#define VECTOR_SET( v, i, o ) (VECTOR_REF( v, i ) = o, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Typed vectors                                                    */
/*---------------------------------------------------------------------*/
#define DEFINE_TVECTOR_START( aux, len, itype ) \
   static struct { __CNST_ALIGN header_t header; \
		   int length; \
		   obj_t descr; \
		   itype items[ len ]; } \
      aux = { __CNST_FILLER, MAKE_HEADER( TVECTOR_TYPE, 0 ), len, 0L,
	      
#define DEFINE_TVECTOR_STOP( name, aux ) \
	   }; static obj_t name = BREF( &(aux.header) )

#ifdef __GNUC__
# define ALLOCATE_TVECTOR_MALLOC( MALLOC, _item_name, _item_type, _len, _descr )   \
    ({obj_t an_object;                                                 \
      an_object = MALLOC(sizeof(struct bgl_tvector_of_##_item_name)    \
                         +                                             \
                         ((_len-1) * sizeof(_item_type))),             \
     (an_object->tvector_t).header = MAKE_HEADER( TVECTOR_TYPE, 0 ),   \
     (an_object->tvector_t).length = _len,                             \
     (an_object->tvector_t).descr = _descr,                            \
       ( BREF( an_object ) ); })
#else
# define ALLOCATE_TVECTOR_MALLOC( MALLOC, _item_name, _item_type, _len, _descr )   \
    (an_object = MALLOC(sizeof(struct bgl_tvector_of_##_item_name)     \
                        +                                              \
                        ((_len-1) * sizeof(_item_type))),              \
    (an_object->tvector_t).header = MAKE_HEADER( TVECTOR_TYPE, 0 ),    \
    (an_object->tvector_t).length = _len,                              \
    (an_object->tvector_t).descr = _descr,                             \
       ( BREF( an_object ) ) )
#endif

#define ALLOCATE_TVECTOR( _item_name, _item_type, _len, _descr )   \
   ALLOCATE_TVECTOR_MALLOC( GC_MALLOC, _item_name, _item_type, _len, _descr )
#define ALLOCATE_ATOMIC_TVECTOR( _item_name, _item_type, _len, _descr )   \
   ALLOCATE_TVECTOR_MALLOC( GC_MALLOC_ATOMIC, _item_name, _item_type, _len, _descr )

#define TVECTORP( o ) (POINTERP( o ) && (TYPE( o ) == TVECTOR_TYPE))

#define TVECTOR( tv ) CREF( tv )->tvector_t

#define TVECTOR_SIZE (sizeof( struct tvector ))

#define TVECTOR_ID( tv ) TVECTOR( tv ).id
#define TVECTOR_ID_SET( tv, _id_ ) (TVECTOR_ID( tv ) = _id_, BUNSPEC)

#define TVECTOR_LENGTH( tv ) TVECTOR( tv ).length
#define TVECTOR_DESCR( tv ) TVECTOR( tv ).descr
#define TVECTOR_DESCR_SET( tv, _d_ ) (TVECTOR_DESCR( tv ) = _d_, BUNSPEC)

#define TVECTOR_REF( it, tv, o ) \
      (&(((struct bgl_tvector_of_##it *) \
       CREF( tv ))->el0))[ o ]
   
#define TVECTOR_SET( it, tv, o, v ) \
     (TVECTOR_REF( it, tv, o ) = (v), BUNSPEC)

#define VECTOR_LENGTH( v ) \
   ((unsigned int)VECTOR( v ).length & VECTOR_LENGTH_MASK)

#define VECTOR_TAG_SET( v, tag ) \
    ( VECTOR( v ).length = \
     ((unsigned int)VECTOR_LENGTH( v ) | \
       (((unsigned int) tag) << VECTOR_LENGTH_SHIFT)), \
       BUNSPEC )

#define VECTOR_TAG( v ) \
   ( ((unsigned int)(VECTOR( v ).length) & ~VECTOR_LENGTH_MASK) >> VECTOR_LENGTH_SHIFT )

/*---------------------------------------------------------------------*/
/*    Structures                                                       */
/*---------------------------------------------------------------------*/
#if( defined( TAG_STRUCTURE ) )
#   define BSTRUCTURE( r ) ((obj_t)((long)p + TAG_STRUCTURE))
#   define CSTRUCTURE( p ) ((obj_t)((long)p - TAG_STRUCTURE))
#   define STRUCTP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_STRUCTURE)))
#else
#   define BSTRUCTURE( p ) BREF( p )
#   define CSTRUCTURE( p ) CREF( p )
#   define STRUCTP( c ) (POINTERP( c ) && (TYPE( c ) == STRUCT_TYPE))
#endif

#define STRUCT( o ) CSTRUCTURE( o )->struct_t

#define STRUCT_SIZE (sizeof( struct structure ))

#define STRUCT_KEY( c ) STRUCT( c ).key
#define STRUCT_KEY_SET( c, k ) (STRUCT_KEY( c ) = k, BUNSPEC)

#define STRUCT_LENGTH( c ) STRUCT( c ).length
   
#define STRUCT_REF( c, i ) (&(STRUCT(c).obj0))[ i ]
#define STRUCT_SET( c, i, o ) (STRUCT_REF( c, i ) = o, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Typed structures                                                 */
/*---------------------------------------------------------------------*/
#define TSTRUCTP( o ) (POINTERP( o ) && (TYPE( o ) == TSTRUCT_TYPE))

#define TSTRUCT( tv ) CREF( tv )->tstruct_t

#define TSTRUCT_SIZE (sizeof( struct tstructure ))

#define TSTRUCT_ID( c ) TSTRUCT( c ).id
#define TSTRUCT_TO_VECTOR( c ) \
   (PROCEDURE_ENTRY( TSTRUCT( c ).to_vector ) \
    ( TSTRUCT( c ).to_vector, c, BEOA ))

#define TSTRUCT_REF( _u_struct, _st, _u_slot ) \
      (((struct { header_t header; \
		  obj_t id; \
		  obj_t to_v; \
		  _u_struct dummy;} *)CREF( _st ))->dummy._u_slot)

#define TSTRUCT_SET( _u_struct, _st, _u_slot, _v ) \
      (TSTRUCT_REF( _u_struct, _st, _u_slot ) = _v, \
       BUNSPEC )

/*---------------------------------------------------------------------*/
/*    Symbols                                                          */
/*---------------------------------------------------------------------*/
#if( defined( TAG_SYMBOL ) )
#   define SYMBOLP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_SYMBOL)))
#   define BSYMBOL( p ) ((obj_t)((long)p + TAG_SYMBOL))
#   define CSYMBOL( p ) ((obj_t)((long)p - TAG_SYMBOL))
#else   
#   define SYMBOLP( o ) (POINTERP( o ) && (TYPE( o ) == SYMBOL_TYPE))
#   define BSYMBOL( p ) BREF( p )
#   define CSYMBOL( p ) CREF( p )
#endif   

#define SYMBOL( o ) (CSYMBOL( o )->symbol_t)
   
#define SYMBOL_SIZE (sizeof( struct symbol ))

#define SYMBOL_TO_STRING( o ) \
   (SYMBOL( o ).string ? SYMBOL( o ).string : bgl_symbol_genname( o, "g" ) )

#define GET_SYMBOL_PLIST( o ) (SYMBOL( o ).cval)

#define SET_SYMBOL_PLIST( o, v ) (GET_SYMBOL_PLIST( o ) = v, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Keywords                                                         */
/*---------------------------------------------------------------------*/
#define KEYWORDP( o ) (POINTERP( o ) && (TYPE( o ) == KEYWORD_TYPE))

#define KEYWORD( o ) (CREF( o )->keyword_t)
   
#define KEYWORD_SIZE (sizeof( struct keyword ))

#define KEYWORD_TO_STRING( o ) KEYWORD( o ).string

#define GET_KEYWORD_PLIST( o ) (KEYWORD( o ).cval)

#define SET_KEYWORD_PLIST( o, v ) (GET_KEYWORD_PLIST( o ) = v, BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Reals                                                            */
/*---------------------------------------------------------------------*/
#if( defined( TAG_REAL ) )
#   define BREAL( p ) ((obj_t)((long)p + TAG_REAL))
#   define CREAL( p ) ((obj_t)((long)p - TAG_REAL))
#   define DEFINE_REAL( name, aux, flonum ) \
      static struct { double real; } \
         const aux = { flonum }; \
         const obj_t name = BREAL( &aux )
#   define REALP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_REAL)))
#else
#   define BREAL( p ) BREF( p )
#   define CREAL( p ) CREF( p )
#   define DEFINE_REAL( name, aux, flonum ) \
      static struct { __CNST_ALIGN header_t header; \
		      double real; } \
         const aux = { __CNST_FILLER, MAKE_HEADER( REAL_TYPE, 0 ), flonum }; \
         const obj_t name = BREAL( &(aux.header) )
#   define REALP( c ) (POINTERP( c ) && (TYPE( c ) == REAL_TYPE))
#endif

#define REAL( o ) (CREAL( o )->real_t)

#define REAL_SIZE  (sizeof( struct real ))
   
#define NEG( x ) (- (x))

#define DOUBLE_TO_REAL( d ) (make_real( d ))
#define REAL_TO_DOUBLE( r ) (REAL( r ).real)

#define FLOAT_TO_REAL( d ) (DOUBLE_TO_REAL( (double)(d) ))
#define REAL_TO_FLOAT( r ) ((float)(REAL( r ).real))

#define __DOUBLE_TO_LLONG_BITS( dd ) \
  double ddd = dd; \
  BGL_LONGLONG_T result; \
  memcpy( &result, &ddd, sizeof( result ) ); \
  result;

#define __LLONG_BITS_TO_DOUBLE( ll ) \
  BGL_LONGLONG_T lll = ll; \
  double result; \
  memcpy( &result, &lll, sizeof( result ) ); \
  result;

/* If ints are bigger than floats (which can happen on 64bit machines) */
/* then * we only want to set the least significant bytes of the int.  */
#define __FLOAT_TO_INT_BITS( ff ) \
  float fff = ff; \
  int result = 0; \
  int offset = sizeof( int ) - sizeof( float ); \
  memcpy( ((char*)&result) + offset, &fff, sizeof( float ) ); \
  result;

/* If ints are bigger than floats (which can happen on 64bit machines) */
/* then we only want to set the least significant bytes of the int.    */
#define __INT_BITS_TO_FLOAT( ii ) \
  int iii = ii; \
  float result; \
  int offset = sizeof( int ) - sizeof( float ); \
  memcpy( &result, ((char*)&iii) + offset, sizeof( float ) ); \
  result;

#if( defined( __GNUC__ ) )
#  define DOUBLE_TO_LLONG_BITS( dd ) ( { __DOUBLE_TO_LLONG_BITS( dd ) } )
#  define LLONG_BITS_TO_DOUBLE( ll ) ( { __LLONG_BITS_TO_DOUBLE( ll ) } )
#  define FLOAT_TO_INT_BITS( ff ) ( { __FLOAT_TO_INT_BITS( ff ) } )
#  define INT_BITS_TO_FLOAT( ii ) ( { __INT_BITS_TO_FLOAT( ii ) } )
#else
BGL_RUNTIME_DECL BGL_LONGLONG_T DOUBLE_TO_LLONG_BITS( double );
BGL_RUNTIME_DECL double LLONG_BITS_TO_DOUBLE( BGL_LONGLONG_T );
BGL_RUNTIME_DECL int FLOAT_TO_INT_BITS( float );
BGL_RUNTIME_DECL float INT_BITS_TO_FLOAT( int );
#endif

#define RANDOMFL() ((double)rand()/RAND_MAX)
   
#if BGL_ISOC99 || defined( __USE_ISOC99 )
#  define BGL_SIGNBIT( a ) signbit( a ) 
#  define BGL_NAN ((double) NAN)
#  define BGL_INFINITY ((double) INFINITY)
#  define BGL_IS_FINITE isfinite
#  define BGL_IS_INF isinf
#  define BGL_IS_NAN isnan
#  define BGL_FL_MAX2( a, b ) fmax( a, b )
#  define BGL_FL_MIN2( a, b ) fmin( a, b )
#  define BGL_FL_ROUND( a ) round( a )
#else
#  if BGL_CCDIV0
#    define BGL_NAN (0.0 / 0.0)
#    define BGL_INFINITY (1.0 / 0.0)
#  else
BGL_RUNTIME_DECL double bgl_nan();
BGL_RUNTIME_DECL double bgl_infinity();
#    define BGL_NAN bgl_nan()
#    define BGL_INFINITY bgl_infinity()
#  endif   
#  define BGL_SIGNBIT( r ) ((r<0.0)?1:(((1.0/r)<0.0)?1:0))
#  define BGL_IS_FINITE( r ) (!(BGL_IS_INF( r ) || BGL_IS_NAN( r )))
#  define BGL_IS_INF( r ) (r==BGL_INFINITY || r==-BGL_INFINITY)
#  define BGL_IS_NAN( r ) ((r) != (r))
/* min and max have to look at -0.0 and +0.0, too */
#  define BGL_FL_MAX2( a, b ) (a<b?b:(a==0.0&&b==0.0&&1.0/a<0.0)?b:a)
#  define BGL_FL_MIN2( a, b ) (a>b?b:(a==0.0&&b==0.0&&1.0/a>0.0)?b:a)
#  define BGL_FL_ROUND( a ) (BGL_SIGNBIT(a)?-(floor(-a+0.5)):floor(a+0.5))
#endif

/*---------------------------------------------------------------------*/
/*    Long long                                                        */
/*---------------------------------------------------------------------*/
#define DEFINE_LLONG( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; \
                   BGL_LONGLONG_T llong; } \
      const aux = { __CNST_FILLER, MAKE_HEADER( LLONG_TYPE, 0 ), (BGL_LONGLONG_T)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#define LLONG_SIZE (sizeof( struct llong ))

#define LLONGP( o ) (POINTERP( o ) && (TYPE( o ) == LLONG_TYPE))

#define LLONG( o ) CREF( o )->llong_t

#define LONG_TO_LLONG( l ) ((BGL_LONGLONG_T)l)   
#define LLONG_TO_LONG( o ) ((long) o)
	    
#define BLLONG_TO_LLONG( l ) (LLONG( l ).llong)
#define BLLONG_TO_LONG( l ) ((long)BLLONG_TO_LLONG( l ))
#define LONG_TO_BLLONG( l ) (make_bllong((BGL_LONGLONG_T)l))
#define LLONG_TO_BLLONG( l ) (make_bllong((BGL_LONGLONG_T)l))

/*---------------------------------------------------------------------*/
/*    Exact long                                                       */
/*---------------------------------------------------------------------*/
#define DEFINE_ELONG( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; \
                   long elong; } \
      const aux = { __CNST_FILLER, MAKE_HEADER( ELONG_TYPE, 0 ), num }; \
      const obj_t name = BREF( &(aux.header) )
		 
#define ELONG_SIZE (sizeof( struct elong ))

#define ELONGP( o ) (POINTERP( o ) && (TYPE( o ) == ELONG_TYPE))

#define ELONG( o ) CREF( o )->elong_t

#define ELONG_TO_BELONG( _1 ) make_belong( _1 )
#define BELONG_TO_LONG( l ) (ELONG( l ).elong)

/*---------------------------------------------------------------------*/
/*    Stdint                                                           */
/*---------------------------------------------------------------------*/
#define BGL_INT8P( o ) \
   (((long)(o) & (long)((1 << (BGL_CNST_SHIFT_INT16)) -1)) == (long)BINT8H)
#define BGL_UINT8P( o ) \
   (((long)(o) & (long)((1 << (BGL_CNST_SHIFT_INT16)) -1)) == (long)BUINT8H)

#define BGL_INT16P( o ) \
   (((long)(o) & (long)((1 << (BGL_CNST_SHIFT_INT16)) -1)) == (long)BINT16H)
#define BGL_UINT16P( o ) \
   (((long)(o) & (long)((1 << (BGL_CNST_SHIFT_INT16)) -1)) == (long)BUINT16H)

#if( defined( BGL_CNST_SHIFT_INT32 ) )   
#  define BGL_INT32P( o ) \
   (((long)(o) & (((long)1 << (BGL_CNST_SHIFT_INT32)) -1)) == (long)BINT32H)
#  define BGL_UINT32P( o ) \
   (((long)(o) & (((long)1 << (BGL_CNST_SHIFT_INT32)) -1)) == (long)BUINT32H)
#else
#  define BGL_INT32P( o ) (POINTERP( o ) && (TYPE( o ) == INT32_TYPE))
#  define BGL_UINT32P( o ) (POINTERP( o ) && (TYPE( o ) == UINT32_TYPE))
#endif
   
#define BGL_INT64P( o ) (POINTERP( o ) && (TYPE( o ) == INT64_TYPE))
#define BGL_UINT64P( o ) (POINTERP( o ) && (TYPE( o ) == UINT64_TYPE))

#define BGL_INT8_TO_BINT8( i ) \
   ((obj_t)(BINT8H + ((int8_t)(i) << BGL_CNST_SHIFT_INT16))) 
#define BGL_UINT8_TO_BUINT8( i ) \
   ((obj_t)(BUINT8H + ((uint8_t)(i) << BGL_CNST_SHIFT_INT16)))

#define BGL_BINT8_TO_INT8( o ) \
   ((int8_t)((unsigned long)(o) >> BGL_CNST_SHIFT_INT16))
#define BGL_BUINT8_TO_UINT8( o ) \
   ((uint8_t)((unsigned long)(o) >> BGL_CNST_SHIFT_INT16))

#define BGL_INT16_TO_BINT16( i ) \
   ((obj_t)(BINT16H + ((int16_t)(i) << BGL_CNST_SHIFT_INT16))) 
#define BGL_UINT16_TO_BUINT16( i ) \
   ((obj_t)(BUINT16H + ((uint16_t)(i) << BGL_CNST_SHIFT_INT16))) 
	    
#define BGL_BINT16_TO_INT16( o ) \
   ((int16_t)((unsigned long)(o) >> BGL_CNST_SHIFT_INT16))
#define BGL_BUINT16_TO_UINT16( o ) \
   ((uint16_t)((unsigned long)(o) >> BGL_CNST_SHIFT_INT16))

#if( defined( BGL_CNST_SHIFT_INT32 ) )   
#  define DEFINE_INT32( name, aux, num ) \
   const obj_t name = BGL_INT32_TO_BINT32( num )
#  define DEFINE_UINT32( name, aux, num ) \
   const obj_t name = BGL_UINT32_TO_BUINT32( num )
   
#  define BGL_INT32_TO_BINT32( i ) \
   ((obj_t)(BINT32H + ((long)(i) << BGL_CNST_SHIFT_INT32))) 
#  define BGL_UINT32_TO_BUINT32( i ) \
   ((obj_t)(BUINT32H + ((long)(i) << BGL_CNST_SHIFT_INT32))) 
#  define BGL_BINT32_TO_INT32( o ) \
   ((int32_t)((unsigned long)(o) >> BGL_CNST_SHIFT_INT32))
#  define BGL_BUINT32_TO_UINT32( o ) \
   ((uint32_t)((unsigned long)(o) >> BGL_CNST_SHIFT_INT32))
#else
#  define DEFINE_INT32( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; int32_t val; } \
      const aux = { __CNST_FILLER, MAKE_HEADER( INT32_TYPE, 0 ), (int32_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#  define DEFINE_UINT32( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; int32_t val; } \
      const aux = { __CNST_FILLER, MAKE_HEADER( UINT32_TYPE, 0 ), (uint32_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#  define BGL_INT32_SIZE (sizeof( struct bgl_sint32 ))
#  define BGL_UINT32_SIZE (sizeof( struct bgl_uint32 ))
#  define BGL_INT32( o ) CREF( o )->sint32_t
#  define BGL_UINT32( o ) CREF( o )->uint32_t
#  define BGL_INT32_TO_BINT32( _1 ) bgl_make_bint32( _1 )
#  define BGL_UINT32_TO_BUINT32( _1 ) bgl_make_buint32( _1 )
#  define BGL_BINT32_TO_INT32( o ) BGL_INT32( o ).val
#  define BGL_BUINT32_TO_UINT32( o ) BGL_UINT32( o ).val
#endif

#define DEFINE_INT64( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; int64_t val; } \
      const aux = { __CNST_FILLER, MAKE_HEADER( INT64_TYPE, 0 ), (int64_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#define DEFINE_UINT64( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; uint64_t val; } \
      const aux = { __CNST_FILLER, MAKE_HEADER( UINT64_TYPE, 0 ), (uint64_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#define BGL_INT64_SIZE (sizeof( struct bgl_sint64 ))
#define BGL_UINT64_SIZE (sizeof( struct bgl_uint64 ))
#define BGL_INT64( o ) CREF( o )->sint64_t
#define BGL_UINT64( o ) CREF( o )->uint64_t
#define BGL_INT64_TO_BINT64( _1 ) bgl_make_bint64( _1 )
#define BGL_UINT64_TO_BUINT64( _1 ) bgl_make_buint64( _1 )
#define BGL_BINT64_TO_INT64( o ) BGL_INT64( o ).val
#define BGL_BUINT64_TO_UINT64( o ) BGL_UINT64( o ).val
			   
/*---------------------------------------------------------------------*/
/*    Bignum                                                           */
/*---------------------------------------------------------------------*/
#  define BIGNUM_SIZE (sizeof( struct bignum ))

#  define BIGNUMP( o ) (POINTERP( o ) && (TYPE( o ) == BIGNUM_TYPE))

#  define BIGNUM( o ) (CREF( o )->bignum_t)

#if( BGL_HAVE_GMP )   
#  define BXSIZ( x ) ((x)->bignum_t.mpz._mp_size)
   
#  define BXZERO( x ) (BXSIZ( x ) == 0)
#  define BXPOSITIVE( x ) (BXSIZ( x ) > 0)
#  define BXNEGATIVE( x ) (BXSIZ( x ) < 0)

#  define BGL_SAFE_BX_TO_FX( x ) bgl_safe_bignum_to_fixnum( x )
#  define BGL_SAFE_PLUS_FX( x, y ) bgl_safe_plus_fx( x, y )
#  define BGL_SAFE_MINUS_FX( x, y ) bgl_safe_minus_fx( x, y )
#  define BGL_SAFE_MUL_FX( x, y ) bgl_safe_mul_fx( x, y )
#  define BGL_SAFE_QUOTIENT_FX( x, y ) bgl_safe_quotient_fx( x, y )
#  define BGL_SAFE_PLUS_ELONG( x, y ) bgl_safe_plus_elong( x, y )
#  define BGL_SAFE_MINUS_ELONG( x, y ) bgl_safe_minus_elong( x, y )
#  define BGL_SAFE_MUL_ELONG( x, y ) bgl_safe_mul_elong( x, y )
#  define BGL_SAFE_QUOTIENT_ELONG( x, y ) bgl_safe_quotient_elong( x, y )
#  define BGL_SAFE_PLUS_LLONG( x, y ) bgl_safe_plus_llong( x, y )
#  define BGL_SAFE_MINUS_LLONG( x, y ) bgl_safe_minus_llong( x, y )
#  define BGL_SAFE_MUL_LLONG( x, y ) bgl_safe_mul_llong( x, y )
#  define BGL_SAFE_QUOTIENT_LLONG( x, y ) bgl_safe_quotient_llong( x, y )
   
extern gmp_randstate_t gmp_random_state;
   
extern obj_t bgl_safe_bignum_to_fixnum( obj_t );
extern obj_t bgl_safe_plus_fx( long, long );
extern obj_t bgl_safe_minus_fx( long, long );
extern obj_t bgl_safe_mul_fx( long, long );
extern obj_t bgl_safe_quotient_fx( long, long );
   
extern obj_t bgl_safe_plus_elong( long, long );
extern obj_t bgl_safe_minus_elong( long, long );
extern obj_t bgl_safe_mul_elong( long, long );
extern obj_t bgl_safe_quotient_elong( long, long );
   
extern obj_t bgl_safe_plus_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
extern obj_t bgl_safe_minus_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
extern obj_t bgl_safe_mul_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
extern obj_t bgl_safe_quotient_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
#else
#  define BGL_BIGNUM_U16VECT( bx ) (BIGNUM( bx ).u16vect)

#  define BGL_SAFE_BX_TO_FX( x ) (x)
#  define BGL_SAFE_PLUS_FX( x, y ) BINT( (x) + (y) )
#  define BGL_SAFE_MINUS_FX( x, y ) BINT( (x) - (y) )
#  define BGL_SAFE_MUL_FX( x, y ) BINT( (x) * (y) )
#  define BGL_SAFE_QUOTIENT_FX( x, y ) BINT( (x) / (y) )
#  define BGL_SAFE_PLUS_ELONG( x, y ) ELONG_TO_BELONG( (x) + (y) )
#  define BGL_SAFE_MINUS_ELONG( x, y ) ELONG_TO_BELONG( (x) - (y) )
#  define BGL_SAFE_MUL_ELONG( x, y ) ELONG_TO_BELONG( (x) * (y) )
#  define BGL_SAFE_QUOTIENT_ELONG( x, y ) ELONG_TO_BELONG( (x) / (y) )
#  define BGL_SAFE_PLUS_LLONG( x, y ) LLONG_TO_BLLONG( (x) + (y) )
#  define BGL_SAFE_MINUS_LLONG( x, y ) LLONG_TO_BLLONG( (x) - (y) )
#  define BGL_SAFE_MUL_LLONG( x, y ) LLONG_TO_BLLONG( (x) * (y) )
#  define BGL_SAFE_QUOTIENT_LLONG( x, y ) LLONG_TO_BLLONG( (x) / (y) )
   
extern bool_t BXZERO( obj_t );
extern bool_t BXPOSITIVE( obj_t );
extern bool_t BXNEGATIVE( obj_t );
#endif
   
/*---------------------------------------------------------------------*/
/*    Array bound checking                                             */
/*---------------------------------------------------------------------*/
#if( TAG_SHIFT <= LONG_MAX )
#   define BOUND_CHECK( o, v ) ((unsigned long)o < (unsigned long)v)
#else
#   define BOUND_CHECK( o, v ) (((long)o >= 0) && ((long)o < (long)v))
#endif

/*---------------------------------------------------------------------*/
/*    HVECTOR                                                          */
/*---------------------------------------------------------------------*/
#define HVECTOR_SIZE (sizeof( struct bgl_hvector ))
   
#define HVECTOR( o ) CREF( o )->hvector_t
   
#define STVECTOR( o, type ) \
   ((struct { header_t header; int length; type obj0; } *)(CREF( o )))
   
#define BGL_HVECTOR_LENGTH( v ) (HVECTOR( v ).length)

#define BGL_HVECTOR_IDENT( v ) (TYPE( v ) - S8VECTOR_TYPE)
   
#define BGL_HVECTORP( v ) \
    (POINTERP( v ) \
     && (TYPE( v ) >= S8VECTOR_TYPE) \
     && (TYPE( v ) <= F64VECTOR_TYPE))

#define BGL_S8VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S8VECTOR_TYPE))
#define BGL_U8VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U8VECTOR_TYPE))
#define BGL_S16VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S16VECTOR_TYPE))
#define BGL_U16VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U16VECTOR_TYPE))
#define BGL_S32VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S32VECTOR_TYPE))
#define BGL_U32VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U32VECTOR_TYPE))
#define BGL_S64VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S64VECTOR_TYPE))
#define BGL_U64VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U64VECTOR_TYPE))
#define BGL_F32VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == F32VECTOR_TYPE))
#define BGL_F64VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == F64VECTOR_TYPE))

#define BGL_S8VREF( v, i ) (&(STVECTOR( v, int8_t )->obj0))[ i ]
#define BGL_S8VSET( v, i, o ) (BGL_S8VREF( v, i ) = o, BUNSPEC)
#define BGL_U8VREF( v, i ) (&(STVECTOR( v, uint8_t )->obj0))[ i ]
#define BGL_U8VSET( v, i, o ) (BGL_S8VREF( v, i ) = o, BUNSPEC)
   
#define BGL_S16VREF( v, i ) (&(STVECTOR( v, int16_t )->obj0))[ i ]
#define BGL_S16VSET( v, i, o ) (BGL_S16VREF( v, i ) = o, BUNSPEC)
#define BGL_U16VREF( v, i ) (&(STVECTOR( v, uint16_t )->obj0))[ i ]
#define BGL_U16VSET( v, i, o ) (BGL_S16VREF( v, i ) = o, BUNSPEC)

#define BGL_S32VREF( v, i ) (&(STVECTOR( v, int32_t )->obj0))[ i ]
#define BGL_S32VSET( v, i, o ) (BGL_S32VREF( v, i ) = o, BUNSPEC)
#define BGL_U32VREF( v, i ) (&(STVECTOR( v, uint32_t )->obj0))[ i ]
#define BGL_U32VSET( v, i, o ) (BGL_U32VREF( v, i ) = o, BUNSPEC)

#define BGL_S64VREF( v, i ) (&(STVECTOR( v, BGL_LONGLONG_T )->obj0))[ i ]
#define BGL_S64VSET( v, i, o ) (BGL_S64VREF( v, i ) = o, BUNSPEC)
#define BGL_U64VREF( v, i ) (&(STVECTOR( v, unsigned BGL_LONGLONG_T )->obj0))[i]
#define BGL_U64VSET( v, i, o ) (BGL_U64VREF( v, i ) = o, BUNSPEC)

#define BGL_F32VREF( v, i ) (&(STVECTOR( v, float )->obj0))[ i ]
#define BGL_F32VSET( v, i, o ) (BGL_F32VREF( v, i ) = o, BUNSPEC)

#define BGL_F64VREF( v, i ) (&(STVECTOR( v, double )->obj0))[ i ]
#define BGL_F64VSET( v, i, o ) (BGL_F64VREF( v, i ) = o, BUNSPEC)

#define BGL_XXX_U8VREF( v, i, type ) \
   (*((type *)(&BGL_S8VREF( v, i ))))
#define BGL_XXX_U8VSET( v, i, o, type ) \
   (*((type *)(&BGL_S8VREF( v, i ))) = (o), BUNSPEC)
   
#define BGL_S16_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, int16_t )
#define BGL_S16_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, int16_t )
#define BGL_U16_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, uint16_t )
#define BGL_U16_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, uint16_t )
   
#define BGL_S32_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, int32_t )
#define BGL_S32_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, int32_t )
#define BGL_U32_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, uint32_t )
#define BGL_U32_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, uint32_t )
   
#define BGL_S64_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, int64_t )
#define BGL_S64_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, int64_t )
#define BGL_U64_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, uint64_t )
#define BGL_U64_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, uint64_t )
   
#define BGL_F32_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, float )
#define BGL_F32_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, float )
   
#define BGL_F64_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, double )
#define BGL_F64_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, double )
   
BGL_RUNTIME_DECL obj_t alloc_hvector( int, int, int );
   
#define BGL_ALLOC_S8VECTOR( len ) \
   alloc_hvector( len, sizeof( int8_t ), S8VECTOR_TYPE )

#define BGL_ALLOC_U8VECTOR( len ) \
   alloc_hvector( len, sizeof( uint8_t ), U8VECTOR_TYPE )

#define BGL_ALLOC_S16VECTOR( len ) \
   alloc_hvector( len, sizeof( int16_t ), S16VECTOR_TYPE )

#define BGL_ALLOC_U16VECTOR( len ) \
   alloc_hvector( len, sizeof( uint16_t ), U16VECTOR_TYPE )

#define BGL_ALLOC_S32VECTOR( len ) \
   alloc_hvector( len, sizeof( int32_t ), S32VECTOR_TYPE )

#define BGL_ALLOC_U32VECTOR( len ) \
   alloc_hvector( len, sizeof( uint32_t ), U32VECTOR_TYPE )

#define BGL_ALLOC_S64VECTOR( len ) \
   alloc_hvector( len, sizeof( BGL_LONGLONG_T ), S64VECTOR_TYPE )

#define BGL_ALLOC_U64VECTOR( len ) \
   alloc_hvector( len, sizeof( unsigned BGL_LONGLONG_T ), U64VECTOR_TYPE )

#define BGL_ALLOC_F32VECTOR( len ) \
   alloc_hvector( len, sizeof( float ), F32VECTOR_TYPE )

#define BGL_ALLOC_F64VECTOR( len ) \
   alloc_hvector( len, sizeof( double ), F64VECTOR_TYPE )

#define BGL_SU8VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S8VREF( target, tstart ), (void *)&BGL_S8VREF( source, sstart ), \
	   (ssend - sstart) )
   
#define BGL_SU16VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S16VREF( target, tstart ), (void *)&BGL_S16VREF( source, sstart ), \
      (ssend - sstart) * 2 )
   
#define BGL_SU32VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S32VREF( target, tstart ), (void *)&BGL_S32VREF( source, sstart ), \
      (ssend - sstart) * 4 )
   
#define BGL_SU64VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S64VREF( target, tstart ), (void *)&BGL_S64VREF( source, sstart ), \
      (ssend - sstart) * 8 )
   
#define BGL_F32VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_F32VREF( target, tstart ), (void *)&BGL_F32VREF( source, sstart ), \
      (ssend - sstart) * 4 )
   
#define BGL_F64VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_F64VREF( target, tstart ), (void *)&BGL_F64VREF( source, sstart ), \
      (ssend - sstart) * 8 )
   
/*---------------------------------------------------------------------*/
/*    Ports                                                            */
/*---------------------------------------------------------------------*/
#define PORT( o ) CREF( o )->port_t
   
#define PORT_CHOOK( o ) (PORT( o ).chook)
#define PORT_CHOOK_SET( o, v ) ((PORT( o ).chook) = (v))
   
#define PORT_FILE( o ) (PORT( o ).stream.file)
#define PORT_FD( o ) (PORT( o ).stream.fd)
#define PORT_CHANNEL( o ) (PORT( o ).stream.channel)

/*---------------------------------------------------------------------*/
/*    Output-ports                                                     */
/*---------------------------------------------------------------------*/
#define OUTPUT_PORT_SIZE (sizeof( struct output_port ))

#define OUTPUT_PORT( o ) (CREF( o )->output_port_t)

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
#define BGL_OUTPUT_PORT_FHOOK_SET( o, v ) ((OUTPUT_PORT( o ).fhook) = (v))
   
#define BGL_OUTPUT_PORT_FLUSHBUF( o ) (OUTPUT_PORT( o ).flushbuf)
#define BGL_OUTPUT_PORT_FLUSHBUF_SET( o, v ) ((OUTPUT_PORT( o ).flushbuf) = (v))
   
#define BGL_OUTPUT_PORT_BUFFER( o ) \
   (OUTPUT_PORT( o ).buf)
   
/* output ports buffering mode */
#define BGL_IONB  0 /* unubuffered */
#define BGL_IOLBF 1 /* line buffered */
#define BGL_IOFBF 2 /* fully buffered */
#define BGL_IOEBF 3 /* extensible buffered */

/* #define BGL_DISPLAY_STRING( o, op ) \                               */
/*    bgl_write_with_lock( op, &STRING_REF( o, 0 ), STRING_LENGTH( o ) ) */
/*                                                                     */
/* #define BGL_DISPLAY_SUBSTRING( o, start, end, op ) \                */
/*    bgl_write_with_lock( op, &STRING_REF( o, start ), end - start )  */

/*---------------------------------------------------------------------*/
/*    Les OUTPUT_STRING_PORTs                                          */
/*---------------------------------------------------------------------*/
#define OUTPUT_STRING_PORT_SIZE (sizeof( struct output_string_port ))

#define OUTPUT_STRING_PORT( o ) CREF( o )->output_string_port_t

#define OUTPUT_STRING_PORT_BUFFER_SIZE 128

#define END_OF_STRING_PORTP( o ) \
   ( OUTPUT_STRING_PORT( o ).offset == OUTPUT_STRING_PORT( o ).size )

/*---------------------------------------------------------------------*/
/*    OUTPUT_PROCEDURE_PORT                                            */
/*---------------------------------------------------------------------*/
#define PORT_ON_PROCEDUREP( o ) ( TYPE( o ) == OUTPUT_PROCEDURE_PORT_TYPE )

#define OUTPUT_PROCEDURE_PORT_SIZE (sizeof( struct output_procedure_port ))

#define OUTPUT_PROCEDURE_PORT( o ) CREF( o )->output_procedure_port_t

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

#define INPUT_GZIP_PORT( o ) CREF( o )->input_gzip_port_t
#define INPUT_PROCEDURE_PORT( o ) CREF( o )->input_procedure_port_t

#define INPUT_PORT( o ) CREF( o )->input_port_t

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
   INPUT_PORT( o ).userseek = p
   
/*---------------------------------------------------------------------*/
/*    Binary ports                                                     */
/*---------------------------------------------------------------------*/
#define BINARY_PORT_SIZE (sizeof( struct binary_port ))

#define BINARY_PORT( o ) CREF( o )->binary_port_t

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

#define BGL_MMAP( o ) CREF( o )->mmap_t

#define BGL_MMAPP( o ) (POINTERP( o ) && (TYPE( o ) == MMAP_TYPE))

#define BGL_MMAP_TO_STRING( s ) (BGL_MMAP( s ).map)
   
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
#define WEAKPTR( o ) CREF( o )->weakptr_t

#define WEAKPTR_SIZE (sizeof(struct bgl_weakptr))

#define BGL_WEAKPTRP( v ) \
 (POINTERP( v ) && (TYPE( v ) == WEAKPTR_TYPE))

BGL_RUNTIME_DECL obj_t make_weakptr( obj_t );
BGL_RUNTIME_DECL obj_t weakptr_data( obj_t );
BGL_RUNTIME_DECL void weakptr_data_set( obj_t , obj_t  );

/*---------------------------------------------------------------------*/
/*    Object macros                                                    */
/*---------------------------------------------------------------------*/
#define BGL_OBJECTP( _obj ) \
   ((POINTERP( _obj ) && (TYPE( _obj ) >= OBJECT_TYPE)))

#define BGL_OBJECT_CLASS_NUM( _obj ) \
   (TYPE( _obj ))

#define BGL_OBJECT_CLASS_NUM_SET( _1, _2 ) \
   (((obj_t)CREF(_1))->header = MAKE_HEADER( _2, 0 ), BUNSPEC)
   
#define BGL_OBJECT_WIDENING( _obj ) \
   (((object_bglt)(CREF(_obj)))->widening)

#define BGL_OBJECT_WIDENING_SET( _obj, _str ) \
   (((((object_bglt)(CREF(_obj)))->widening) = _str), BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Classes                                                          */
/*---------------------------------------------------------------------*/
#define BGL_CLASSP( o ) (POINTERP( o ) && (TYPE( o ) == CLASS_TYPE))

#define BGL_CLASS_SIZE (sizeof( struct bgl_class ) )
#define BGL_CLASS( f ) (CREF( f )->class_t)
   
#define BGL_CLASS_NAME( f ) (BGL_CLASS( f ).name)
   
#define BGL_CLASS_INDEX( f ) (BGL_CLASS( f ).index)
   
#define BGL_CLASS_DEPTH( f ) (BGL_CLASS( f ).depth)
   
#define BGL_CLASS_SUPER( f ) (BGL_CLASS( f ).super)
#define BGL_CLASS_ANCESTORS( f ) (BGL_CLASS( f ).ancestors)
#define BGL_CLASS_ANCESTORS_REF( f, i ) (&(BGL_CLASS( f ).ancestor0))[ i ]
   
#define BGL_CLASS_SUBCLASSES( f ) (BGL_CLASS( f ).subclasses)
#define BGL_CLASS_SUBCLASSES_SET( f, v ) (BGL_CLASS_SUBCLASSES( f ) = v)
   
#define BGL_CLASS_DIRECT_FIELDS( f ) (BGL_CLASS( f ).direct_fields)
#define BGL_CLASS_DIRECT_FIELDS_SET( f, v ) (BGL_CLASS_DIRECT_FIELDS( f ) = v)
   
#define BGL_CLASS_ALL_FIELDS( f ) (BGL_CLASS( f ).all_fields)
#define BGL_CLASS_ALL_FIELDS_SET( f, v ) (BGL_CLASS_ALL_FIELDS( f ) = v)
   
#define BGL_CLASS_VIRTUAL_FIELDS( f ) (BGL_CLASS( f ).virtual_fields)
   
#define BGL_CLASS_MODULE( f ) (BGL_CLASS( f ).module)
   
#define BGL_CLASS_ALLOC_FUN( f ) (BGL_CLASS( f ).alloc_fun)

#define BGL_CLASS_HASH( f ) (BGL_CLASS( f ).hash)
   
#define BGL_CLASS_NEW_FUN( f ) (BGL_CLASS( f ).new_fun)
   
#define BGL_CLASS_NIL_FUN( f ) (BGL_CLASS( f ).nil_fun)
   
#define BGL_CLASS_NIL( f ) (BGL_CLASS( f ).nil)
#define BGL_CLASS_NIL_SET( f, v ) (BGL_CLASS_NIL( f ) = v)
   
#define BGL_CLASS_CONSTRUCTOR( f ) (BGL_CLASS( f ).constructor)
   
#define BGL_CLASS_SHRINK( f ) (BGL_CLASS( f ).shrink)
   
#define BGL_CLASS_EVDATA( f ) (BGL_CLASS( f ).evdata)   
#define BGL_CLASS_EVDATA_SET( f, o ) (BGL_CLASS_EVDATA( f ) = o)
   
/*---------------------------------------------------------------------*/
/*    Process handling                                                 */
/*---------------------------------------------------------------------*/
#define PROCESSP( o ) (POINTERP( o ) && (TYPE( o ) == PROCESS_TYPE))
#define PROCESS_SIZE (sizeof( struct process ))
#define PROCESS( o ) (CREF( o )->process_t)
#define PROCESS_PID( o ) (PROCESS( o ).pid)
#define PROCESS_INPUT_PORT( o ) (PROCESS( o ).stream[ 0 ])
#define PROCESS_OUTPUT_PORT( o ) (PROCESS( o ).stream[ 1 ])
#define PROCESS_ERROR_PORT( o ) (PROCESS( o ).stream[ 2 ])

/*---------------------------------------------------------------------*/
/*    Socket handling                                                  */
/*---------------------------------------------------------------------*/
#define SOCKETP( o ) (POINTERP( o ) && (TYPE( o ) == SOCKET_TYPE))
#define SOCKET_SIZE (sizeof( struct socket ))
#define SOCKET( o ) (CREF( o )->socket_t)
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
#define BGL_DATAGRAM_SOCKET( o ) (CREF( o )->datagram_socket_t)

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
#define BGL_REGEXP( o ) (CREF( o )->regexp_t)
#define BGL_REGEXP_PREG( o ) (BGL_REGEXP( o ).preg)   
#define BGL_REGEXP_PREG_SET( o, v ) (BGL_REGEXP_PREG( o ) = (v))
#define BGL_REGEXP_PAT( o ) (BGL_REGEXP( o ).pat)   

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
#define CUSTOM( f ) CREF( f )->custom_t

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
#define BGL_DATE( f ) CREF( f )->date_t

#define BGL_DATE_NANOSECOND( f ) (BGL_DATE( f ).nsec)
#define BGL_DATE_SECOND( f ) (BGL_DATE( f ).sec)
#define BGL_DATE_MINUTE( f ) (BGL_DATE( f ).min)
#define BGL_DATE_HOUR( f ) (BGL_DATE( f ).hour)
#define BGL_DATE_DAY( f ) (BGL_DATE( f ).mday)
#define BGL_DATE_WDAY( f ) (BGL_DATE( f ).wday)
#define BGL_DATE_YDAY( f ) (BGL_DATE( f ).yday)
#define BGL_DATE_MONTH( f ) (BGL_DATE( f ).mon)
#define BGL_DATE_YEAR( f ) (BGL_DATE( f ).year)
#define BGL_DATE_TIMEZONE( f ) (BGL_DATE( f ).timezone)
#define BGL_DATE_ISDST( f ) (BGL_DATE( f ).isdst)		 

/*---------------------------------------------------------------------*/
/*    Mutexes and condition variables                                  */
/*---------------------------------------------------------------------*/
#define BGL_MUTEXP( o ) (POINTERP( o ) && (TYPE( o ) == MUTEX_TYPE))
   
#define BGL_MUTEX( o )  (CREF( o )->mutex_t)
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
#   define BGL_MUTEX_SYSMUTEX( o ) &(BGL_MUTEX( o ).sysmutex)
#else
#   define BGL_MUTEX_SYSMUTEX( o ) BGL_MUTEX( o ).sysmutex
#endif
   
#define BGL_CONDVARP( o ) (POINTERP( o ) && (TYPE( o ) == CONDVAR_TYPE))
#define BGL_CONDVAR( o ) (CREF( o )->condvar_t)
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
#define FOREIGN( f ) CREF( f )->foreign_t

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

#define BGL_DYNAMIC_ENV( env ) (CREF( env )->dynamic_env_t)

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
   BGL_ENV_ERROR_NOTIFIERS_SET( BGL_CURRENT_DYNAMIC_ENV , _not )
   
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
#   include <assert.h>
#else
#undef assert   
#   define assert( exp ) ;
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
#define SET_EXIT( exit ) \
   SETJMP( jmpbuf )
#define JUMP_EXIT( exit, val ) \
   BGL_EXIT_VALUE_SET( val ), LONGJMP( exit, 1 )

#ifdef __ia64__
/* IA64 code */
#include <ucontext.h>

struct ia64_rv_t {
   long retval;
   long first_return;
};
		 
typedef struct {
   ucontext_t ctx;
   long backing_store_size;
   void *backing_store;
   struct ia64_rv_t rv;
} callcc_jmp_buf;
		 
extern unsigned long  __libc_ia64_register_backing_store_base;
extern struct ia64_rv_t ia64_getcontext (ucontext_t *) __asm__ ("getcontext");
		 
#  define CALLCC_SET_EXIT( exit ) \
  (jmpbuf.rv = ia64_getcontext( &(jmpbuf.ctx) ), \
     (jmpbuf.rv.first_return ? \
       jmpbuf.backing_store_size = jmpbuf.ctx.uc_mcontext.sc_ar_bsp - \
                                   __libc_ia64_register_backing_store_base, \
       jmpbuf.backing_store = GC_MALLOC( jmpbuf.backing_store_size ), \
       memcpy( jmpbuf.backing_store, \
               (void *)__libc_ia64_register_backing_store_base, \
               jmpbuf.backing_store_size ), \
       0 : 1))

#  define CALLCC_JUMP_EXIT( exit, val ) \
   (BGL_EXIT_VALUE_SET( val ), \
    memcpy( (void *)__libc_ia64_register_backing_store_base, \
            ((callcc_jmp_buf *)exit)->backing_store, \
            ((callcc_jmp_buf *)exit)->backing_store_size), \
    setcontext( &(((callcc_jmp_buf *)exit)->ctx) ), \
    BUNSPEC)
#else
/* !IA64 code */
typedef jmp_buf_t callcc_jmp_buf;
		 
#  define CALLCC_SET_EXIT( exit ) SET_EXIT( exit )
#  define CALLCC_JUMP_EXIT( exit, val ) (JUMP_EXIT( exit, val ), BUNSPEC)
#endif
		 
/*---------------------------------------------------------------------*/
/*    the `bind-exit' linking.                                         */
/*---------------------------------------------------------------------*/
struct exitd {
   void *exit;
   long userp;
   obj_t stamp;
   /* protected blocks */
   union scmobj *protect0;
   union scmobj *protect1;
   union scmobj *protectn;
   /* linking */
   struct bgl_dframe *top_of_frame;
   struct exitd *prev;
};

#define EXITD_SYSTEM 0
#define EXITD_USER 1
#define EXITD_CALLCC 2		 

#define PUSH_ENV_EXIT( env, _xit, _ser ) \
   struct exitd exitd; \
   exitd.exit  = _xit; \
   exitd.userp = _ser; \
   exitd.protect0 = BFALSE; \
   exitd.protect1 = BFALSE; \
   exitd.protectn = BNIL; \
   exitd.top_of_frame = BGL_ENV_GET_TOP_OF_FRAME( env ); \
   exitd.prev  = BGL_ENV_EXITD_TOP( env ); \
   exitd.stamp = BGL_ENV_EXITD_STAMP( env ); \
   BGL_ENV_EXITD_TOP_SET( env, (&exitd) );
   
#define PUSH_EXIT( _xit, _ser ) \
   PUSH_ENV_EXIT( BGL_CURRENT_DYNAMIC_ENV(), _xit, _ser )

#define POP_ENV_EXIT( env ) \
   BGL_ENV_SET_TOP_OF_FRAME( env, BGL_ENV_EXITD_TOP( env )->top_of_frame); \
   BGL_ENV_EXITD_TOP_SET( env, BGL_ENV_EXITD_TOP( env )->prev ); \

#define POP_EXIT() \
   POP_ENV_EXIT( BGL_CURRENT_DYNAMIC_ENV() )

#define EXITD_TO_EXIT( ptr ) \
   ((struct exitd *)(ptr))->exit

#define EXITD_USERP( ptr ) \
   (((struct exitd *)(ptr))->userp != EXITD_SYSTEM)

#define EXITD_CALLCCP( ptr ) \
   (((struct exitd *)(ptr))->userp == EXITD_CALLCC)

#define EXITD_STAMP( ptr ) \
   (((struct exitd *)(ptr))->stamp)

#define BGL_EXITD_BOTTOMP( extd ) \
   (((struct exitd *)(extd)) == BGL_ENV_EXITD_BOTTOM( BGL_CURRENT_DYNAMIC_ENV() ))

#define BGL_EXITD_PROTECT0( ptr ) \
   (((struct exitd *)(ptr))->protect0)
   
#define BGL_EXITD_PROTECT1( ptr ) \
   (((struct exitd *)(ptr))->protect1)
   
#define BGL_EXITD_PROTECTN( ptr ) \
   (((struct exitd *)(ptr))->protectn)

#define BGL_EXITD_PROTECT0_SET( extd, p ) \
   (BGL_EXITD_PROTECT0( extd ) = (p))
   
#define BGL_EXITD_PROTECT1_SET( extd, p ) \
   (BGL_EXITD_PROTECT1( extd ) = (p))
   
#define BGL_EXITD_PROTECTN_SET( extd, p ) \
   (BGL_EXITD_PROTECTN( extd ) = (p))

#if( HAVE_ALLOCA && defined( __GNUC__ ) )
#  if( defined( TAG_PAIR ) )
#    define MAKE_STACK_PAIR( a, d ) \
        ({ obj_t an_object; \
           an_object = alloca( PAIR_SIZE ); \
	   an_object->pair_t.car = a; \
	   an_object->pair_t.cdr = d; \
           ( BPAIR( an_object ) ); })
#  else   
#    define MAKE_STACK_PAIR( a, d ) \
        ({ obj_t an_object; \
           an_object = alloca( PAIR_SIZE ); \
   	   an_object->pair_t.header = MAKE_HEADER( PAIR_TYPE, 0 ); \
	   an_object->pair_t.car = a; \
	   an_object->pair_t.cdr = d; \
           ( BPAIR( an_object ) ); })
#  endif     
#else
#  define MAKE_STACK_PAIR( a, d ) MAKE_PAIR( a, d )   
#endif
   
#define BGL_EXITD_PUSH_PROTECT( extd, p ) \
   BGL_EXITD_PROTECT0( extd ) == BFALSE ? BGL_EXITD_PROTECT0_SET( extd, p ) : \
   BGL_EXITD_PROTECT1( extd ) == BFALSE ? BGL_EXITD_PROTECT1_SET( extd, p ) : \
      BGL_EXITD_PROTECTN_SET( extd, MAKE_STACK_PAIR( p, BGL_EXITD_PROTECTN( extd ) ) )
   
#define BGL_EXITD_POP_PROTECT( extd ) \
   BGL_EXITD_PROTECT1( extd ) == BFALSE ? \
      BGL_EXITD_PROTECT0_SET( extd, BFALSE ) :	\
      NULLP( BGL_EXITD_PROTECTN( extd ) ) ? \
        BGL_EXITD_PROTECT1_SET( extd, BFALSE ) : \
        BGL_EXITD_PROTECTN_SET( extd, CDR( BGL_EXITD_PROTECTN( extd ) ) )
   
/*---------------------------------------------------------------------*/
/*    `dynamic-wind' before thunk linking.                             */
/*---------------------------------------------------------------------*/
struct befored {
   obj_t before;
   struct befored *prev;
};

#define PUSH_BEFORE( _bfr ) \
   struct befored befored; \
   befored.before = _bfr; \
   befored.prev = BGL_BEFORED_TOP(); \
   BGL_BEFORED_TOP_SET( &befored );

#define POP_BEFORE() \
   BGL_BEFORED_TOP_SET( BGL_BEFORED_TOP()->prev )

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
   
#define STACK( _o_ ) CREF( _o_ )->stack_t

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
/*    Saw C macros                                                     */
/*---------------------------------------------------------------------*/
#define BGL_RTL_STRING_REF(v,i) ((unsigned char) STRING_REF(v,i))
#define BGL_RTL_EQ(x,y) (x == y)
#define BGL_RTL_GE(x,y) (x >= y)
#define BGL_RTL_LE(x,y) (x <= y)
#define BGL_RTL_GT(x,y) (x > y)
#define BGL_RTL_LT(x,y) (x < y)
#define BGL_RTL_OR(x,y) (x | y)
#define BGL_RTL_AND(x,y) (x & y)
#define BGL_RTL_ADD(x,y) (x + y)
#define BGL_RTL_SUB(x,y) (x - y)
#define BGL_RTL_MUL(x,y) (x * y)
#define BGL_RTL_DIV(x,y) (x / y)
#define BGL_RTL_REM(x,y) (x % y)
#define BGL_RTL_XOR(x,y) (x ^ y)
#define BGL_RTL_RSH(x,y) (x >> y)
#define BGL_RTL_LSH(x,y) (x << y)

#define BGL_RTL_PUSH_ENV_EXIT( env, _xit, _ser ) \
   exitd.exit  = _xit; \
   exitd.userp = _ser; \
   exitd.protect0 = BFALSE; \
   exitd.protect1 = BFALSE; \
   exitd.protectn = BNIL; \
   exitd.top_of_frame = BGL_ENV_GET_TOP_OF_FRAME( env ); \
   exitd.prev  = BGL_ENV_EXITD_TOP( env ); \
   exitd.stamp = BGL_ENV_EXITD_STAMP( env ); \
   BGL_ENV_EXITD_TOP_SET( env, (&exitd) );

#define BGL_RTL_PUSH_EXIT( _xit, _ser ) \
   BGL_RTL_PUSH_ENV_EXIT( BGL_CURRENT_DYNAMIC_ENV(), _xit, _ser )

#define BGL_RTL_NOP() (BUNSPEC)
#define BGL_RTL_MOV(r) (r)
#define BGL_RTL_RETURN(r) return(r)
#define BGL_RTL_LOADI(v) (v)
#define BGL_RTL_LOADG(g) (g)
#define BGL_RTL_LOADFUN(g) ((obj_t) g)
#define BGL_RTL_STOREG(g,v) g=v
#define BGL_RTL_GLOBALREF(g) __EVMEANING_ADDRESS(g)
#define BGL_RTL_GO(l) goto l
#define BGL_RTL_IFEQ(l,r) if(!r) goto l
#define BGL_RTL_IFNE(l,r) if(r) goto l
#define BGL_RTL_APPLY(f,r) apply(f,r)
#define BGL_RTL_VALLOC(n) create_vector(n)
#define BGL_RTL_VREF(v,i) VECTOR_REF(v,i)
#define BGL_RTL_VSET(v,i,r) VECTOR_REF(v,i)=r
#define BGL_RTL_VLENGTH(v) VECTOR_LENGTH(v)
#define BGL_RTL_TVALLOC(m,t,n,d) ALLOCATE_TVECTOR(m,t,n,d)
#define BGL_RTL_TVREF(m,t,v,i) TVECTOR_REF(m,v,i)
#define BGL_RTL_TVSET(m,t,v,i,r) TVECTOR_REF(m,v,i)=r
#define BGL_RTL_TVLENGTH(m,t,v) TVECTOR_LENGTH(v)
#define BGL_RTL_CAST(t,e) ((t) e)
#define BGL_RTL_CAST_NULL(t) ((t) 0)
#define BGL_RTL_JUMPEXIT(x,v) JUMP_EXIT(x,v)
#define BGL_RTL_FAIL(p,m,o) FAILURE(p,m,o)
#define BGL_RTL_PROTECTED(x) (x)
#define BGL_RTL_MAKEBOX(v) MAKE_CELL(v)
#define BGL_RTL_BOXREF(r) CELL_REF(r)
#define BGL_RTL_BOXSET(r,v) CELL_REF(r)=v

/*---------------------------------------------------------------------*/
/*    The external declarations                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t bigloo_exit( obj_t );

BGL_RUNTIME_DECL obj_t va_generic_entry( obj_t, ... );
BGL_RUNTIME_DECL obj_t opt_generic_entry( obj_t, ... );
BGL_RUNTIME_DECL obj_t apply( obj_t, obj_t );

BGL_RUNTIME_DECL void bgl_init_module_debug_start( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_library( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_import( char *, char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_object( char * );
BGL_RUNTIME_DECL void bgl_init_module_debug_end( char * );
   
BGL_RUNTIME_DECL obj_t the_failure( obj_t, obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_system_failure( int, obj_t, obj_t, obj_t );

BGL_RUNTIME_DECL obj_t bgl_make_procedure( obj_t, int, int );
BGL_RUNTIME_DECL obj_t make_fx_procedure( function_t, int, int );
BGL_RUNTIME_DECL obj_t make_va_procedure( function_t, int, int );
BGL_RUNTIME_DECL obj_t bgl_time( obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_procedure_entry_to_string( obj_t ); 
BGL_RUNTIME_DECL obj_t bgl_string_to_procedure_entry( obj_t );

BGL_RUNTIME_DECL obj_t make_pair( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t make_extended_pair( obj_t a, obj_t d, obj_t e );
BGL_RUNTIME_DECL obj_t make_cell( obj_t );
BGL_RUNTIME_DECL obj_t make_real( double );
BGL_RUNTIME_DECL obj_t make_belong( long );
BGL_RUNTIME_DECL obj_t make_bllong( BGL_LONGLONG_T );

#if( !defined( BGL_CNST_SHIFT_INT32 ) )   
BGL_RUNTIME_DECL obj_t bgl_make_bint32( int32_t );
BGL_RUNTIME_DECL obj_t bgl_make_buint32( uint32_t );
#endif
   
BGL_RUNTIME_DECL obj_t bgl_make_bint64( int64_t );
BGL_RUNTIME_DECL obj_t bgl_make_buint64( uint64_t );

BGL_RUNTIME_DECL obj_t bgl_string_to_bignum( char *, int );

BGL_RUNTIME_DECL obj_t bgl_reverse( obj_t );
BGL_RUNTIME_DECL obj_t bgl_reverse_bang( obj_t );
   
BGL_RUNTIME_DECL long bgl_list_length( obj_t );
BGL_RUNTIME_DECL obj_t bgl_remq( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_remq_bang( obj_t, obj_t );
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
BGL_RUNTIME_DEF bool_t bgl_port_isatty( obj_t );
					
BGL_RUNTIME_DECL obj_t create_vector( int );

BGL_RUNTIME_DECL obj_t make_string_sans_fill();
BGL_RUNTIME_DECL obj_t string_to_bstring( char * );
BGL_RUNTIME_DECL obj_t string_to_bstring_len( char *, int );
BGL_RUNTIME_DECL obj_t close_init_string();
BGL_RUNTIME_DECL obj_t bgl_string_shrink( obj_t, long );

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
   
BGL_RUNTIME_DECL obj_t bgl_make_date();
   
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

BGL_RUNTIME_DECL obj_t bgl_getgroups();
   
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
BGL_RUNTIME_DECL obj_t bgl_make_server_socket( obj_t, int, int );
BGL_RUNTIME_DECL obj_t bgl_socket_accept( obj_t, bool_t, obj_t, obj_t );
BGL_RUNTIME_DECL long bgl_socket_accept_many( obj_t, bool_t, obj_t, obj_t, obj_t );
   
BGL_RUNTIME_DECL obj_t bgl_gethostname();
BGL_RUNTIME_DECL obj_t bgl_socket_hostname( obj_t );
BGL_RUNTIME_DECL obj_t bgl_datagram_socket_hostname( obj_t );
BGL_RUNTIME_DECL obj_t bgl_getsockopt( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_setsockopt( obj_t, obj_t, obj_t );
   
BGL_RUNTIME_DECL void bgl_init_trace( void );

BGL_RUNTIME_DECL long bgl_rgc_blit_string( obj_t, char *, long, long );

BGL_RUNTIME_DECL obj_t bigloo_nan, bigloo_infinity, bigloo_minfinity;

BGL_RUNTIME_DECL obj_t bgl_regcomp( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_regfree( obj_t );
BGL_RUNTIME_DECL obj_t bgl_regmatch( obj_t, char *, bool_t, int, int );

BGL_RUNTIME_DECL void bgl_restore_signal_handlers();

#if( BGL_HAVE_UNISTRING )
BGL_RUNTIME_DECL int bgl_strcoll( obj_t, obj_t );
#endif					

#ifdef __cplusplus
}
#endif

#endif

