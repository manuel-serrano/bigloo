/*=====================================================================*/
/*    serrano/prgm/project/bigloo/nanh/bde/bmem/lib/init.c             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:28:06 2003                          */
/*    Last change :  Fri Nov  1 19:20:39 2024 (serrano)                */
/*    Copyright   :  2003-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Allocation profiling initialization                              */
/*=====================================================================*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>

#include <bigloo.h>
#include <bmem.h>

#include <dlfcn.h>
#ifndef RTLD_LAZY
#   define RTLD_LAZY 0
#endif

extern void GC_dump_statistics_json(FILE *f);
extern void GC_dump_statistics_sexp(FILE *f);
extern void GC_reset_statistics();
extern void type_dump(FILE *f);
extern void alloc_dump_statistics();
extern void alloc_dump_statistics_json(FILE *f);
extern void alloc_dump_statistics_sexp(FILE *f);

extern long long GC_alloc_total();
extern void bmem_init_wrapper(void *);
static void bmem_init();
extern void backtrace_init();

/*---------------------------------------------------------------------*/
/*    A static table to compute small hash values                      */
/*---------------------------------------------------------------------*/
static char* native_allocators[] = {
   "make_vector", "create_vector", 0L
};

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
int bmem_debug = 0;
int bmem_thread = 0;
int bmem_verbose = 1;
int bmem_color = 1;
int bmem_backtrace = 0;

pthread_key_t bmem_key;
pthread_mutex_t bmem_mutex;

/* garbage collector */
void *(*____GC_malloc)(size_t) = 0;
void *(*____GC_realloc)(void *, size_t) = 0;
void *(*____GC_malloc_atomic)(size_t) = 0;
void *(*____GC_malloc_uncollectable)(size_t) = 0;
void (*____GC_gcollect)() = 0;
void *(*____GC_add_gc_hook)(void (*)()) = 0;
char **____executable_name = 0;
void *____command_line = 0;
void (*____GC_reset_allocated_bytes)() = 0;
BGL_LONGLONG_T (*____bgl_current_nanoseconds)() = 0;

/* trace */
void (*____bgl_init_trace_register)(void (*i)(), obj_t (*g)(int), void (*w)(obj_t));
obj_t (*____bgl_get_trace_stack)(int);

/* thread */
void *(*____pthread_getspecific)(pthread_key_t);
int (*____pthread_setspecific)(pthread_key_t, void *);
int (*____pthread_key_create)(pthread_key_t *, void (*) (void *));
int (*____pthread_mutex_init)(pthread_mutex_t *, void *);
       
/* classes */
void *(*____register_class)(void *, void *, void *, long, void *, void *, void *, void *, void *, void *, void *);
int (*____bgl_types_number)();
long (*____get_hash_power_number)(char *, unsigned long);
long (*____get_hash_power_number_len)(char *, unsigned long, long);
void *(*____bgl_get_symtab)() = 0;
void (*____bgl_init_objects)() = 0;
void (*____bglpth_setup)() = 0;
void *unknown_ident;

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    unbound ...                                                      */
/*---------------------------------------------------------------------*/
void *
unbound() {
   fprintf(stderr, "unbound function\n");
   exit(-2);
}

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    open_shared_library ...                                          */
/*---------------------------------------------------------------------*/
void *open_shared_library(char *lib) {
   void *handle;
   
   if (!(handle = dlopen(lib, RTLD_LAZY))) {
      FAIL(IDENT, "Can't open library", lib);
      exit(-1);
   }
   
   return handle;
}

/*---------------------------------------------------------------------*/
/*    void *(*)()                                                      */
/*    get_function ...                                                 */
/*---------------------------------------------------------------------*/
fun_t
get_function(void *handle, char *id) {
   char *err;
   fun_t fun = dlsym(handle, id);

   if (bmem_verbose >= 2) {
      fprintf(stderr, "  %s...", id);
   }
   if (!fun || (err = dlerror())) {
      FAIL(IDENT, "Can't find function", id);
      exit(-2);
   } else {
      if (bmem_verbose >= 2) {
	 fprintf(stderr, "ok\n");
      }
      return fun;
   }
}

/*---------------------------------------------------------------------*/
/*    void *(*)()                                                      */
/*    find_function ...                                                */
/*---------------------------------------------------------------------*/
fun_t
find_function(void *handle, char *id) {
   char *err;
   fun_t fun = dlsym(handle, id);

   if (bmem_verbose >= 2) {
      fprintf(stderr, "  %s...", id);
   }
   if (!fun ||(err = dlerror())) {
      if (bmem_verbose >= 2) {
	 fprintf(stderr, "no\n");
      }
      return(fun_t)&unbound;
   } else {
      if (bmem_verbose >= 2) {
	 fprintf(stderr, "ok\n");
      }
      return fun;
   }
}

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    get_variable ...                                                 */
/*---------------------------------------------------------------------*/
void *
get_variable(void *handle, char *id) {
   char *err;
   fun_t fun = dlsym(handle, id);

   if (bmem_verbose >= 2) {
      fprintf(stderr, "  %s...", id);
   }
   if (!fun ||(err = dlerror())) {
      FAIL(IDENT, "Can't find variable", id);
      exit(-2);
   } else {
      if (bmem_verbose >= 2) {
	 fprintf(stderr, "ok\n");
      }
      return fun;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    dump_statistics ...                                              */
/*---------------------------------------------------------------------*/
static void
dump_statistics() {
   char *n = getenv("BMEMMON");
   char *fmt = getenv("BMEMFORMAT");
   char *e = 0L;
   FILE *f = 0L;
   int bmemdumpfmt = BMEMDUMPFORMAT_NONE;
   char *suf = "";

   if (fmt && !strcmp(fmt, "json")) {
      bmemdumpfmt = BMEMDUMPFORMAT_JSON;
      suf = ".json";
   }
      
   if (fmt && !strcmp(fmt, "sexp")) {
      bmemdumpfmt = BMEMDUMPFORMAT_SEXP;
      suf = ".bmem";
   }
      

   if (!n) {
      if (____executable_name && *____executable_name) {
	 char *s1 = strrchr(*____executable_name, '/');
	 char *s2 = strrchr(s1 ? s1 + 1: *____executable_name,  '.');
	 char *s = s1 ? s1 + 1: *____executable_name;
	 int l = strlen(s);
	 char *r = malloc(l + 6);

	 e = *____executable_name;
	 
	 if (s2) {
	    strcpy(r, s);
	    strcpy(&r[ s2 - s ], suf);
	 } else {
	    sprintf(r, "%s%s", s, suf);
	 }
	 n = r;
      } else {
	 char *r = malloc(7);
	 e = "???";
	 sprintf(r, "a%s", suf);
	 n = r;
      }
   }

   if (bmem_verbose >= 1 && n) {
      if (bmem_color) {
	 fprintf(stderr, "\nDumping file \"[0m[1;34m%s[0m\"...\n", n);
      } else {
	 fprintf(stderr, "\nDumping file \"%s\"...\n", n);
      }
      fflush(stderr);
   }
   
   if (n && !(f = fopen(n, "w"))) {
      FAIL(IDENT, "Can't open output file", n);
   }

   if (bmemdumpfmt == BMEMDUMPFORMAT_JSON) {
      // json dump
      fprintf(f, "{\n  \"monitor\": { \"info\": { \"exec\": \"%s\", \"version\": \"%s\", \"sizeWord\": %d }},\n", e, VERSION, BMEMSIZEOFWORD);
      GC_dump_statistics_json(f);
      alloc_dump_statistics_json(f);
      fprintf(f, "  \"kbtotal-size\": %lld\n", GC_alloc_total() / 1024);
      fprintf(f, "}\n");
   } else if (bmemdumpfmt == BMEMDUMPFORMAT_SEXP) {
      // text dump
      fprintf(f, "(monitor (info (exec \"%s\") (version \"%s\") (sizeof-word %d))\n",
	       e, VERSION, BMEMSIZEOFWORD);
      GC_dump_statistics_sexp(f);
      alloc_dump_statistics_sexp(f);
      fprintf(f, ")\n");
      
      fprintf(stderr, "Total size: %lldMB(%lldKB)\n",
	      GC_alloc_total() / 1024 / 1024, GC_alloc_total() / 1024);
   } else {
      fprintf(stderr, "\n(export \"BMEMVERBOSE=0\" to disable bmem messages)\n");
      fprintf(stderr, "(export \"BMEMFORMAT=json\" to generate json format)\n");
      fprintf(stderr, "(export \"BMEMFORMAT=sexp\" to generate sexp format)\n\n");
   }

   fflush(stderr);
   fflush(stdout);

   if (f) fclose(f);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_bmem_reset ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_bmem_reset() {
   GC_reset_statistics();
   //alloc_reset_statistics();
   //thread_reset_statistics();

   fprintf(stderr, "bmem reset\n");
   
   return BTRUE;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bigloo_abort ...                                                 */
/*---------------------------------------------------------------------*/
int
bigloo_abort(long n) {
   return n;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_dump ...                                                    */
/*---------------------------------------------------------------------*/
static void
bmem_dump(int _) {
   static int indump = 0;

   if (!indump) {
      indump = 1;

      if (getenv("BMEMCOLOR")) {
	 if (!strcmp(getenv("BMEMCOLOR"), "no")) {
	    bmem_color = 0;
	 }
      } else {
	 bmem_color = isatty(fileno(stdout));
      }
      
      ____GC_gcollect();
      dump_statistics();
      alloc_dump_statistics();
      indump = 0;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_bmem ...                                            */
/*---------------------------------------------------------------------*/
void
bglpth_setup_bmem() {
   void *hdl;
   char bigloothread_lib[ 1000 ];
   static void(*____bglthread_setup_bmem)();
   
   bmem_thread = 2;

   /* Hello world */
   if (getenv("BMEMVERBOSE")) {
      bmem_verbose = atoi(getenv("BMEMVERBOSE"));
   }
   
   if (getenv("BMEMLIBBIGLOOTHREAD")) {
      strcpy(bigloothread_lib, getenv("BMEMLIBBIGLOOTHREAD"));
   } else {
      sprintf(bigloothread_lib, "%s/libbigloopthread_s_mt-%s.%s",
	       LIBRARY_DIRECTORY, BGL_RELEASE_NUMBER,
	       SHARED_LIB_SUFFIX);
   }

   if (bmem_verbose >= 2) {
      fprintf(stderr, "Loading thread library %s...\n", bigloothread_lib);
   }

   hdl = open_shared_library(bigloothread_lib);

   ____bglthread_setup_bmem =(void(*)())get_function(hdl, "bglpth_setup_bmem");
   ____pthread_getspecific = get_function(hdl, "bglpth_pthread_getspecific");
   ____pthread_setspecific =(int(*)())get_function(hdl, "bglpth_pthread_setspecific");
   ____pthread_key_create =(int(*)())get_function(hdl, "bglpth_pthread_key_create");
   ____pthread_mutex_init =(int(*)())get_function(hdl, "bglpth_pthread_mutex_init");

   if (____pthread_key_create(&bmem_key, 0L)) {
      FAIL(IDENT, "Can't get thread key", "bmem_key");
      exit(-2);
   }

   if (____pthread_mutex_init(&bmem_mutex, 0L)) {
      FAIL(IDENT, "Can't get thread key", "bmem_key");
      exit(-2);
   }

   ____bglthread_setup_bmem();

   bmem_init();
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_init_inner ...                                              */
/*    -------------------------------------------------------------    */
/*    This is the standard initialization point for the Bigloo         */
/*    C runtime.                                                       */
/*---------------------------------------------------------------------*/
static void
bmem_init_inner() {
   void *hdl;
   char bigloo_lib[ 1000 ];
   char gc_lib[ 1000 ];
   char *bgllibsuffix, *bglgcsuffix = "_u";

   /* Hello world */
   if (getenv("BMEMVERBOSE")) {
      bmem_verbose = atoi(getenv("BMEMVERBOSE"));
   }
   
   /* backtrace */
   if (getenv("BMEMBACKTRACE")) {
      bmem_backtrace = atoi(getenv("BMEMBACKTRACE"));
      if (bmem_backtrace < 0 && bmem_verbose >= 1) {
	 fprintf(stderr, "Disabling stack back traces...\n");
      }
   }
   
   if (!getenv("BMEMTHREAD")) {
      if (bmem_verbose >= 1) {
	 fprintf(stderr, "Bmem initialization...\n");
      }
   } else {
      if (bmem_verbose >= 1) {
	 fprintf(stderr, "Bmem mt initialization...\n");
      }

      bglpth_setup_bmem();
   }
   
   if (getenv("BMEMLIBBIGLOO")) {
      strcpy(bigloo_lib, getenv("BMEMLIBBIGLOO"));
   } else {
      if (getenv("BMEMLIBSUFFIX")) {
	 bgllibsuffix = getenv("BMEMLIBSUFFIX");
      } else {
	 bgllibsuffix = "_s";
      }
   
      sprintf(bigloo_lib, "%s/libbigloo%s-%s.%s",
	       LIBRARY_DIRECTORY, bgllibsuffix, BGL_RELEASE_NUMBER,
	       SHARED_LIB_SUFFIX);
   }

#if (BGL_GC_CUSTOM == 1)
   if (getenv("BMEMLIBBIGLOOGC")) {
      strcpy(gc_lib, getenv("BMEMLIBBIGLOOGC"));
   } else {
      if (getenv("BMEMGCSUFFIX")) {
	 bglgcsuffix = getenv("BMEMGCSUFFIX");
      } else {
	 bglgcsuffix = "";
      }
      sprintf(gc_lib, "%s/lib%s%s-%s.%s",
	       LIBRARY_DIRECTORY,
	       BGL_GC_LIBRARY,
	       bglgcsuffix,
	       BGL_RELEASE_NUMBER,
	       SHARED_LIB_SUFFIX);
   }
#else
   strcpy(gc_lib, BGL_GC_LIBRARY);
#endif
   if (getenv("BMEMDEBUG"))
      bmem_debug = atoi(getenv("BMEMDEBUG"));

   /* The GC library */
   if (bmem_verbose >= 1) {
      fprintf(stderr, "Loading library %s...\n", gc_lib);
   }
   hdl = open_shared_library(gc_lib);
   ____GC_malloc = get_function(hdl, "GC_malloc");
   ____GC_realloc = get_function(hdl, "GC_realloc");
   ____GC_malloc_atomic = get_function(hdl, "GC_malloc_atomic");
   ____GC_malloc_uncollectable = get_function(hdl, "GC_malloc_uncollectable");
   ____GC_add_gc_hook = get_function(hdl, "GC_add_gc_hook");
   ____GC_gcollect =(void(*)())get_function(hdl, "GC_gcollect");
   
   ____GC_add_gc_hook(GC_collect_hook);
   ____GC_reset_allocated_bytes =(void(*)())get_function(hdl, "GC_reset_allocated_bytes");

   /* The Bigloo library */
   if (bmem_verbose >= 1) {
      fprintf(stderr, "Loading library %s...\n", bigloo_lib);
   }
   hdl = open_shared_library(bigloo_lib);
   ____executable_name = get_variable(hdl, "executable_name");
   ____command_line = get_variable(hdl, "command_line");
   ____bgl_init_objects =(void(*)())get_function(hdl, "bgl_init_objects");
   ____get_hash_power_number =(long(*)())get_function(hdl, "get_hash_power_number");
   ____get_hash_power_number_len =(long(*)())get_function(hdl, "get_hash_power_number_len");
   ____bgl_get_symtab = get_function(hdl, "bgl_get_symtab");
   ____bgl_current_nanoseconds =(BGL_LONGLONG_T(*)())get_function(hdl, "bgl_current_nanoseconds");

   ____bgl_init_trace_register =(void(*)())get_function(hdl, "bgl_init_trace_register");
   
   /* class */
   ____register_class = get_function(hdl, "BGl_registerzd2classz12zc0zz__objectz00");
   ____bgl_types_number =(int(*)())get_function(hdl, "bgl_types_number");

   /* backtrace init */
   backtrace_init();
   
   /* procedure */
   bmem_init_wrapper(hdl);

   /* declare types */
   declare_type(NO_TYPE_NUM, "-", 0L);
   declare_type(UNKNOWN_TYPE_NUM, "?", 0L);
   declare_type(UNKNOWN_ATOMIC_TYPE_NUM, "?a", 0L);
   declare_type(UNKNOWN_UNCOLLECTABLE_TYPE_NUM, "?u", 0L);
   declare_type(UNKNOWN_REALLOC_TYPE_NUM, "?r", 0L);
   declare_type(_DYNAMIC_ENV_TYPE_NUM, "%dynamic-env", 0L);
   declare_type(_THREAD_TYPE_NUM, "%native-thread", 0L);
   declare_type(ROWSTRING_TYPE_NUM, "char *", 0L);
   declare_type(LLONG_TYPE_NUM, "llong", 0L);
   declare_type(ELONG_TYPE_NUM, "elong", 0L);
   declare_type(PROCEDURE_LIGHT_TYPE_NUM, "procedure-light", 0L);
   declare_type(TSTRUCT_TYPE_NUM, "tstruct", 0L);
   declare_type(TVECTOR_TYPE_NUM, "tvector", 0L);
   declare_type(EXTENDED_PAIR_TYPE_NUM, "epair", 0L);
   declare_type(BINARY_PORT_TYPE_NUM, "binary-port", 0L);
   declare_type(OUTPUT_STRING_PORT_TYPE_NUM, "output-string-port", 0L);
   declare_type(FOREIGN_TYPE_NUM, "foreign", 0L);
   declare_type(PROCESS_TYPE_NUM, "process", 0L);
   declare_type(REAL_TYPE_NUM, "real", 0L);
   declare_type(STRUCT_TYPE_NUM, "struct", 0L);
   declare_type(SOCKET_TYPE_NUM, "socket", 0L);
   declare_type(CELL_TYPE_NUM, "cell", 0L);
   declare_type(DATE_TYPE_NUM, "date", 0L);
   declare_type(OUTPUT_PORT_TYPE_NUM, "output-port", 0L);
   declare_type(INPUT_PORT_TYPE_NUM, "input-port", 0L);
   declare_type(STACK_TYPE_NUM, "stack", 0L);
   declare_type(SYMBOL_TYPE_NUM, "symbol", 0L);
   declare_type(KEYWORD_TYPE_NUM, "keyword", 0L);
   declare_type(CUSTOM_TYPE_NUM, "custom", 0L);
   declare_type(OPAQUE_TYPE_NUM, "opaque", 0L);
   declare_type(UCS2_STRING_TYPE_NUM, "ucs2-string", 0L);
   declare_type(PROCEDURE_TYPE_NUM, "procedure", 0L);
   declare_type(VECTOR_TYPE_NUM, "vector", 0L);
   declare_type(STRING_TYPE_NUM, "string", 0L);
   declare_type(PAIR_TYPE_NUM, "pair", 0L);
   declare_type(HOSTENT_TYPE_NUM, "hostent", 0L);
   declare_type(PORT_TIMEOUT_TYPE_NUM, "port-timeout", 0L);
   declare_type(CLASS_TYPE_NUM, "class", 0L);
   declare_type(DATAGRAM_SOCKET_TYPE_NUM, "datagram-socket", 0L);
   declare_type(REGEXP_TYPE_NUM, "regexp", 0L);
   declare_type(INT32_TYPE_NUM, "int32", 0L);
   declare_type(UINT32_TYPE_NUM, "uint32", 0L);
   declare_type(INT64_TYPE_NUM, "int64", 0L);
   declare_type(UINT64_TYPE_NUM, "uint64", 0L);
   declare_type(MUTEX_TYPE_NUM, "mutex", 0L);
   declare_type(SPINLOCK_TYPE_NUM, "spinlock", 0L);
   declare_type(CONDVAR_TYPE_NUM, "condvar", 0L);
   declare_type(BIGNUM_TYPE_NUM, "bignum", 0L);

   /* alloc init */
   alloc_init(native_allocators);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_init ...                                                    */
/*---------------------------------------------------------------------*/
static void
bmem_init() {
   static int initp = 0;

   if (!initp) {
      initp = 1;
      bmem_init_inner();
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_objects ...                                             */
/*    -------------------------------------------------------------    */
/*    This is the standard initialization point for the Bigloo         */
/*    C runtime.                                                       */
/*---------------------------------------------------------------------*/
void
bgl_init_objects() {
   /* initialize the preloading */
   bmem_init();

   /* initialize the runtime system */
   ____bgl_init_objects();

   unknown_ident = string_to_symbol("unknown_function");

   /* signal registration */
   signal(2, bmem_dump);

   /* exit registration */
   atexit((void(*)(void))bmem_dump);
}


/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    register_class ...                                               */
/*---------------------------------------------------------------------*/
obj_t
BGl_registerzd2classz12zc0zz__objectz00(obj_t name, obj_t module, obj_t super,
					 long hash,
					 obj_t creator, obj_t ator, obj_t ctor,
					 obj_t nil, obj_t shrink,
					 obj_t plain, obj_t virtual) {
   static int init = 0;
   char tmp[256];
   obj_t alloc;
   char *cname = BSTRING_TO_STRING(SYMBOL_TO_STRING(name));
   int tnum = ____bgl_types_number();
   obj_t class;

   if (!init) {
      if (bmem_verbose >= 2) {
	 fprintf(stderr, "Defining classes...\n");
      }
      init = 1;
   }

   declare_type(tnum, cname, BSTRING_TO_STRING(SYMBOL_TO_STRING(module)));

   class = ____register_class(name, module, super,
			      hash,
			      creator, ator, ctor,
			      nil, shrink,
			      plain, virtual);

   return class;
}
