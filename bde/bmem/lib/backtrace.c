/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/backtrace.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Oct  6 15:37:29 2021                          */
/*    Last change :  Mon Dec  6 09:18:25 2021 (serrano)                */
/*    Copyright   :  2021 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    libbacktrace interface                                           */
/*=====================================================================*/
#include <strings.h>
#include <bigloo.h>
#include <bmem.h>

/*---------------------------------------------------------------------*/
/*    import                                                           */
/*---------------------------------------------------------------------*/
extern obj_t bigloo_demangle(obj_t);

/*---------------------------------------------------------------------*/
/*    orig_init_trace ...                                              */
/*---------------------------------------------------------------------*/
void (*orig_init_trace)(obj_t) = 0L;

/*---------------------------------------------------------------------*/
/*    hashtable for function names                                     */
/*---------------------------------------------------------------------*/
static hashtable_t *allocators = 0L;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    backtrace_alloc_name_put ...                                     */
/*---------------------------------------------------------------------*/
void
backtrace_alloc_name_put(char *function, int tnum) {
   hashtable_put(allocators, function, (void *)((long)tnum));
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    backtrace_frame_type ...                                         */
/*---------------------------------------------------------------------*/
int
backtrace_frame_type(const char *filename, const char *function) {
   long n = (long)hashtable_get(allocators, function);
   return n ? (int)n : -1;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    backtrace_alloc_cb ...                                           */
/*---------------------------------------------------------------------*/
int
backtrace_alloc_cb(void *data, uintptr_t pc, const char *filename, int lineno, const char *function) {
   alloc_info_t *info = (alloc_info_t *)data;

   if (!function) {
      return 0;
   }

   if (!info->filename && function && !alloc_is_native(function)) {
      if (strcmp(filename, "lib/wrapper.c")) {
	 info->filename = (char *)filename;
	 info->lineno = lineno;
      } else {
	 return 0;
      }
   }
   
   if (info->typenum == -1 || info->typenum == NO_TYPE_NUM) {
      info->typenum = backtrace_frame_type(filename, function);
   }
   
   if (info->typenum != -1 && info->filename) {
      return 1;
   }

   return info->depth-- == 0;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    cbe ...                                                          */
/*---------------------------------------------------------------------*/
static void cbe(void *data, const char *msg, int errnum) {
   fprintf(stderr, "*** BACKTRACE ERROR:%s (%d)\n", msg, errnum);
}

/*---------------------------------------------------------------------*/
/*    struct backtrace_state *                                         */
/*    libbacktrace_get_state ...                                       */
/*---------------------------------------------------------------------*/
static struct backtrace_state *
libbacktrace_get_state(obj_t env) {
#if BGL_HAVE_BACKTRACE   
   if (!BGL_ENV_GET_BACKTRACE(env)) {
      struct backtrace_state *bt_state = backtrace_create_state(0L, 0, cbe, 0L);
      
      BGL_ENV_SET_BACKTRACE(env, (struct bgl_dframe *)bt_state);
      return bt_state;
   } else {
      return BGL_ENV_GET_BACKTRACE(env);
   }
#endif
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    backtrace_full ...                                               */
/*---------------------------------------------------------------------*/
#if !BGL_HAVE_BACKTRACE   
static void
backtrace_full(obj_t env, int start, int (*cb)(), void (*ce)(), void *data) {
   struct bgl_dframe *runner = BGL_ENV_GET_TOP_OF_FRAME(env);
							   
   while (start-- > 0) {
      if (!runner) {
	 return ;
      }
      runner = runner->link;
   }
   
   while (runner) {
      int res;
      char *filename = "bigloo";
      obj_t name = runner->name;
      char *function = 0L;
      int at = 1;
      
      if (SYMBOLP(runner->name)) {
	 name = SYMBOL_TO_STRING(runner->name);
      }

      if (STRINGP(name)) {
	 function = BSTRING_TO_STRING(name);
	 if (PAIRP(runner->location)) {
	    filename = BSTRING_TO_STRING(CAR(CDR(runner->location)));
	    at = CINT(CAR(CDR(CDR(runner->location))));
	 }
      
	 if (cb(data, 0L, filename, at, function)) {
	    return;
	 } else {
	    runner = runner->link;
	 }
      }
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    libbacktrace_init ...                                            */
/*---------------------------------------------------------------------*/
static void
libbacktrace_init(obj_t env) {
   orig_init_trace(env);
   libbacktrace_get_state(env);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    backtrace_for_each ...                                           */
/*---------------------------------------------------------------------*/
void
backtrace_for_each(backtrace_full_callback proc, int start, void *data) {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();

   if (env) {
#if BGL_HAVE_BACKTRACE   
      struct backtrace_state *bt_state =
	 (struct backtrace_state *)BGL_ENV_GET_BACKTRACE(env);
#else
      obj_t bt_state = env;
#endif      
      backtrace_full(bt_state, start, proc, cbe, data);
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    backtrace_init ...                                               */
/*---------------------------------------------------------------------*/
void backtrace_init() {
   orig_init_trace = bgl_init_trace;
   allocators = hashtable_create(128);

#if BGL_HAVE_BACKTRACE   
   ____bgl_init_trace_register(&libbacktrace_init, 0L, 0L);
#endif   
}
