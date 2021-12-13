/*=====================================================================*/
/*    .../bigloo/bigloo/api/libbacktrace/src/Clib/bglbacktrace.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec  2 14:27:09 2021                          */
/*    Last change :  Sun Dec  5 08:40:20 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Optional libbacktrace Bigloo binding                             */
/*=====================================================================*/
#include <bigloo.h>
#include <backtrace.h>

/*---------------------------------------------------------------------*/
/*    imports                                                          */
/*---------------------------------------------------------------------*/
extern bool_t bigloo_mangledp(obj_t);
extern obj_t bigloo_module_demangle(obj_t);

/*---------------------------------------------------------------------*/
/*    struct getinfo                                                   */
/*---------------------------------------------------------------------*/
struct getinfo {
   long depth;
   long start;
   obj_t pair;
};
   
/*---------------------------------------------------------------------*/
/*    orig_init_trace ...                                              */
/*---------------------------------------------------------------------*/
static void (*orig_init_trace)(obj_t) = 0L;
static obj_t linesym = 0L;

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    backtrace_foreach_cb ...                                         */
/*---------------------------------------------------------------------*/
static int
backtrace_foreach_cb(void *data, uintptr_t pc, const char *filename, int lineno, const char *function) {
   obj_t proc = (obj_t)data;
   
   return PROCEDURE_ENTRY(proc)
      (proc,
       filename ? string_to_bstring((char *)filename) : BUNSPEC,
       BINT(lineno),
       function ? string_to_bstring((char *)function) : BUNSPEC,
       BEOA)
      != BFALSE;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    backtrace_get_cb ...                                             */
/*---------------------------------------------------------------------*/
static int
backtrace_get_cb(void *data, uintptr_t pc, const char *filename, int lineno, const char *function) {
   struct getinfo *info = (struct getinfo *)data;

   if (!linesym) {
      linesym = string_to_symbol("line");
   }

   if (info->start-- > 0) {
      return 0;
   } if (!filename && !function) {
      /* nothing relevant */
      return info->depth-- == 0;
   } else {
      obj_t fun = function ? string_to_bstring((char *)function) : BUNSPEC;
      obj_t file = filename ? string_to_bstring((char *)filename) : BUNSPEC;
      obj_t pline = MAKE_PAIR(BINT(lineno), BNIL);
      obj_t pfile = MAKE_PAIR(file, pline);
      obj_t pat = MAKE_PAIR(linesym, pfile);
      obj_t rest = MAKE_PAIR(pat, BNIL);
      obj_t entry = MAKE_PAIR(fun, rest);

      /* a Scheme function */
      if (fun != BUNSPEC && bigloo_mangledp(fun)) {
	 SET_CAR(entry, bigloo_module_demangle(fun));
      }
      
      SET_CDR(info->pair, MAKE_PAIR(entry, BNIL));
      info->pair = CDR(info->pair);
      return info->depth-- == 0;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    cbe ...                                                          */
/*---------------------------------------------------------------------*/
static void cbe(void *data, const char *msg, int errnum) {
   fprintf(stderr, "*** BACKTRACE ERROR:%s (%d)\n", msg, errnum);
}

/*---------------------------------------------------------------------*/
/*    static struct backtrace_state *                                  */
/*    libbacktrace_get_state ...                                       */
/*---------------------------------------------------------------------*/
static struct backtrace_state *
libbacktrace_get_state(obj_t env) {
   if (!BGL_ENV_GET_BACKTRACE(env)) {
      struct backtrace_state *bt_state = backtrace_create_state(0L, 0, cbe, 0L);
      
      BGL_ENV_SET_BACKTRACE(env, (struct bgl_dframe *)bt_state);
      return bt_state;
   } else {
      return BGL_ENV_GET_BACKTRACE(env);
   }
}

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
/*    bgl_backtrace_foreach ...                                        */
/*---------------------------------------------------------------------*/
void
bgl_backtrace_foreach(void *proc) {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();

   if (env) {
      struct backtrace_state *bt_state = libbacktrace_get_state(env);
      backtrace_full(bt_state, 0, backtrace_foreach_cb, cbe, proc);
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_backtrace_get ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_backtrace_get(long depth, long start) {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();

   if (env) {
      struct backtrace_state *bt_state = libbacktrace_get_state(env);
      obj_t pair = MAKE_PAIR(BNIL, BNIL);
      struct getinfo info = { depth = depth, start = start, pair = pair };

      backtrace_full(bt_state, start, backtrace_get_cb, cbe, &info);

      return CDR(pair);
   } else {
      return BNIL;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    libbacktrace_get ...                                             */
/*---------------------------------------------------------------------*/
static obj_t
libbacktrace_get(int depth) {
   return bgl_backtrace_get(depth, 0);
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_backtrace ...                                           */
/*---------------------------------------------------------------------*/
void bgl_init_backtrace() {
   orig_init_trace = bgl_init_trace;
   bgl_init_trace_register(&libbacktrace_init, &libbacktrace_get, 0L);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglbacktrace_setup ...                                           */
/*---------------------------------------------------------------------*/
void
bglbacktrace_setup(int argc, char *argv[], char *env[]) {
   return bgl_init_backtrace();
}
