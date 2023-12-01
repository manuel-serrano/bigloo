/*=====================================================================*/
/*    .../bigloo/bigloo/api/libbacktrace/src/Clib/bglbacktrace.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec  2 14:27:09 2021                          */
/*    Last change :  Mon Feb 21 10:10:12 2022 (serrano)                */
/*    Copyright   :  2021-22 Manuel Serrano                            */
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
   obj_t env;
   char ineval;
   long depth;
   long start;
   obj_t pair;
   struct bgl_dframe *runner;
};
   
/*---------------------------------------------------------------------*/
/*    orig_init_trace ...                                              */
/*---------------------------------------------------------------------*/
static void (*orig_init_trace)(obj_t) = 0L;
static obj_t (*orig_get_trace_stack)(int) = 0L;
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
/*    static void                                                      */
/*    eval_get_trace_stack ...                                         */
/*---------------------------------------------------------------------*/
static void
eval_get_trace_stack(struct getinfo *info) {
   struct bgl_dframe *runner = info->runner;
   obj_t l = MAKE_PAIR(BNIL, BNIL);
   obj_t r = l;

   while ((info->depth > 1) && runner) {
      if (SYMBOLP(runner->name) || STRINGP(runner->name)) {
	 obj_t fun = runner->name;
	 obj_t file = runner->location;
	 obj_t rest = MAKE_PAIR(file, BNIL);
	 obj_t entry = MAKE_PAIR(fun, rest);

	 SET_CDR(info->pair, MAKE_PAIR(entry, BNIL));
	 info->pair = CDR(info->pair);
	 info->depth--;
      } else if (KEYWORDP(runner->name)) {
	 // a mark that stop the stack walk
	 runner = runner->link;
	 break;
      }
      
      runner = runner->link;
   }
   info->runner = runner;
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
	 obj_t dm = bigloo_module_demangle(fun);
	 long len = STRING_LENGTH(dm);
	 char *s = BSTRING_TO_STRING(dm);
	 char ec[] = "@__evaluate_comp";
	 char ea[] = "@__evaluate";
	 char ev[] = "@__eval";

	 if ((len > sizeof(ec)) && !strcmp(&(s[len-sizeof(ec)+1]), ec)) {
	    /* grab the portion of the Scheme eval trace stack */
	    if (!info->ineval) {
	       info->ineval = 1;
	       eval_get_trace_stack(info);
	       return info->depth-- == 0;
	    } else {
	       return 0;
	    }
	 } else if (!strcmp(&(s[len-sizeof(ea)+1]), ea)) {
	    return 0;
	 } else if (!strcmp(&(s[len-sizeof(ev)+1]), ev)) {
	    return 0;
	 } else {
	    info->ineval = 0;
	    SET_CAR(entry, dm);
	 }
      
	 SET_CDR(info->pair, MAKE_PAIR(entry, BNIL));
	 info->pair = CDR(info->pair);
	 return info->depth-- == 0;
      } else {
	 return 0;
      }
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
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_backtrace_get ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_backtrace_get(long depth, long start) {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   obj_t res;

   if (env) {
      struct backtrace_state *bt_state = libbacktrace_get_state(env);
      obj_t pair = MAKE_PAIR(BNIL, BNIL);
      struct getinfo info = { env, 0, depth, start, pair, BGL_ENV_GET_TOP_OF_FRAME(env) };

      backtrace_full(bt_state, start, backtrace_get_cb, cbe, &info);

      res = CDR(pair);
   } else {
      res = BNIL;
   }

   if (NULLP(res)) {
      return orig_get_trace_stack(depth);
   } else {
      return res;
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
   orig_get_trace_stack = bgl_get_trace_stack;
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
