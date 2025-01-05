/*=====================================================================*/
/*    serrano/prgm/project/bigloo/wasm/runtime/Clib/ctrace.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Mar 31 18:06:36 1995                          */
/*    Last change :  Fri Dec  8 09:48:32 2023 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Execution traces (mainly for error reporting)                    */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Stack registrations                                              */
/*---------------------------------------------------------------------*/
static void default_init_trace(obj_t);
static obj_t default_get_trace_stack(int);
static void default_walk_trace_stack(obj_t);
static obj_t no_get_trace_stack(int);
static void no_walk_trace_stack(obj_t);

BGL_RUNTIME_DEF void (*bgl_init_trace)(obj_t) = &default_init_trace;
BGL_RUNTIME_DEF obj_t (*bgl_get_trace_stack)(int) = &default_get_trace_stack;
BGL_RUNTIME_DEF void (*bgl_walk_trace_stack)(obj_t) = &default_walk_trace_stack;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_trace_register ...                                      */
/*---------------------------------------------------------------------*/
void
bgl_init_trace_register(void (*i)(), obj_t (*g)(int), void (*w)(obj_t)) {
   bgl_init_trace = i;

   bgl_get_trace_stack = (g ? g : no_get_trace_stack);
   bgl_walk_trace_stack = (w ? w : no_walk_trace_stack);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    default_init_trace ...                                           */
/*---------------------------------------------------------------------*/
static void
default_init_trace(obj_t env) {
   BGL_DYNAMIC_ENV(env).top.name = BUNSPEC;
   BGL_DYNAMIC_ENV(env).top.location = BUNSPEC;
   BGL_DYNAMIC_ENV(env).top.link = 0;

   BGL_ENV_SET_TOP_OF_FRAME(env, &(BGL_DYNAMIC_ENV(env).top));
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    default_get_trace_stack ...                                      */
/*---------------------------------------------------------------------*/
static obj_t
default_get_trace_stack(int depth) {
   long level = 0L;
   struct bgl_dframe *runner = BGL_ENV_GET_TOP_OF_FRAME(BGL_CURRENT_DYNAMIC_ENV());
   obj_t l = MAKE_PAIR(BNIL, BNIL);
   obj_t r = l;

   while (((depth < 0) || (level < depth)) && runner) {
      if (SYMBOLP( runner->name) || STRINGP(runner->name)) {
	 obj_t p = MAKE_PAIR(runner->name, MAKE_PAIR(runner->location, BNIL));
	 SET_CDR(r, MAKE_PAIR(p, BNIL));
	 r = CDR(r);
	 level++; 
      }
      
      runner = runner->link;
   }
   
   return CDR(l);
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    no_get_trace_stack ...                                           */
/*---------------------------------------------------------------------*/
static obj_t
no_get_trace_stack(int depth) {
   return BNIL;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    default_walk_trace_stack ...                                     */
/*---------------------------------------------------------------------*/
static void
default_walk_trace_stack(obj_t proc) {
   struct bgl_dframe *runner = BGL_ENV_GET_TOP_OF_FRAME(BGL_CURRENT_DYNAMIC_ENV());

_loop: {
      if (SYMBOLP( runner->name) || STRINGP(runner->name)) {
	 obj_t p = MAKE_PAIR(runner->name, MAKE_PAIR(runner->location, BNIL));
	 if (BGL_PROCEDURE_CALL1(proc, p) != BFALSE) {
	    goto _loop;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    default_walk_trace_stack ...                                     */
/*---------------------------------------------------------------------*/
static void
no_walk_trace_stack(obj_t proc) {
   ;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    cref ...                                                         */
/*---------------------------------------------------------------------*/
obj_t
cref(obj_t obj) {
   return CREF(obj);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    car ...                                                          */
/*---------------------------------------------------------------------*/
obj_t
car(obj_t obj) {
   return CAR(obj);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    cdr ...                                                          */
/*---------------------------------------------------------------------*/
obj_t
cdr(obj_t obj) {
   return CDR(obj);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    byteshow ...                                                     */
/*---------------------------------------------------------------------*/
static void
byteshow(unsigned char *addr) {
#define PP_CHAR(c) (((c) >= 33) && ((c) < 127)) ? c : '.'

   printf("  %08lx  :  %02x %02x %02x %02x  :  %c%c%c%c\n",
           (unsigned long)addr,
           addr[ 0 ],
           addr[ 1 ],
           addr[ 2 ],
           addr[ 3 ],
           PP_CHAR(addr[ 0 ]),
           PP_CHAR(addr[ 1 ]),
           PP_CHAR(addr[ 2 ]),
           PP_CHAR(addr[ 3 ]));
}
             
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    memshow ...                                                      */
/*---------------------------------------------------------------------*/
void
memshow(char *from, char *to, long step) {
   char *i;

   step *= 4;
      
   if (from > to) 
      for (i = from; i > to; i -= step)
         byteshow((unsigned char *)i);
   else
      for (i = from; i < to; i += step)
         byteshow((unsigned char *)i);

   puts("");
   return ;
}
