/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cpcre2.h         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Apr  7 13:48:10 2021                          */
/*    Last change :  Fri Dec  8 19:06:27 2023 (serrano)                */
/*    Copyright   :  2021-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo PCRE2 binding                                             */
/*=====================================================================*/
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

extern obj_t make_string(long, unsigned char);

static obj_t utf8_symbol = BUNSPEC;
static obj_t javascript_symbol = BUNSPEC;
static obj_t caseless_symbol = BUNSPEC;
static obj_t multiline_symbol = BUNSPEC;
static obj_t noraise_symbol = BUNSPEC;
static obj_t anchored_symbol = BUNSPEC;

static pcre2_general_context *pcre2_context = 0L;

#if (!defined(PCRE_JAVASCRIPT_COMPAT))
#  define PCRE_JAVASCRIPT_COMPAT 0
#endif	    

#if (!defined(PCRE_NEWLINE_ANY))
#  define PCRE_NEWLINE_ANY 0
#endif	    

#define BGL_REGEXP_PCRE2(o) (pcre2_code *)(BGL_REGEXP_PREG(o))
#define BGL_REGEXP_CHAR(o) (char)(long)(BGL_REGEXP_PREG(o))

#define PCRE_BGLNORAISE PCRE2_DUPNAMES

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_pcre2_options_init ...                                       */
/*---------------------------------------------------------------------*/
static void
bgl_pcre2_options_init() {
   if (utf8_symbol == BUNSPEC) {
      utf8_symbol = string_to_symbol("UTF8");
      javascript_symbol = string_to_symbol("JAVASCRIPT_COMPAT");
      caseless_symbol = string_to_symbol("CASELESS");
      multiline_symbol = string_to_symbol("MULTILINE");
      noraise_symbol = string_to_symbol("NORAISE");
      anchored_symbol = string_to_symbol("ANCHORED");
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_pcre2_options ...                                            */
/*---------------------------------------------------------------------*/
static int
bgl_pcre2_options(obj_t args) {
   int options = PCRE2_ALLOW_EMPTY_CLASS;

   if (PAIRP(args)) {
      bgl_pcre2_options_init();
   
      while (PAIRP(args)) {
	 if (CAR(args) == utf8_symbol) {
	    options |= PCRE2_UTF;
	 } else if (CAR(args) == caseless_symbol) {
	    options |= PCRE2_CASELESS;
	 } else if (CAR(args) == javascript_symbol) {
	    options |= PCRE2_MATCH_UNSET_BACKREF;
	 } else if (CAR(args) == multiline_symbol) {
	    options |= PCRE2_MULTILINE | PCRE_NEWLINE_ANY;
	 } else if (CAR(args) == anchored_symbol) {
	    options |= PCRE2_ANCHORED;
	 } else if (CAR(args) == noraise_symbol) {
	    options |= PCRE_BGLNORAISE;
	 } else {
	    if (CAR(args) != BFALSE) {
	       C_SYSTEM_FAILURE(BGL_IO_PARSE_ERROR, "pregexp",
				 "Illegal PCRE option", CAR(args));
	       return 0;
	    }
	 }

	 args = CDR(args);
      }
   }

   return options;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regfree ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
bgl_regfree(obj_t re) {
   pcre2_code *rx = BGL_REGEXP_PCRE2(re);

   if (BGL_REGEXP_PCRE2(re)) {
      pcre2_code_free(BGL_REGEXP_PCRE2(re));
      BGL_REGEXP_PREG(re) = 0L;
   }

   if (BGL_REGEXP(re).match_data) {
      pcre2_match_data_free(BGL_REGEXP(re).match_data);
      BGL_REGEXP(re).match_data = 0;
   }
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regmatch ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
bgl_regmatch(obj_t re, char *string, bool_t stringp, int beg, int len, int offset) {
   int oveccount = BGL_REGEXP(re).capturecount + 1;
   int r;

   if (!BGL_REGEXP(re).match_data) {
      BGL_REGEXP(re).match_data =
	 pcre2_match_data_create_from_pattern(BGL_REGEXP_PCRE2(re), pcre2_context);
   }
   
   string += offset;

   r = pcre2_jit_match(BGL_REGEXP_PCRE2(re), 
			(PCRE2_SPTR8)string, len,
			beg, 0, BGL_REGEXP(re).match_data, 0L);

   if (r < 0) {
      return BFALSE;
   } else {
      int i;
      obj_t tmp;
      obj_t res = MAKE_STACK_PAIR_TMP(BNIL, BNIL, tmp);
      obj_t tail = res;
      PCRE2_SIZE *ovect = pcre2_get_ovector_pointer(BGL_REGEXP(re).match_data);

      for (i = 0; i < oveccount * 2; i += 2) {
	 if ((long)(ovect[ i ]) < 0) {
	    SET_CDR(tail, MAKE_PAIR(BFALSE, BNIL));
	 } else {
	    obj_t s = stringp
	       ? string_to_bstring_len(
		  &string[ ovect[ i ] ],
		  ovect[ i + 1 ] - ovect[ i ])
	       : MAKE_PAIR(
		  BINT(ovect[ i ]),
		  BINT(ovect[ i + 1 ]));
	    
	    SET_CDR(tail, MAKE_PAIR(s, BNIL));
	 }
	 tail = CDR(tail);
      }

      return CDR(res);
   }
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_regmatch_n ...                                               */
/*---------------------------------------------------------------------*/
static long
bgl_regmatch_n(obj_t re, char *string, obj_t vres, int beg, int len, int offset) {
   int oveccount = BGL_REGEXP(re).capturecount + 1;
   int r;

   if (!BGL_REGEXP(re).match_data) {
      BGL_REGEXP(re).match_data =
	 pcre2_match_data_create_from_pattern(BGL_REGEXP_PCRE2(re), pcre2_context);
   }
   
   string += offset;

   r = pcre2_jit_match(BGL_REGEXP_PCRE2(re), 
		       (PCRE2_SPTR8)string, len, beg, 0, BGL_REGEXP(re).match_data, 0L);

   if (r < 0) {
      return -1;
   } else {
      long i;
      long vlen = VECTOR_LENGTH(vres) & ~1;
      long end = oveccount * 2 < vlen ? oveccount * 2 : vlen;
      PCRE2_SIZE *ovect = pcre2_get_ovector_pointer(BGL_REGEXP(re).match_data);

      for (i = 0; i < end; i += 2) {
	 VECTOR_SET(vres, i, BINT((long)(ovect[ i ])));
	 VECTOR_SET(vres, i + 1, BINT((long)(ovect[ i + 1 ])));
      }
      
      return i >> 1;
   }
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    char_compile ...                                                 */
/*---------------------------------------------------------------------*/
static long
char_compile(char *string, int options) {
   return (long)(*string);
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_charmatch ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
bgl_charmatch(obj_t re, char *string, bool_t stringp, int beg, int len, int offset) {
   char c = BGL_REGEXP_CHAR(re);

   while (beg < len) {
      if (string[offset + beg++] == c) {
	 obj_t p = stringp ?
	    make_string(1, c) : MAKE_PAIR(BINT(beg - 1), BINT(beg));

	 return MAKE_PAIR(p, BNIL);
      }
   }

   return BFALSE;
}
   
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_charmatch_anchored ...                                       */
/*---------------------------------------------------------------------*/
static obj_t
bgl_charmatch_anchored(obj_t re, char *string, bool_t stringp, int beg, int len, int offset) {
   char c = BGL_REGEXP_CHAR(re);

   if (string[offset + beg] == c) {
      obj_t p = stringp ?
	 make_string(1, c) : MAKE_PAIR(BINT(beg), BINT(beg+1));

      return MAKE_PAIR(p, BNIL);
   }

   return BFALSE;
}
   
/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_charmatch_n ...                                              */
/*---------------------------------------------------------------------*/
static long
bgl_charmatch_n(obj_t re, char *string, obj_t vres, int beg, int len, int offset) {
   char c = BGL_REGEXP_CHAR(re);

   while (beg < len) {
      if (string[offset + beg++] == c) {
	 if ((VECTOR_LENGTH(vres) & ~1) > 0) {
	    VECTOR_SET(vres, 0, BINT(beg - 1));
	    VECTOR_SET(vres, 1, BINT(beg));

	    return 1;
	 } else {
	    return 0;
	 }
      }
   }

   return -1;
}
   
/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_charmatch_anchored_n ...                                     */
/*---------------------------------------------------------------------*/
static long
bgl_charmatch_anchored_n(obj_t re, char *string, obj_t vres, int beg, int len, int offset) {
   char c = BGL_REGEXP_CHAR(re);

   if (string[offset + beg] == c) {
      if ((VECTOR_LENGTH(vres) & ~1) > 0) {
	 VECTOR_SET(vres, 0, BINT(beg));
	 VECTOR_SET(vres, 1, BINT(beg+1));

	 return 1;
      } else {
	 return 0;
      }
   }

   return -1;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_charfree ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
bgl_charfree(obj_t re) {
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_pcre2_regcomp_finalize ...                                   */
/*---------------------------------------------------------------------*/
static void
bgl_pcre2_regcomp_finalize(void *re, void *_) {
   BGL_REGEXP(BREF(re)).free(BREF(re));
}

/*---------------------------------------------------------------------*/
/*    CHAR_REGEXP ...                                                  */
/*---------------------------------------------------------------------*/
#define CHAR_REGEXP(pat, options) \
   (STRING_LENGTH(pat) == 1 \
    && !strchr("$[*+?.(", STRING_REF(pat, 0)) \
    && !(options & PCRE2_CASELESS))

/*---------------------------------------------------------------------*/
/*    CHAR_ESCAPE_REGEXP ...                                           */
/*---------------------------------------------------------------------*/
#define CHAR_ESCAPE_REGEXP(pat, options) \
   (STRING_LENGTH(pat) == 2 \
    && STRING_REF(pat, 0) == '\\' \
    && strchr("\\-$[*+?.(", STRING_REF(pat, 1)) \
    && !(options & PCRE2_CASELESS))

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regcomp ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regcomp(obj_t pat, obj_t optargs, bool_t finalize) {
   obj_t re = bgl_make_regexp(pat);
   int errorcode;
   PCRE2_SIZE erroffset;
   int options = bgl_pcre2_options(optargs);
   static int init = 1000;

   if (CHAR_REGEXP(pat, options)) {
      BGL_REGEXP_PREG(re) = (void *)char_compile(BSTRING_TO_STRING(pat), options);

      if ((PCRE2_ANCHORED & options) == PCRE2_ANCHORED) {
	 BGL_REGEXP(re).match = bgl_charmatch_anchored;
	 BGL_REGEXP(re).match_n = bgl_charmatch_anchored_n;
      } else {
	 BGL_REGEXP(re).match = bgl_charmatch;
	 BGL_REGEXP(re).match_n = bgl_charmatch_n;
      }
      BGL_REGEXP(re).free = bgl_charfree;
      BGL_REGEXP(re).capturecount = 1;
      
      return re;
   } else if (CHAR_ESCAPE_REGEXP(pat, options)) {
      BGL_REGEXP_PREG(re) = (void *)char_compile(BSTRING_TO_STRING(pat) + 1, options);

      if ((PCRE2_ANCHORED & options) == PCRE2_ANCHORED) {
	 BGL_REGEXP(re).match = bgl_charmatch_anchored;
	 BGL_REGEXP(re).match_n = bgl_charmatch_anchored_n;
      } else {
	 BGL_REGEXP(re).match = bgl_charmatch;
	 BGL_REGEXP(re).match_n = bgl_charmatch_n;
      }
      BGL_REGEXP(re).free = bgl_charfree;
      BGL_REGEXP(re).capturecount = 1;
      
      return re;
   } else {
      if (!pcre2_context) {
	 pcre2_context = pcre2_general_context_create(0L, 0L, 0L);
      }

      if (finalize && !init--) { 
	 init = 1000;
	 /* force finalizers to free unused regexp */
	 GC_invoke_finalizers();
      }
   
      if ((BGL_REGEXP_PREG(re) =
	   pcre2_compile((PCRE2_SPTR)(BSTRING_TO_STRING(pat)),
			  PCRE2_ZERO_TERMINATED,
			  options & ~PCRE_BGLNORAISE,
			  &errorcode,
			  &erroffset,
			  NULL))) {
	 pcre2_jit_compile(BGL_REGEXP_PCRE2(re), PCRE2_JIT_COMPLETE);
	 pcre2_pattern_info(BGL_REGEXP_PCRE2(re),
			     PCRE2_INFO_CAPTURECOUNT,
			     &(BGL_REGEXP(re).capturecount));
	 
	 BGL_REGEXP(re).match_data = 0;
	 BGL_REGEXP(re).match = bgl_regmatch;
	 BGL_REGEXP(re).match_n = bgl_regmatch_n;
	 BGL_REGEXP(re).free = bgl_regfree;
      
	 if (finalize) {
	    GC_register_finalizer(CREF(re),
				   (GC_finalization_proc)&bgl_pcre2_regcomp_finalize,
				   0, 0L, 0L);
	 }
	 
	 return re;
      } else {
	 PCRE2_UCHAR *errbuf = alloca(256);
	 PCRE2_UCHAR *buf = alloca(256 + 80);

	 pcre2_get_error_message(errorcode, errbuf, 256);
	 sprintf((char *)buf, "PCRE2 compilation failed at offset %ld: %s\n",
		  erroffset, errbuf);

	 if (!(options & PCRE_BGLNORAISE)) {
	    C_SYSTEM_FAILURE(BGL_IO_PARSE_ERROR, "pregexp", (char *)buf, pat);

	    return re;
	 } else {
	    return string_to_bstring((char *)buf);
	 }
      }
   }
}

