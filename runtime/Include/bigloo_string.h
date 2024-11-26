/*=====================================================================*/
/*    .../prgm/project/bigloo/flt/runtime/Include/bigloo_string.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Tue Nov 26 11:17:31 2024 (serrano)                */
/*    Copyright   :  2016-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo STRINGs                                                   */
/*=====================================================================*/
#ifndef BIGLOO_STRING_H 
#define BIGLOO_STRING_H

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
/*    extern                                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t make_string_sans_fill(long);
BGL_RUNTIME_DECL obj_t string_to_bstring(char *);
BGL_RUNTIME_DECL obj_t string_to_bstring_len(char *, int);
BGL_RUNTIME_DECL obj_t close_init_string();
BGL_RUNTIME_DECL obj_t bgl_string_shrink(obj_t, long);
BGL_RUNTIME_DECL bool_t bigloo_strcmp(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_strcmp_at(obj_t, obj_t, long);
BGL_RUNTIME_DECL bool_t bigloo_strcmp_ci_at(obj_t, obj_t, long);
BGL_RUNTIME_DECL bool_t bigloo_strncmp_at(obj_t, obj_t, long, long);
BGL_RUNTIME_DECL bool_t bigloo_strncmp_ci_at(obj_t, obj_t, long, long);
BGL_RUNTIME_DECL bool_t bigloo_strncmp(obj_t, obj_t, long);
BGL_RUNTIME_DECL bool_t bigloo_strncmp_ci(obj_t, obj_t, long);
BGL_RUNTIME_DECL bool_t bigloo_stricmp(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_lt(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_le(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_gt(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_ge(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_cilt(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_cile(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_cigt(obj_t, obj_t);
BGL_RUNTIME_DECL bool_t bigloo_string_cige(obj_t, obj_t);

BGL_RUNTIME_DECL obj_t ucs2_to_utf8_string(ucs2_t *, long);
BGL_RUNTIME_DECL obj_t ucs2_string_to_utf8_string(obj_t);
BGL_RUNTIME_DECL obj_t make_ucs2_string(int, ucs2_t);

#if (BGL_HAVE_UNISTRING)
BGL_RUNTIME_DECL int bgl_strcoll(obj_t, obj_t);
#endif					

/*---------------------------------------------------------------------*/
/*    BGL_STRING_LENGTH_FIELDP ...                                     */
/*---------------------------------------------------------------------*/
#define BGL_STRING_LENGTH_FIELDP \
   (defined(TAG_STRING) || (BGL_HEADER_DATA_BIT_SIZE == 0))

/*---------------------------------------------------------------------*/
/*    bgl_string ...                                                   */
/*---------------------------------------------------------------------*/
struct bgl_string {
#if (!defined(TAG_STRING))
   header_t header;
#endif		
#if (defined(TAG_STRING) || BGL_HEADER_DATA_BIT_SIZE == 0)
   long length;
#endif
   unsigned char char0[1];
};

struct bgl_ucs2_string {
   header_t header;
   long length;
   ucs2_t char0;
};   

#define STRING(o) (CSTRING(o)->string)
#define UCS2_STRING(o) (CUCS2STRING(o)->ucs2_string)

#define STRING_SIZE (sizeof(struct bgl_string))
#define UCS2_STRING_SIZE (sizeof(struct bgl_ucs2_string))

/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
#if (defined(TAG_STRING))
#  define BSTRING(p) BGL_BPTR((obj_t)((long)p + TAG_STRING))
#  define CSTRING(p) BGL_CPTR((obj_t)((long)p - TAG_STRING))
#  if (TAG_STRING != TAG_QNAN)
#    define BGL_STRINGP(o) BGL_TAGGED_PTRP(o, TAG_STRING, TAG_MASK)
#  else
#    define BGL_STRINGP(o) \
       (((((long)c) & TAG_MASK) == TAG_STRING) && ((long) c) & NAN_MASK)
#  endif
#else
#  define BSTRING(p) BREF(p)
#  define CSTRING(p) BGL_CPTR(((obj_t)((unsigned long)(p) - TAG_POINTER)))
#  define BGL_STRINGP(c) (POINTERP(c) && (TYPE(c) == STRING_TYPE))
#endif

#define STRINGP(o) BGL_STRINGP(o)

#define BUCS2STRING(p) BREF(p)
#define CUCS2STRING(p) CREF(p)

#define UCS2_STRINGP(c) (POINTERP(c) && (TYPE(c) == UCS2_STRING_TYPE))

/*---------------------------------------------------------------------*/
/*    BGL_MAKE_STRING_HEADER ...                                       */
/*---------------------------------------------------------------------*/
#if BGL_STRING_LENGTH_FIELDP
#  define BGL_MAKE_STRING_HEADER(_v, _t, _l) \
     (_v->string.length = _l, BGL_MAKE_HEADER(_t, 0))
#else
#  define BGL_MAKE_STRING_HEADER(_v, _t, _l) \
     BGL_MAKE_HEADER(_t, _l)
#endif

/*---------------------------------------------------------------------*/
/*    alloc                                                            */
/*---------------------------------------------------------------------*/
/* When producing C code for a compiler that is unable to    */
/* accept large splitted string, Bigloo emits a declaration  */
/* of a C characters array. This requires 2 macros, one for  */
/* starting the declaration and one for ending it. The       */
/* array itself, is inserted in between the two macros by    */
/* bigloo such as:                                           */
/*        DEFINE_STRING_START(f, a, 2),                      */
/*          {45,46,0},                                       */
/*        DEFINE_STRING_STOP(f, a, 2);                       */
#if (defined(TAG_STRING))
#  define DEFINE_STRING(name, aux, str, len) \
   static struct { __CNST_ALIGN long length; \
                      char string[len + 1]; } \
         aux = { __CNST_FILLER len, str }; \
         static obj_t name = BSTRING(&(aux.length))
#  define DEFINE_STRING_START(name, aux, len) \
      static struct { __CNST_ALIGN long length; \
                      char string[len + 1]; } \
         aux = { __CNST_FILLER len
#  define DEFINE_STRING_STOP(name, aux) \
        }; static obj_t name = BSTRING(&(aux.length) 
#elif (BGL_HEADER_DATA_BIT_SIZE == 0)
#  define DEFINE_STRING(name, aux, str, len) \
      static struct { __CNST_ALIGN header_t header; \
                      long length; \
                      char string[len + 1]; } \
         aux = { __CNST_FILLER BGL_MAKE_HEADER(STRING_TYPE, 0), len, str }; \
         static obj_t name = BSTRING(&(aux.header))
#  define DEFINE_STRING_START(name, aux, len) \
      static struct { __CNST_ALIGN header_t header; \
                      long length; \
                      char string[len + 1]; } \
         aux = { __CNST_FILLER BGL_MAKE_HEADER(STRING_TYPE, 0), len
#  define DEFINE_STRING_STOP(name, aux) \
        }; static obj_t name = BSTRING(&(aux.header))
#else
#  define DEFINE_STRING(name, aux, str, len) \
      static struct { __CNST_ALIGN header_t header; \
                      char string[len + 1]; } \
         aux = { __CNST_FILLER BGL_MAKE_HEADER(STRING_TYPE, len), str }; \
         static obj_t name = BSTRING(&(aux.header))
#  define DEFINE_STRING_START(name, aux, len) \
      static struct { __CNST_ALIGN header_t header; \
                      char string[len + 1]; } \
         aux = { __CNST_FILLER BGL_MAKE_HEADER(STRING_TYPE, 0)
#  define DEFINE_STRING_STOP(name, aux) \
        }; static obj_t name = BSTRING(&(aux.header))
#endif

/*---------------------------------------------------------------------*/
/*    api                                                              */
/*---------------------------------------------------------------------*/
#if (defined(TAG_STRING) || BGL_HEADER_DATA_BIT_SIZE == 0)
#  define BGL_STRING_LENGTH(_s) STRING(_s).length
#  define BGL_STRING_LENGTH_SET(_s, _l) (STRING(_s).length = _l)
#else
#  define BGL_STRING_LENGTH(_s) BGL_HEADER_FULLSIZE(STRING(_s).header)
#  define BGL_STRING_LENGTH_SET(_s, _l) (((obj_t)COBJECT(_s))->header = BGL_MAKE_HEADER(STRING_TYPE, _l))
#endif

#define STRING_LENGTH(_s) BGL_STRING_LENGTH(_s)

#define BSTRING_TO_USTRING(s) (&(STRING(s).char0[0]))
#define BSTRING_TO_STRING(s) ((char *)(BSTRING_TO_USTRING(s)))

#define STRING_REF(v, i) (((unsigned char *)BSTRING_TO_STRING(v))[i])
#define STRING_SET(s, i, c) (STRING_REF(s, i) = c, BUNSPEC)
#define STRING_PTR_NULL(_p) (_p == 0)

#define BGL_MEMCHR(s, c, n, i) memchr(&s[i], c, n)
#define BGL_MEMCHR_ZERO(s) ((s) == 0L)
#define BGL_MEMCHR_DIFF(s1, s2) ((s1) - (s2))
	 
#define UCS2_STRING_LENGTH(s) UCS2_STRING(s).length

#define BUCS2_STRING_TO_UCS2_STRING(s) (&(UCS2_STRING(s).char0))

#define UCS2_STRING_REF(v, i) (BUCS2_STRING_TO_UCS2_STRING(v)[i])
#define UCS2_STRING_SET(s, i, c) (UCS2_STRING_REF(s, i) = c, BUNSPEC)
	    
/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

