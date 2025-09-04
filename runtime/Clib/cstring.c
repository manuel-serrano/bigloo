/*=====================================================================*/
/*    serrano/prgm/project/bigloo/wasm/runtime/Clib/cstring.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Sep  5 09:55:58 1995                          */
/*    Last change :  Thu Sep  4 09:57:25 2025 (serrano)                */
/*    -------------------------------------------------------------    */
/*    String management                                                */
/*=====================================================================*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    string_to_bstring_len ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
string_to_bstring_len(char *c_string, int len) {
   /* STRING_SIZE already includes 1 byte for one char */
   obj_t string = GC_MALLOC_ATOMIC(STRING_SIZE + len);
   char *dst;
   
   if (!c_string) c_string = "";

#if (!defined(TAG_STRING))
   string->string.header = BGL_MAKE_STRING_HEADER(string, STRING_TYPE, len);
#else
   string->string.length = len;
#endif   

   dst = BSTRING_TO_STRING(BSTRING(string));

   memcpy(dst, c_string, len);
   dst[len] = '\0';
   
   return BSTRING(string);
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_bstring_to_gc_cstring ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
char *
bgl_bstring_to_gc_cstring(obj_t str) {
   char *ostr = BSTRING_TO_STRING(str);
   char *nstr =  (char *)GC_MALLOC_ATOMIC(BGL_STRING_LENGTH(ostr) + 1);

   memcpy(nstr, ostr, BGL_STRING_LENGTH(ostr));

   return nstr;
}
   
/*---------------------------------------------------------------------*/
/*    string_to_bstring ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
string_to_bstring(char *c_string) {
   return string_to_bstring_len(c_string, c_string ? (int)strlen(c_string) : 0);
}

/*---------------------------------------------------------------------*/
/*    make_string ...                                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_string(long len, unsigned char c) {
   obj_t string;

   if (len < 0) {
      C_FAILURE("make-string", "Illegal string size", BINT(len));
   } else {
      string = GC_MALLOC_ATOMIC(STRING_SIZE + len);

#if (!defined(TAG_STRING))
      string->string.header = BGL_MAKE_STRING_HEADER(string, STRING_TYPE, len);
#else
      string->string.length = len;
#endif

      memset(&(string->string.char0), c, len);
      STRING_SET(BSTRING(string), len, '\0');
		
      return BSTRING(string);
   }
}

/*---------------------------------------------------------------------*/
/*    make_string_sans_fill ...                                        */
/*    -------------------------------------------------------------    */
/*    This is a private function that does not need to check the       */
/*    correctness of the len argument.                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_string_sans_fill(long len) {
   obj_t string = GC_MALLOC_ATOMIC(STRING_SIZE + len);

#if (!defined(TAG_STRING))
   string->string.header = BGL_MAKE_STRING_HEADER(string, STRING_TYPE, len);
#else
   string->string.length = len;
#endif
   
   STRING_SET(BSTRING(string), len, '\0');
   return BSTRING(string);
}

/*---------------------------------------------------------------------*/
/*    c_constant_string_to_string ...                                  */
/*---------------------------------------------------------------------*/
obj_t
c_constant_string_to_string(char *c_string) {
   return string_to_bstring(c_string);
}

/*---------------------------------------------------------------------*/
/*    string_append ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
string_append(obj_t s1, obj_t s2) {
   int l1 = BGL_STRING_LENGTH(s1);
   int l2 = BGL_STRING_LENGTH(s2);
   int l12 = l1 + l2;
   obj_t string = GC_MALLOC_ATOMIC(STRING_SIZE + l12);

#if (!defined(TAG_STRING))
   string->string.header = BGL_MAKE_STRING_HEADER(string, STRING_TYPE, l12);
#else
   string->string.length = l12;
#endif	

   memcpy(&(string->string.char0), &STRING_REF(s1, 0), l1);
   memcpy(&((char *)(&(string->string.char0)))[l1], &STRING_REF(s2, 0), l2 + 1);
	
   return BSTRING(string);
}
 
/*---------------------------------------------------------------------*/
/*    string_append_3 ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
string_append_3(obj_t s1, obj_t s2, obj_t s3) {
   int l1 = BGL_STRING_LENGTH(s1);
   int l2 = BGL_STRING_LENGTH(s2);
   int l3 = BGL_STRING_LENGTH(s3);
   int  l123 = l1 + l2 + l3;
   obj_t string = GC_MALLOC_ATOMIC(STRING_SIZE + l123);

#if (!defined(TAG_STRING))
   string->string.header = BGL_MAKE_STRING_HEADER(string, STRING_TYPE, l123);
#else
   string->string.length = l123;
#endif	

   memcpy(&(string->string.char0), &STRING_REF(s1, 0), l1);
   memcpy(&((char *)(&(string->string.char0)))[l1], &STRING_REF(s2, 0), l2);
   memcpy(&((char *)(&(string->string.char0)))[l1 + l2], &STRING_REF(s3, 0), l3 + 1);
	
   return BSTRING(string);
}
 
/*---------------------------------------------------------------------*/
/*    c_substring ...                                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
c_substring(obj_t src_string, long min, long max) {
   long len = max - min;
   obj_t dst_string = GC_MALLOC_ATOMIC(STRING_SIZE + len);

#if (!defined(TAG_STRING))
   dst_string->string.header = BGL_MAKE_STRING_HEADER(dst_string, STRING_TYPE, len);
#else
   dst_string->string.length = len;
#endif
   
   memcpy(&(dst_string->string.char0), &STRING_REF(src_string, min), len);
   ((char *)(&(dst_string->string.char0)))[len] = '\0';

   return BSTRING(dst_string);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    blit_string ...                                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
blit_string(obj_t s1, long offset1, obj_t s2, long offset2, long len) {
   /* intrinsic memcpy doest not work when memory areas overlap */
   unsigned char *src = &STRING_REF(s1, offset1);
   unsigned char *dest = &STRING_REF(s2, offset2);

   if (((src + len) <= dest) || ((dest + len) <= src))
      memcpy(dest, src, len);
   else
      memmove(dest, src, len);

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static char                                                      */
/*    letters ...                                                      */
/*---------------------------------------------------------------------*/
static char letters[] = "0123456789abcdefghijklmnopqrstuvwxyz";

/*---------------------------------------------------------------------*/
/*    integer_to_string ...                                            */
/*---------------------------------------------------------------------*/
obj_t
integer_to_string(long x, long radix) {
   obj_t aux;
   char *s;
   long ax;
   int bits = (x <= 0 ? 1 : 0);

   for (ax = x; ax != 0; ax /= radix)
      bits++;
   aux = make_string_sans_fill(bits);

   s = BSTRING_TO_STRING(aux) + bits;
   *s = '\0';

   for (ax = x, s--; bits > 0; bits--, s--, ax /= radix)
      *s = letters[abs((int)(ax % radix))];

   if (x < 0) *(s+1) = '-';
   
   return aux;
}

/*---------------------------------------------------------------------*/
/*    unsigned_to_string ...                                           */
/*---------------------------------------------------------------------*/
obj_t
unsigned_to_string(long x, long radix) {
   obj_t aux;
   char *s;
   unsigned long ax;
   int bits = (x == 0 ? 1 : 0);

   for (ax = x; ax > 0; ax /= radix)
      bits++;
   aux = make_string_sans_fill(bits);

   s = BSTRING_TO_STRING(aux) + bits;
   *s = '\0';

   for (ax = x, s--; bits > 0; bits--, s--, ax /= radix)
      *s = letters[ax % radix];

   return aux;
}

/*---------------------------------------------------------------------*/
/*    integer_to_string_padding ...                                    */
/*---------------------------------------------------------------------*/
obj_t
integer_to_string_padding(long x, long padding, long radix) {
   obj_t aux;
   int bits = (x <= 0 ? 1 : 0);
   long ax = BGL_LABS(x);
   unsigned long axx = (unsigned long)ax;
   char fmt[30];

   switch (radix) {
      case 2 :
      {
	 char *s;

	 while (axx > 0) {
	    bits++, axx /= 2;
	 }
	 
	 aux = make_string(bits > padding ? bits : padding, '0');

	 s = BSTRING_TO_STRING(aux) + (bits > padding ? bits : padding);
	 *s = '\0';

	 for (s--; bits > 0; bits--, s--, ax = ax >> 1)
	    *s =  ((ax & 1) == 0) ? '0' : '1';

	 if (x < 0) *BSTRING_TO_STRING(aux) = '-';

	 return aux;
      }
      break;

      case 8 :
	 if (x < 0) {
	    sprintf(fmt, "-%%0%ldlo", padding - 1);
	 } else {
	    sprintf(fmt, "%%0%ldlo", padding);
	 }
	 break;
      case 16 :
	 if (x < 0) {
	    sprintf(fmt, "-%%0%ldlx", padding - 1);
	 } else {
	    sprintf(fmt, "%%0%ldlx", padding);
	 }
	 break;
      default :
	 if (x < 0) {
	    sprintf(fmt, "-%%0%ldld", padding - 1);
	 } else {
	    sprintf(fmt, "%%0%ldld", padding);
	 }
	 break;
   }

   while (axx > 0) bits++, axx /= radix;

   aux = make_string_sans_fill(bits > padding ? bits : padding);
   sprintf(BSTRING_TO_STRING(aux), fmt, ax);

   return aux;
}

/*---------------------------------------------------------------------*/
/*    llong_to_string ...                                              */
/*---------------------------------------------------------------------*/
obj_t
llong_to_string(BGL_LONGLONG_T x, long radix) {
   obj_t aux;
   char *s;
   BGL_LONGLONG_T ax;
   int bits = (x <= 0 ? 1 : 0);
   char letters[] = "0123456789abcdef";

   for (ax = x; ax != 0; ax /= radix)
      bits++;
   aux = make_string_sans_fill(bits);

   s = BSTRING_TO_STRING(aux) + bits;
   *s = '\0';

   for (ax = x, s--; bits > 0; bits--, s--, ax /= radix)
      *s = letters[abs((int)(ax % radix))];

   if (x < 0) *(s+1) = '-';

   return aux;
}

/*---------------------------------------------------------------------*/
/*    ullong_to_string ...                                             */
/*---------------------------------------------------------------------*/
obj_t
ullong_to_string(BGL_LONGLONG_T x, long radix) {
   obj_t aux;
   char *s;
   unsigned BGL_LONGLONG_T ax;
   int bits = (x == 0 ? 1 : 0);
   char letters[] = "0123456789abcdef";

   for (ax = x; ax > 0; ax /= radix)
      bits++;
   aux = make_string_sans_fill(bits);

   s = BSTRING_TO_STRING(aux) + bits;
   *s = '\0';

   for (ax = x, s--; bits > 0; bits--, s--, ax /= radix)
      *s = letters[ax % radix];

   return aux;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_double_to_ieee_string ...                                    */
/*    -------------------------------------------------------------    */
/*    A converted string is exactly sizeof(double)+1 actual characters */
/*    long. The +1 is used to encoded the endianess of the machine     */
/*    that has constructed the string.                                 */
/*    -------------------------------------------------------------    */
/*    This function encodes the number into a big endian               */
/*    representation. This is mandatory in order to conform the        */
/*    JVM specification                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t 
bgl_double_to_ieee_string(double o) {
   obj_t res = make_string_sans_fill(sizeof(double));
   char *aux = BSTRING_TO_STRING(res);
   char *tmp = (char *)(&o);

#if (BGL_BIG_ENDIAN == 1)
   int i;
   
   for (i = 0; i < sizeof(double); i++) {
      aux[i] = tmp[i];
   }
   aux[i] = 0;
#else
   int i, j;
   
   for (j = 0, i = (sizeof(double) - 1); i >= 0; i--, j++) {
      aux[j] = tmp[i];
   }
   aux[j] = 0;
#endif
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_ieee_string_to_double ...                                    */
/*    -------------------------------------------------------------    */
/*    The number is stored using a big endian encoding                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
double
bgl_ieee_string_to_double(obj_t s) {
   char *aux = BSTRING_TO_STRING(s);
   double res;
   char *tmp = (char *)(&res);

#if (BGL_BIG_ENDIAN == 1)
   int i;
   
   for (i = 0; i < sizeof(double); i++) {
      tmp[i] = aux[i];
   }
#else
   int i, j;
   
   for (j = 0, i = sizeof(double) - 1;  i >= 0; i--, j++) {
      tmp[i] = aux[j];
   }
#endif

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_float_to_ieee_string ...                                     */
/*    -------------------------------------------------------------    */
/*    A converted string is exactly sizeof(float)+1 actual characters  */
/*    long. The +1 is used to encoded the endianess of the machine     */
/*    that has constructed the string.                                 */
/*    -------------------------------------------------------------    */
/*    This function encodes the number into a big endian               */
/*    representation. This is mandatory in order to conform the        */
/*    JVM specification                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t 
bgl_float_to_ieee_string(float o) {
   obj_t res = make_string_sans_fill(sizeof(float));
   char *aux = BSTRING_TO_STRING(res);
   char *tmp = (char *)(&o);

#if (BGL_BIG_ENDIAN == 1)
   int i;
   
   for (i = 0; i < sizeof(float); i++) {
      aux[i] = tmp[i];
   }
   aux[i] = 0;
#else
   int i, j;
   for (j = 0, i = (sizeof(float) - 1); i >= 0; i--, j++) {
      aux[j] = tmp[i];
   }
   aux[j] = 0;
#endif
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    float                                                            */
/*    bgl_ieee_string_to_float ...                                     */
/*    -------------------------------------------------------------    */
/*    The number is stored using a big endian encoding                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
float
bgl_ieee_string_to_float(obj_t s) {
   char *aux = BSTRING_TO_STRING(s);
   float res;
   char *tmp = (char *)(&res);

#if (BGL_BIG_ENDIAN == 1)
   int i;
   
   for (i = 0; i < sizeof(float); i++) {
      tmp[i] = aux[i];
   }
#else
   int i, j;
   
   for (j = 0, i = sizeof(float) - 1; i >= 0; i--, j++) {
      tmp[i] = aux[j];
   }
#endif

   return res;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strcmp ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strcmp(obj_t o1, obj_t o2) {
   int l1 = BGL_STRING_LENGTH(o1);

   if (l1 == BGL_STRING_LENGTH(o2)) {
      return !memcmp((void *)BSTRING_TO_STRING(o1),
		      (void *)BSTRING_TO_STRING(o2), l1);
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strcmp_at ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strcmp_at(obj_t o1, obj_t o2, long d) {
   long l1 = BGL_STRING_LENGTH(o1);
   long l2 = BGL_STRING_LENGTH(o2);
   
   if ((d >= 0) && ((l2 + d) <= l1)) 
      return !memcmp((void *)(BSTRING_TO_STRING(o1) + d),
		      (void *)BSTRING_TO_STRING(o2),
		      l2);
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strcmp_ci_at ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strcmp_ci_at(obj_t o1, obj_t o2, long d) {
   long l1 = BGL_STRING_LENGTH(o1);
   long l2 = BGL_STRING_LENGTH(o2);

   if ((d >= 0) && ((l2 + d) <= l1)) {
      char *st1 = ((char *)BSTRING_TO_STRING(o1)) + d;
      char *st2 = (char *)BSTRING_TO_STRING(o2);
      long i = 0;
      
      for (;
	   (i < l2) && (tolower(*st1) == tolower(*st2));
	   i++, st1++, st2++);

      return (i == l2);
   }
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strncmp_at ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strncmp_at(obj_t o1, obj_t o2, long d, long l3) {
   long l1 = BGL_STRING_LENGTH(o1);
   long l2 = BGL_STRING_LENGTH(o2);
   long l = l2 < l3 ? l2 : l3;

   if ((d >= 0) && (l3 >= 0) && ((l + d) <= l1)) 
      return !memcmp((void *)(BSTRING_TO_STRING(o1) + d),
		      (void *)BSTRING_TO_STRING(o2),
		      l);
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strncmp_ci_at ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strncmp_ci_at(obj_t o1, obj_t o2, long d, long l3) {
   long l1 = BGL_STRING_LENGTH(o1);
   long l2 = BGL_STRING_LENGTH(o2);
   long l = l2 < l3 ? l2 : l3;

   if ((d >= 0) && (l3 >= 0) && (l + d) <= l1) {
      char *st1 = ((char *)BSTRING_TO_STRING(o1)) + d;
      char *st2 = (char *)BSTRING_TO_STRING(o2);
      long i = 0;
      
      for (;
	   (i < l2) && (tolower(*st1) == tolower(*st2));
	   i++, st1++, st2++);

      return (i == l);
   }
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strncmp ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strncmp(obj_t o1, obj_t o2, long l) {
   long l1 = BGL_STRING_LENGTH(o1);
   long l2 = BGL_STRING_LENGTH(o2);

   if ((l1 >= l) && (l2 >= l))
      return !memcmp((void *)BSTRING_TO_STRING(o1),
		      (void *)BSTRING_TO_STRING(o2), l);
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bigloo_strncmp_ci ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_strncmp_ci(obj_t o1, obj_t o2, long l) {
   long l1 = BGL_STRING_LENGTH(o1);
   long l2 = BGL_STRING_LENGTH(o2);

   if ((l1 >= l) && (l2 >= l)) {
      char *st1 = (char *)BSTRING_TO_STRING(o1);
      char *st2 = (char *)BSTRING_TO_STRING(o2);
      long i = 0;
      
      for (;
	   (i < l) && (tolower(*st1) == tolower(*st2));
	   i++, st1++, st2++);

      return (i == l);
   }
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    strcicmp ...                                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
bigloo_strcicmp(obj_t bst1, obj_t bst2) {
   long l1 = BGL_STRING_LENGTH(bst1);

   if (l1 == BGL_STRING_LENGTH(bst2)) {
      char *st1 = (char *)BSTRING_TO_STRING(bst1);
      char *st2 = (char *)BSTRING_TO_STRING(bst2);
      
      for (; l1 > 0; l1--, st1++, st2++)
        if (tolower(*st1) != tolower(*st2)) return 0;
      return 1;
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_lt ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
bigloo_string_lt(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1, l2;
   long i, min;

   l1 = BGL_STRING_LENGTH(bst1);
   l2 = BGL_STRING_LENGTH(bst2);

   min = (l1 < l2) ? l1 : l2;

   for (i = 0; (*st1 == *st2) && (i < min); i++, st1++, st2++);

   if (i < min) {
      return *st1 < *st2;
   } else {
      return l1 < l2;
   }
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_le ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
bigloo_string_le(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_le = (l1 <= l2);
   long min = l1_le ? l1 : l2;

   for (; min > 0; min--, st1++, st2++)
      if (*st1 != *st2)
         return (*st1 <= *st2); 

   return l1_le;
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_gt ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
bigloo_string_gt(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_gt = (l1 > l2);
   long min = l1_gt ? l2 : l1;

   for (; min > 0; min--, st1++, st2++)
      if (*st1 != *st2)
         return (*st1 > *st2); 

   return l1_gt;
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_ge ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
bigloo_string_ge(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_ge = (l1 >= l2);
   long min = l1_ge ? l2 : l1;

   for (; min > 0; min--, st1++, st2++)
      if (*st1 != *st2)
         return (*st1 >= *st2); 

   return l1_ge;
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_cilt ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
bigloo_string_cilt(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_lt = (l1 < l2);
   long min = l1_lt ? l1 : l2;
   
   for (; min > 0; min--, st1++, st2++) {
      unsigned char c1 = tolower(*st1);
      unsigned char c2 = tolower(*st2);
      if (c1 != c2)
         return (c1 < c2); 
   }

   return l1_lt;
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_cile ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
bigloo_string_cile(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_le = (l1 <= l2);
   long min = l1_le ? l1 : l2;
   
   for (; min > 0; min--, st1++, st2++) {
      unsigned char c1 = tolower(*st1);
      unsigned char c2 = tolower(*st2);
      if (c1 != c2)
         return (c1 <= c2); 
   }

   return l1_le;
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_cigt ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
bigloo_string_cigt(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_gt = (l1 > l2);
   long min = l1_gt ? l2 : l1;
   
   for (; min > 0; min--, st1++, st2++) {
      unsigned char c1 = tolower(*st1);
      unsigned char c2 = tolower(*st2);
      if (c1 != c2)
         return (c1 > c2); 
   }

   return l1_gt;
}

/*---------------------------------------------------------------------*/
/*    bigloo_string_cige ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
bigloo_string_cige(obj_t bst1, obj_t bst2) {
   unsigned char *st1 = (unsigned char *)BSTRING_TO_STRING(bst1);
   unsigned char *st2 = (unsigned char *)BSTRING_TO_STRING(bst2);
   long l1 = BGL_STRING_LENGTH(bst1);
   long l2 = BGL_STRING_LENGTH(bst2);

   bool_t l1_ge = (l1 >= l2);
   long min = l1_ge ? l2 : l1;
   
   for (; min > 0; min--, st1++, st2++) {
      unsigned char c1 = tolower(*st1);
      unsigned char c2 = tolower(*st2);
      if (c1 != c2)
         return (c1 >= c2); 
   }

   return l1_ge;
}

/*---------------------------------------------------------------------*/
/*    xdigit_to_byte ...                                               */
/*---------------------------------------------------------------------*/
#define XDIGIT_TO_BYTE(s_) \
  (isdigit(s_) ? (s_ - '0') : 10 + ((s_ >= 'a') ?  (s_ - 'a') : (s_ - 'A')))

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_string_shrink ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_string_shrink(obj_t s, long nlen) {
   if (nlen < BGL_STRING_LENGTH(s)) {
      BGL_STRING_LENGTH_SET(s, nlen);
      BSTRING_TO_STRING(s)[nlen] = '\0';
   }

   return s;
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    strtoll ...                                                      */
/*---------------------------------------------------------------------*/
#if !BGL_HAVE_STRTOLL
BGL_RUNTIME_DEF
BGL_LONGLONG_T
bgl_strtoll(const char *ptr, char **endptr, int base) {
   if (base != 10)
      C_FAILURE("cstring", "cannot convert from base other than 10", BINT(base));
   return (BGL_LONGLONG_T)strtod(ptr, endptr);
}
#endif

#if !BGL_HAVE_STRTOULL
BGL_RUNTIME_DEF
BGL_LONGLONG_T
bgl_strtoull(const char *ptr, char **endptr, int base) {
  if (base != 10)
    C_FAILURE("cstring", "cannot convert from base other than 10", BINT(base));
  return (BGL_LONGLONG_T)strtod(ptr, endptr);
}
#endif

