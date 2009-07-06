;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/struct.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 30 13:02:29 1992                          */
;*    Last change :  Tue Mar 11 15:52:21 2008 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Non R4Rs structure and SRFI-9 records.                           */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/struct.texi@                              */
;*       @node Structures@                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __structure
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_vectors_6_8
	    __r4_characters_6_6
	    __r4_pairs_and_lists_6_3
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    
	    __evenv)
   
   (extern  (macro c-struct-ref::obj (::struct ::int)
		   "STRUCT_REF")
	    (macro c-struct-set!::obj (::struct ::int ::obj)
		   "STRUCT_SET")
	    (macro u-struct-ref::obj (::obj ::int)
		   "STRUCT_REF")
	    (macro u-struct-set!::obj (::obj ::int ::obj)
		   "STRUCT_SET")
	    ;; the struct-key function takes ::obj (instead of ::symbol)
	    ;; parameters because the key of the structure is used by
	    ;; intext. intext stores non symbol into that field. to avoid
	    ;; untrapped type error it is safe to untype the C functions.
	    (macro c-struct-key::obj (::struct)
		   "STRUCT_KEY")
	    (macro c-struct-key-set!::obj (::struct ::obj)
		   "STRUCT_KEY_SET")
	    (macro c-struct?::bool (::obj)
		   "STRUCTP")
	    (macro c-struct-length::int (::struct)
		   "STRUCT_LENGTH")
	    (c-make-struct::struct (::symbol ::int ::obj)
				   "make_struct")
	    (macro c-make-s-struct::struct (::symbol ::int ::obj)
		   "MAKE_S_STRUCT")
	    (c-create-struct::struct (::symbol ::int)
				     "create_struct")
	    (macro c-create-s-struct::struct (::symbol ::int)
		   "CREATE_S_STRUCT"))
   
   (java    (class foreign
	       (method static c-struct-ref::obj (::struct ::int)
		       "STRUCT_REF")
	       (method static c-struct-set!::obj (::struct ::int ::obj)
		       "STRUCT_SET")
	       (method static u-struct-ref::obj (::struct ::int)
		       "UNSAFE_STRUCT_REF")
	       (method static u-struct-set!::obj (::struct ::int ::obj)
		       "UNSAFE_STRUCT_SET")
	       (method static c-struct-key::obj (::struct)
		       "STRUCT_KEY")
	       (method static c-struct-key-set!::obj (::struct ::obj)
		       "STRUCT_KEY_SET")
	       (method static c-struct?::bool (::obj)
		       "STRUCTP")
	       (method static c-struct-length::int (::struct)
		       "STRUCT_LENGTH")
	       (method static c-make-struct::struct (::symbol ::int ::obj)
		       "make_struct")
	       (method static c-make-s-struct::struct (::symbol ::int ::obj)
		       "MAKE_S_STRUCT")
	       (method static c-create-struct::struct (::symbol ::int)
		       "create_struct")
	       (method static c-create-s-struct::struct (::symbol ::int)
		       "CREATE_S_STRUCT")))
   
   (export  (inline make-struct::struct ::symbol ::int ::obj)
	    (inline struct?::bool ::obj)
	    (inline record?::bool ::obj)
	    (inline struct-key::symbol ::struct)
	    (inline struct-key-set! ::struct ::symbol)
	    (inline struct-length::int ::struct)
	    (inline struct-ref ::struct ::int)
	    (inline struct-set! ::struct ::int ::obj)
	    (struct-update! ::struct ::struct)
	    (struct->list::pair ::struct)
	    (list->struct::struct ::pair))
   
   (pragma  (c-struct? (predicate-of struct) no-cfa-top nesting)
	    (struct? side-effect-free no-cfa-top nesting)
	    (record? side-effect-free no-cfa-top nesting)
	    (c-make-struct no-cfa-top)
	    (make-struct no-cfa-top)
	    (c-struct-length side-effect-free no-cfa-top nesting args-safe)
	    (struct-length side-effect-free no-cfa-top nesting)
	    (c-struct-ref side-effect-free no-cfa-top nesting args-safe)
	    (struct-ref side-effect-free no-cfa-top nesting)
	    (u-struct-ref side-effect-free no-cfa-top nesting)
	    (c-struct-key side-effect-free no-cfa-top nesting args-safe)
	    (struct-key side-effect-free no-cfa-top nesting)))
	    
;*---------------------------------------------------------------------*/
;*    make-struct ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (make-struct key len init)
   (c-make-struct key len init))

;*---------------------------------------------------------------------*/
;*    struct? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (struct? o)
   (c-struct? o))

;*---------------------------------------------------------------------*/
;*    record? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (record? o)
   (c-struct? o))

;*---------------------------------------------------------------------*/
;*    struct-key ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (struct-key s)
   (c-struct-key s))

;*---------------------------------------------------------------------*/
;*    struct-key-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (struct-key-set! s k)
   (c-struct-key-set! s k))

;*---------------------------------------------------------------------*/
;*    struct-length ...                                                */
;*---------------------------------------------------------------------*/
(define-inline  (struct-length s)
   (c-struct-length s))
   
;*---------------------------------------------------------------------*/
;*    struct-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline  (struct-ref s k)
   (c-struct-ref s k))

;*---------------------------------------------------------------------*/
;*    struct-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline  (struct-set! s k o)
   (c-struct-set! s k o))

;*---------------------------------------------------------------------*/
;*    struct-update! ...                                               */
;*---------------------------------------------------------------------*/
(define (struct-update! dst src)
   (if (and (eq? (struct-key dst) (struct-key src))
	    (=fx (struct-length dst) (struct-length src)))
       (let loop ((i (-fx (struct-length dst) 1)))
	  (if (=fx i -1)
	      dst
	      (begin
		 (struct-set! dst i (struct-ref src i))
		 (loop (-fx i 1)))))
       (error "struct-update!" "Incompatible structures" (list dst src))))

;*---------------------------------------------------------------------*/
;*    struct->list ...                                                 */
;*---------------------------------------------------------------------*/
(define (struct->list struct)
   (let loop ((i (-fx (struct-length struct) 1))
	      (r '()))
      (if (=fx i -1)
	  (cons (struct-key struct) r)
	  (loop (-fx i 1) (cons (struct-ref struct i) r)))))

;*---------------------------------------------------------------------*/
;*    list->struct ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->struct lst)
   (cond
      ((null? lst)
       (error 'list->struct "Illegal empty list" lst))
      ((not (symbol? (car lst)))
       (error 'list->struct "Illegal struct key" (car lst)))
      (else
       (let* ((len (length (cdr lst)))
	      (struct (make-struct (car lst) len #unspecified)))
	  (let loop ((i 0)
		     (l (cdr lst)))
	     (if (null? l)
		 struct
		 (begin
		    (struct-set! struct i (car l))
		    (loop (+fx i 1) (cdr l)))))))))
