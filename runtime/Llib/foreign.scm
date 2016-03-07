;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/foreign.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  5 16:50:26 1995                          */
;*    Last change :  Thu Mar  3 13:31:00 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The foreign object management.                                   */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/foreign.texi@                             */
;*       @node Foreign Interface@                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __foreign
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __bignum
	    
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_strings_6_7
	    __r4_symbols_6_4
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    
	    __evenv)
   
   (extern  (macro c-foreign?::bool (::obj) "FOREIGNP")
	    (macro c-foreign-null?::bool (::obj) "FOREIGN_NULLP")
	    (macro c-foreign-eq?::bool (::foreign ::foreign) "FOREIGN_EQP")
	    (macro foreign-id::symbol (::foreign) "FOREIGN_ID")
	    (macro %string-ptr-null?::bool (::string) "STRING_PTR_NULL")
	    (macro %void*-ptr-null?::bool (::void*) "FOREIGN_PTR_NULL")
	    (infix macro $make-string-ptr-null::string () "0L")
	    (infix macro $make-void*-null::void* () "0L"))
   
   (java    (class foreign
	       (method static c-foreign?::bool (::obj)
		       "FOREIGNP")
 	       (method static c-foreign-null?::bool (::obj)
		       "FOREIGN_NULLP")
	       (method static c-foreign-eq?::bool (::obj ::obj)
		       "FOREIGN_EQP")
	       (method static foreign-id::symbol (::obj)
		       "FOREIGN_ID")
	       (method static %string-ptr-null?::bool (::string)
		       "STRING_PTR_NULL")
	       (method static %void*-ptr-null?::bool (::void*)
		       "OBJECT_PTR_NULL")
	       (method static $make-string-ptr-null::string ()
		       "MAKE_STRING_PTR_NULL")
	       (method static $make-void*-null::void* ()
		       "MAKE_VOID_STAR_NULL")
	       (method static foreign-print::void (::string)
		       "print")))
	          
   (export  (inline foreign?::bool ::obj)
	    (inline foreign-eq?::bool ::obj ::obj)
	    (inline foreign-null?::bool ::obj)
	    (inline string-ptr-null?::bool ::string)
	    (inline obj->cobj::cobj ::obj)
	    (inline void*-null?::bool ::void*)
	    (inline make-string-ptr-null::string)
	    (inline make-void*-null::void*))
   
   (pragma  (c-foreign? (predicate-of foreign) no-cfa-top nesting)
	    (foreign? (predicate-of foreign) no-cfa-top nesting)
	    (c-foreign-null? side-effect-free no-cfa-top nesting)
	    (foreign-null? side-effect-free no-cfa-top nesting)
	    (c-foreign-eq? side-effect-free no-cfa-top nesting)
	    (foreign-eq? side-effect-free no-cfa-top nesting)
	    (string-ptr-null? side-effect-free no-cfa-top nesting)
	    (void*-null? side-effect-free no-cfa-top nesting)
	    (obj->cobj side-effect-free)))
 
;*---------------------------------------------------------------------*/
;*    foreign? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (foreign? obj)
   (c-foreign? obj))

;*---------------------------------------------------------------------*/
;*    foreign-eq? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (foreign-eq? o1 o2)
   (c-foreign-eq? o1 o2))

;*---------------------------------------------------------------------*/
;*    foreign-null? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (foreign-null? obj)
   (if (foreign? obj)
       (c-foreign-null? obj)
       (error "foreign-null?" "not a foreign object" obj)))

;*---------------------------------------------------------------------*/
;*    @deffn string-ptr-null?@ ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (string-ptr-null? obj::string)
   (%string-ptr-null? obj))
 
;*---------------------------------------------------------------------*/
;*    obj->cobj ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (obj->cobj obj)
   ($obj->cobj obj))

;*---------------------------------------------------------------------*/
;*    void*-null? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (void*-null? obj::void*)
   (%void*-ptr-null? obj))

;*---------------------------------------------------------------------*/
;*    make-string-ptr-null ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (make-string-ptr-null)
   ($make-string-ptr-null))

;*---------------------------------------------------------------------*/
;*    make-void*-null ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (make-void*-null)
   ($make-void*-null))
