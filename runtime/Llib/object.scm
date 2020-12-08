;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/object.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 25 14:20:42 1996                          */
;*    Last change :  Fri Mar 27 12:01:07 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `object' library                                             */
;*    -------------------------------------------------------------    */
;*    This module _cannot_ contain method definitions otherwise        */
;*    it cannot be initialized.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __object

   (use     __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __foreign
	    __param
	    __bexit
	    __bignum
	    __thread
	    __bit
	    __hash
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
 	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __pp_circle
	    __evenv)

   (extern  (macro $object-widening::obj (::object)
		   "BGL_OBJECT_WIDENING")
	    (macro $object-widening-set!::obj (::object ::obj)
		   "BGL_OBJECT_WIDENING_SET")
	    
	    (macro object-header-size::long (::obj)
		   "BGL_OBJECT_HEADER_SIZE")
	    (macro object-header-size-set!::long (::obj ::long)
		   "BGL_OBJECT_HEADER_SIZE_SET")

	    (macro %object-type-number::long
		   "OBJECT_TYPE")
	    (macro %object?::bool (::obj)
		   "BGL_OBJECTP")
	    (macro $nanobject?::bool (::obj)
		   "BGL_NANOBJECTP")
	    (macro %object-class-num::long (::object)
		   "BGL_OBJECT_CLASS_NUM")
	    (macro %object-class-num-set!::obj (::object ::long)
		   "BGL_OBJECT_CLASS_NUM_SET")
	    ($bigloo-generic-mutex::mutex "bigloo_generic_mutex")
	    (%object-hashnumber::long (::obj) "bgl_obj_hash_number")
	    ($make-generic::procedure (::procedure) "bgl_make_generic")

	    (macro $make-class::class (::symbol ::symbol ::long
					 ::obj ::pair-nil
					 ::procedure ::long
					 ::vector ::vector
					 ::obj ::vector
					 ::obj ::procedure
					 ::obj ::long
					 ::obj)
		   "bgl_make_class")
	    (macro $classp::bool (::obj)
		   "BGL_CLASSP")
	    (macro $class-name::symbol (::class)
		   "BGL_CLASS_NAME")
	    (macro $class-index::long (::class)
		   "BGL_CLASS_INDEX")
	    (macro $class-depth::long (::class)
		   "BGL_CLASS_DEPTH")
	    (macro $class-super::obj (::class)
		   "BGL_CLASS_SUPER")
	    (macro $class-ancestors-ref::class (::class ::long)
		   "BGL_CLASS_ANCESTORS_REF")
	    (macro $class-subclasses::pair-nil (::class)
		   "BGL_CLASS_SUBCLASSES")
	    (macro $class-subclasses-set!::obj (::class ::pair-nil)
		   "BGL_CLASS_SUBCLASSES_SET")
	    (macro $class-nil::obj (::class)
		   "BGL_CLASS_NIL")
	    (macro $class-nil-set!::obj (::class ::obj)
		   "BGL_CLASS_NIL_SET")
	    (macro $class-module::symbol (::class)
		   "BGL_CLASS_MODULE")
	    (macro $class-alloc-fun::procedure (::class)
		   "BGL_CLASS_ALLOC_FUN")
	    (macro $class-hash::long (::class)
		   "BGL_CLASS_HASH")
	    (macro $class-new-fun::procedure (::class)
		   "BGL_CLASS_NEW_FUN")
	    (macro $class-nil-fun::procedure (::class)
		   "BGL_CLASS_NIL_FUN")
	    (macro $class-constructor::obj (::class)
		   "BGL_CLASS_CONSTRUCTOR")
	    (macro $class-shrink::obj (::class)
		   "BGL_CLASS_SHRINK")
	    (macro $class-virtual-fields::vector (::class)
		   "BGL_CLASS_VIRTUAL_FIELDS")
	    (macro $class-direct-fields::vector (::class)
		   "BGL_CLASS_DIRECT_FIELDS")
	    (macro $class-direct-fields-set!::obj (::class ::vector)
		   "BGL_CLASS_DIRECT_FIELDS_SET")
	    (macro $class-all-fields::vector (::class)
		   "BGL_CLASS_ALL_FIELDS")
	    (macro $class-all-fields-set!::obj (::class ::vector)
		   "BGL_CLASS_ALL_FIELDS_SET")
	    (macro $class-evdata::obj (::class)
		   "BGL_CLASS_EVDATA")
	    (macro $class-evdata-set!::obj (::class ::obj)
		   "BGL_CLASS_EVDATA_SET")

	    (export bigloo-types-number "bgl_types_number"))

   (java    (class foreign
	       (field static $bigloo-generic-mutex::mutex
		  "bigloo_generic_mutex")
	       (field static %object-type-number::long
		  "OBJECT_TYPE")
	       (method static $object-widening::obj (::object)
		  "BGL_OBJECT_WIDENING")
	       (method static $object-widening-set!::obj (::object ::obj)
		  "BGL_OBJECT_WIDENING_SET")
	       (method static %object?::bool (::obj)
		  "BGL_OBJECTP")
	       (method static %object-class-num::long (::object)
		  "BGL_OBJECT_CLASS_NUM")
	       (method static %object-class-num-set!::obj (::object ::long)
		  "BGL_OBJECT_CLASS_NUM_SET")
	       (method static $make-generic::procedure (::procedure)
		  "bgl_make_generic")
	       (method static %object-hashnumber::int (::obj)
		  "bgl_obj_hash_number")
	       
	       (method static $make-class::class (::symbol ::symbol ::long
						    ::obj ::pair-nil
						    ::procedure ::long
						    ::vector ::vector
						    ::obj ::vector
						    ::obj ::procedure
						    ::obj ::long
						    ::obj)
		  "bgl_make_class")
	       (method static $classp::bool (::obj)
		  "BGL_CLASSP")
	       (method static $class-name::symbol (::class)
		  "BGL_CLASS_NAME")
	       (method static $class-index::long (::class)
		  "BGL_CLASS_INDEX")
	       (method static $class-depth::long (::class)
		  "BGL_CLASS_DEPTH")
	       (method static $class-super::obj (::class)
		  "BGL_CLASS_SUPER")
	       (method static $class-ancestors-ref::class (::class ::long)
		  "BGL_CLASS_ANCESTORS_REF")
	       (method static $class-subclasses::pair-nil (::class)
		  "BGL_CLASS_SUBCLASSES")
	       (method static $class-subclasses-set!::obj (::class ::pair-nil)
		  "BGL_CLASS_SUBCLASSES_SET")
	       (method static $class-nil::obj (::class)
		  "BGL_CLASS_NIL")
	       (method static $class-nil-set!::obj (::class ::obj)
		  "BGL_CLASS_NIL_SET")
	       (method static $class-module::symbol (::class)
		  "BGL_CLASS_MODULE")
	       (method static $class-alloc-fun::procedure (::class)
		  "BGL_CLASS_ALLOC_FUN")
	       (method static $class-hash::long (::class)
		  "BGL_CLASS_HASH")
	       (method static $class-new-fun::procedure (::class)
		  "BGL_CLASS_NEW_FUN")
	       (method static $class-nil-fun::procedure (::class)
		  "BGL_CLASS_NIL_FUN")
	       (method static $class-constructor::obj (::class)
		  "BGL_CLASS_CONSTRUCTOR")
	       (method static $class-shrink::obj (::class)
		  "BGL_CLASS_SHRINK")
	       (method static $class-virtual-fields::vector (::class)
		  "BGL_CLASS_VIRTUAL_FIELDS")
	       (method static $class-direct-fields::vector (::class)
		  "BGL_CLASS_DIRECT_FIELDS")
	       (method static $class-direct-fields-set!::obj (::class ::vector)
		  "BGL_CLASS_DIRECT_FIELDS_SET")
	       (method static $class-all-fields::vector (::class)
		  "BGL_CLASS_ALL_FIELDS")
	       (method static $class-all-fields-set!::obj (::class ::vector)
		  "BGL_CLASS_ALL_FIELDS_SET")
	       (method static $class-evdata::obj (::class)
		  "BGL_CLASS_EVDATA")
	       (method static $class-evdata-set!::obj (::class ::obj)
		  "BGL_CLASS_EVDATA_SET")))

   (export  (class object::object)
	    (class &condition)
	    
	    (class &exception::&condition
	       (fname (default #f))
	       (location (default #f))
	       (stack (default (get-trace-stack))))
	    
	    (class &error::&exception
	       proc
	       (msg read-only)
	       (obj read-only))
	    (class &type-error::&error
	       (type read-only))
	    (class &index-out-of-bounds-error::&error
	       (index read-only))
	    (class &io-error::&error)
	    (class &io-port-error::&io-error)
	    (class &io-read-error::&io-port-error)
	    (class &io-write-error::&io-port-error)
	    (class &io-closed-error::&io-port-error)
	    (class &io-file-not-found-error::&io-error)
	    (class &io-parse-error::&io-error)
	    (class &io-unknown-host-error::&io-error)
	    (class &io-malformed-url-error::&io-error)
	    (class &io-sigpipe-error::&io-error)
	    (class &io-timeout-error::&io-error)
	    (class &io-connection-error::&io-error)
	    
	    (class &process-exception::&error)
	    
	    (class &stack-overflow-error::&error)
	    
	    (class &security-exception::&exception
	       (message::bstring read-only (default "")))
	    (class &access-control-exception::&security-exception
	       (obj::obj read-only (default #unspecified))
	       (permission::obj read-only (default #unspecified)))
	    
	    (class &warning::&exception
	       (args read-only))
	    (class &eval-warning::&warning)

	    (inline bigloo-generic-bucket-pow::int)
	    (inline bigloo-generic-bucket-size::int)
	    (inline bigloo-generic-bucket-mask::int)
	    (bigloo-types-number::long)
	    *classes*
	    (inline object?::bool ::obj)
	    (inline object-class-num::long ::object)
	    (inline object-class-num-set! ::object ::long)
	    (inline object-class::obj ::object)
	    (find-class::class ::symbol)
	    (find-class-by-hash::obj ::int)
	    (class-exists ::symbol)
	    (class?::bool ::obj)
	    (eval-class?::bool ::obj)
	    (class-abstract?::bool ::class)
	    (class-wide?::bool ::class)
	    (class-super ::class)
	    (class-subclasses::pair-nil ::class)
	    (inline class-num::long ::class)
	    (class-name::symbol ::class)
	    (class-module::symbol ::class)
	    (class-hash::long ::class)
	    (class-fields::vector ::class)
	    (inline class-all-fields::vector ::class)
	    (class-evdata::obj ::class)
	    (class-evdata-set! ::class ::obj)
	    (class-evfields-set! ::class ::vector)
	    (find-class-field ::class ::symbol)
	    (class-constructor::obj ::class)
	    (class-allocator::procedure ::class)
	    (class-creator::obj ::class)
	    (inline class-nil::obj ::class)
	    (class-nil-init!::obj ::class)
	    (make-class-field::class-field ::symbol ::obj ::obj ::bool ::bool ::obj ::obj ::obj)
	    (class-field?::bool ::obj)
	    (class-field-name::symbol ::class-field)
	    (class-field-info::obj ::class-field)
	    (class-field-default-value?::bool ::class-field)
	    (class-field-default-value::obj ::class-field)
	    (class-field-virtual?::bool ::class-field)
	    (class-field-accessor::procedure ::class-field)
	    (class-field-mutable?::bool ::class-field)
	    (class-field-mutator::procedure ::class-field)
	    (class-field-type::obj ::class-field)
	    (register-class!::class ::symbol ::symbol ::obj ::long ::obj ::obj ::obj ::procedure ::obj ::obj ::vector)
	    (register-generic!::obj ::procedure ::procedure ::obj ::obj)
	    (generic-add-method!::procedure ::procedure ::obj ::procedure ::obj)
	    (generic-add-eval-method!::procedure ::procedure ::obj ::procedure ::obj)
	    (procedure->generic::procedure ::procedure)
	    (inline find-method::procedure ::object ::procedure)
	    (find-method-from::pair ::object ::procedure class)
	    (find-super-class-method::procedure ::object ::procedure class)
	    (inline generic-default::procedure ::procedure)
	    (inline generic-method-array ::procedure)
	    (inline method-array-ref ::procedure ::vector ::int)
	    (isa?::bool ::obj ::class)
	    (inline %isa/cdepth?::bool ::obj ::class ::long)
	    (inline %isa-object/cdepth?::bool ::object ::class ::long)
	    (inline %isa/final?::bool ::obj ::class)
	    (inline %isa-object/final?::bool ::object ::class)
	    (nil?::bool ::object)
	    (generic object-print ::object ::output-port ::procedure)
	    (generic object-display ::object . port)
	    (generic object-write ::object . port)
	    (generic object-hashnumber::int ::object)
	    (generic object-equal?::bool ::object ::object)
	    
	    (generic exception-notify exc)
	    
	    (allocate-instance::object ::symbol)
	    (inline wide-object?::bool ::object)
	    (inline class-virtual::vector ::class)
	    (call-virtual-getter ::object ::int)
	    (call-virtual-setter ::object ::int ::obj)
	    (call-next-virtual-getter ::obj ::object ::int)
	    (call-next-virtual-setter ::obj ::object ::int ::obj)
	    (inline object-widening::obj ::object)
	    (inline object-widening-set!::obj ::object ::obj)
	    (%object-widening::obj ::object)
	    (%object-widening-set!::obj ::object ::obj)
	    (generic-memory-statistics))
	       
   (static  *nb-classes-max*
	    *nb-classes*::obj
	    *nb-generics-max*
	    *nb-generics*
	    *class-key*
	    (inline generic-default-set! ::procedure ::procedure)
	    (inline generic-method-array-set! ::procedure ::vector))

   (pragma  (%object-class-num args-safe)
	    (%object-class-num-set! args-safe)
            (class? fail-safe side-effect-free no-cfa-top nesting (effect))
	    (class-super side-effect-free no-cfa-top no-trace nesting)
	    (class-subclasses side-effect-free no-cfa-top no-trace nesting)
	    (class-constructor side-effect-free no-cfa-top no-trace nesting)
	    (class-creator side-effect-free no-cfa-top no-trace nesting)
	    (class-nil side-effect-free no-cfa-top no-trace nesting)
	    (class-num side-effect-free no-cfa-top no-trace nesting)
	    (class-name side-effect-free no-cfa-top no-trace nesting)
	    (class-module side-effect-free no-cfa-top no-trace nesting)
	    (object-class side-effect-free no-cfa-top no-trace nesting)
	    (find-super-class-method side-effect-free no-cfa-top no-trace nesting)
	    (isa? fail-safe side-effect-free no-cfa-top no-trace nesting (effect))
	    (%isa/cdepth? fail-safe side-effect-free no-cfa-top no-trace nesting (effect))
	    (%isa-object/cdepth? fail-safe side-effect-free no-cfa-top no-trace nesting (effect))
	    (%isa/final? fail-safe side-effect-free no-cfa-top no-trace nesting (effect))
	    (%isa-object/final? fail-safe side-effect-free no-cfa-top no-trace nesting (effect))
	    (%object? (predicate-of object) no-cfa-top nesting)
	    (wide-object? side-effect-free no-cfa-top no-trace nesting)
	    (object-widening side-effect-free no-cfa-top nesting)
	    (register-class! no-init-flow)))

;*---------------------------------------------------------------------*/
;*    bigloo-generic-bucket-size ...                                   */
;*    -------------------------------------------------------------    */
;*    This variable is used for the generic functions compaction.      */
;*    It is not obvious that the larger this number is, the more       */
;*    compact the generic function tables are. I think there is        */
;*    some need for experimentation here in order to select the        */
;*    *best* value.                                                    */
;*                                                                     */
;*      * Without compaction, each generic function has an array of    */
;*        methods whose size is exactly the number of declared         */
;*        classes.                                                     */
;*      * With compaction, the method array size is devided by the     */
;*        value of BIGLOO-GENERIC-BUCKET-SIZE. Each value contained in */
;*        the method array is another array of size                    */
;*        BIGLOO-GENERIC-BUCKET-SIZE. Looking for a method requires    */
;*        two memory reads. However, this method is reasonably fast    */
;*        and efficient. The compaction comes from that all the bucket */
;*        arrays full of DEFAULT-METHOD are shared amongst the generic */
;*        functions method array.                                      */
;*                                                                     */
;*    Because of the compaction framework, we enforce the size of      */
;*    the generic vectors to be a multiple of                          */
;*    BIGLOO-GENERIC-BUCKET-SIZE.                                      */
;*---------------------------------------------------------------------*/
(define-inline (bigloo-generic-bucket-pow)
   4)
(define-inline (bigloo-generic-bucket-size)
   (bit-lsh 1 (bigloo-generic-bucket-pow)))
(define-inline (bigloo-generic-bucket-mask)
   (-fx (bigloo-generic-bucket-size) 1))

;*---------------------------------------------------------------------*/
;*    bigloo-types-number ...                                          */
;*    -------------------------------------------------------------    */
;*    Returns the number of currently defined types in the running     */
;*    application. This function is used inside Bdb in order to        */
;*    allocate the allocation tables.                                  */
;*---------------------------------------------------------------------*/
(define (bigloo-types-number)
   (if (fixnum? *nb-classes*)
       (+fx %object-type-number *nb-classes*)
       %object-type-number))

;*---------------------------------------------------------------------*/
;*    make-class ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-class name::symbol module::symbol num::long
	   super::obj sub::pair-nil 
	   alloc::procedure ha::long
	   fd::vector allfd::vector
	   constr::obj virt::vector
	   new::obj nil::procedure
	   shrink::obj depth::long
	   evdata)
   ($make-class name module num 
      super sub
      alloc ha
      fd allfd
      constr virt new nil shrink depth
      evdata))

;*---------------------------------------------------------------------*/
;*    class? ...                                                       */
;*---------------------------------------------------------------------*/
(define (class? obj)
   ($classp obj))

;*---------------------------------------------------------------------*/
;*    class-exists ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-exists::obj cname)
   (let loop ((i 0))
      (unless (=fx i *nb-classes*)
	 (let ((cla (vector-ref-ur *classes* i)))
	    (if (eq? (class-name cla) cname)
		cla
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    find-class ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-class cname)
   (or (class-exists cname) 
       (error "find-class" "Cannot find class" cname)))

;*---------------------------------------------------------------------*/
;*    find-class-by-hash ...                                           */
;*---------------------------------------------------------------------*/
(define (find-class-by-hash hash)
   (let loop ((i 0))
      (unless (=fx i *nb-classes*)
	 (let ((cla (vector-ref-ur *classes* i)))
	    (if (eq? (class-hash cla) hash)
		cla
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    eval-class? ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-class? obj)
   (and (class? obj) (class-evdata obj)))

;*---------------------------------------------------------------------*/
;*    class-name ...                                                   */
;*---------------------------------------------------------------------*/
(define (class-name class)
   ($class-name class))

;*---------------------------------------------------------------------*/
;*    class-module ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-module class)
   ($class-module class))

;*---------------------------------------------------------------------*/
;*    class-num ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (class-num class)
   ($class-index class))

;*---------------------------------------------------------------------*/
;*    class-virtual ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (class-virtual class)
   ($class-virtual-fields class))

;*---------------------------------------------------------------------*/
;*    class-evdata ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-evdata class)
   ($class-evdata class))

;*---------------------------------------------------------------------*/
;*    class-evdata-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (class-evdata-set! class data)
   ($class-evdata-set! class data))

;*---------------------------------------------------------------------*/
;*    class-evfields-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (class-evfields-set! class fields)
   (cond
      ((not (eval-class? class))
       (error "class-evfields-set!" "Not an eval class" class))
      ((>fx (vector-length (class-fields class)) 0)
       (error "class-evfields-set!" "Fields already set" class))
      (else
       (let ((sfields (class-all-fields (class-super class))))
	  ($class-direct-fields-set! class fields)
	  ($class-all-fields-set! class (vector-append sfields fields))))))

;*---------------------------------------------------------------------*/
;*    class-fields ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-fields class)
   ($class-direct-fields class))

;*---------------------------------------------------------------------*/
;*    class-all-fields ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (class-all-fields class)
   ($class-all-fields class))

;*---------------------------------------------------------------------*/
;*    find-class-field ...                                             */
;*---------------------------------------------------------------------*/
(define (find-class-field class name::symbol)
   (let ((fields (class-all-fields class)))
      (let loop ((i (-fx (vector-length fields) 1)))
	 (if (=fx i -1)
	     #f
	     (let ((f (vector-ref-ur fields i)))
		(if (eq? (class-field-name f) name)
		    f
		    (loop (-fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    make-class-field ...                                             */
;*---------------------------------------------------------------------*/
(define (make-class-field name getter setter ronly virtual info default type)
   (vector name getter setter virtual make-class-field
      info default type (not ronly)))

;*---------------------------------------------------------------------*/
;*    class-field? ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-field? obj)
   (and (vector? obj)
	(=fx (vector-length obj) 9)
	(eq? (vector-ref-ur obj 4) make-class-field)))
	 
;*---------------------------------------------------------------------*/
;*    class-field-name ...                                             */
;*---------------------------------------------------------------------*/
(define (class-field-name::symbol field)
   (vector-ref-ur field 0))

;*---------------------------------------------------------------------*/
;*    class-field-virtual? ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-virtual?::bool field)
   (vector-ref-ur field 3))

;*---------------------------------------------------------------------*/
;*    class-field-accessor ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-accessor::procedure field)
   (vector-ref-ur field 1))

;*---------------------------------------------------------------------*/
;*    class-field-mutable? ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-mutable?::bool field)
   (vector-ref-ur field 8))

;*---------------------------------------------------------------------*/
;*    class-field-mutator ...                                          */
;*---------------------------------------------------------------------*/
(define (class-field-mutator::procedure field)
   (vector-ref-ur field 2))

;*---------------------------------------------------------------------*/
;*    class-field-info ...                                             */
;*---------------------------------------------------------------------*/
(define (class-field-info field)
   (vector-ref-ur field 5))

;*---------------------------------------------------------------------*/
;*    class-field-default-value? ...                                   */
;*---------------------------------------------------------------------*/
(define (class-field-default-value? field)
   (procedure? (vector-ref-ur field 6)))

;*---------------------------------------------------------------------*/
;*    class-field-default-value ...                                    */
;*---------------------------------------------------------------------*/
(define (class-field-default-value field)
   ;; the index is internally used in the function
   ;; (@ EVAL-REGISTER-CLASS __EVOBJECT)
   (let ((p (vector-ref-ur field 6)))
      (if (procedure? p)
	  (p)
	  (error "class-field-default-value"
	     "This field has no default value"
	     (class-field-name field)))))

;*---------------------------------------------------------------------*/
;*    class-field-type ...                                             */
;*---------------------------------------------------------------------*/
(define (class-field-type field)
   (vector-ref-ur field 7))

;*---------------------------------------------------------------------*/
;*    class-super ...                                                  */
;*---------------------------------------------------------------------*/
(define (class-super class)
   ($class-super class))
		     
;*---------------------------------------------------------------------*/
;*    class-depth ...                                                  */
;*---------------------------------------------------------------------*/
(define (class-depth class)
   ($class-depth class))
		     
;*---------------------------------------------------------------------*/
;*    class-ancestors-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (class-ancestors-ref class cdepth::long)
   ($class-ancestors-ref class cdepth))
		     
;*---------------------------------------------------------------------*/
;*    class-abstract? ...                                              */
;*---------------------------------------------------------------------*/
(define (class-abstract? class)
   (not (procedure? (class-allocator class))))
		     
;*---------------------------------------------------------------------*/
;*    class-wide? ...                                                  */
;*---------------------------------------------------------------------*/
(define (class-wide? class)
   (procedure? (class-shrink class)))
		     
;*---------------------------------------------------------------------*/
;*    class-subclasses ...                                             */
;*---------------------------------------------------------------------*/
(define (class-subclasses class)
   ($class-subclasses class))
		     
;*---------------------------------------------------------------------*/
;*    class-subclasses-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (class-subclasses-set! class sub)
   ($class-subclasses-set! class sub))

;*---------------------------------------------------------------------*/
;*    class-allocator ...                                              */
;*---------------------------------------------------------------------*/
(define (class-allocator class)
   (if (class? class)
       ($class-alloc-fun class)
       (bigloo-type-error "class-allocator" "class" class)))

;*---------------------------------------------------------------------*/
;*    class-hash ...                                                   */
;*---------------------------------------------------------------------*/
(define (class-hash class)
   ($class-hash class))

;*---------------------------------------------------------------------*/
;*    class-constructor ...                                            */
;*---------------------------------------------------------------------*/
(define (class-constructor class)
   ($class-constructor class))

;*---------------------------------------------------------------------*/
;*    class-creator ...                                                */
;*---------------------------------------------------------------------*/
(define (class-creator class)
   ($class-new-fun class))

;*---------------------------------------------------------------------*/
;*    class-nil-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (class-nil-init! class)
   (let ((proc ($class-nil-fun class)))
      (if (class-wide? class)
	  (let* ((super (class-super class))
		 (o ((class-allocator super)))
		 (wo ((class-allocator class) o)))
	     ($class-nil-set! class wo)
	     (proc wo)
	     wo)
	  (let ((o ((class-allocator class))))
	     ($class-nil-set! class o)
	     (proc o)
	     o))))

;*---------------------------------------------------------------------*/
;*    class-nil ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (class-nil class)
   (cond-expand
      (bigloo-unsafe-type
       ;;; disable type checking
       (or ($class-nil class) (class-nil-init! class)))
      (else
       (if (class? class)
	   (or ($class-nil class) (class-nil-init! class))
	   (bigloo-type-error "class-nil" "class" class)))))

;*---------------------------------------------------------------------*/
;*    class-shrink ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-shrink class)
   (if (class? class)
       ($class-shrink class)
       (bigloo-type-error "class-shrink" "class" class)))

;*---------------------------------------------------------------------*/
;*    Classes                                                          */
;*    -------------------------------------------------------------    */
;*    See the initialize-objects! function to understand these         */
;*    stranges affections.                                             */
;*---------------------------------------------------------------------*/
(define *nb-classes-max* *nb-classes-max*)  
(define *nb-classes* *nb-classes*)
(define *classes* *classes*)

;*---------------------------------------------------------------------*/
;*    Generics                                                         */
;*---------------------------------------------------------------------*/
(define *nb-generics-max* *nb-generics-max*)
(define *nb-generics* *nb-generics*)
(define *generics* *generics*)
(define *class-key* *class-key*)

;*---------------------------------------------------------------------*/
;*    initialized-objects? ...                                         */
;*---------------------------------------------------------------------*/
(define (initialized-objects?)
   (fixnum? *nb-classes*))

;*---------------------------------------------------------------------*/
;*    initialize-objects! ...                                          */
;*    -------------------------------------------------------------    */
;*    Due to some bootstrap pbms we have to suppose this module        */
;*    is uninitialized before using it. This function makes the        */
;*    initialization.                                                  */
;*---------------------------------------------------------------------*/
(define (initialize-objects!)
   (unless (initialized-objects?)
      (set! *nb-classes* 0)
      (set! *nb-classes-max* 64)
      (set! *classes* ($make-vector-uncollectable *nb-classes-max* #f))
      (set! *nb-generics-max* 64)
      (set! *nb-generics* 0)
      (set! *generics* ($make-vector-uncollectable *nb-generics-max* #f))
      (unless (pair? *class-key*) (set! *class-key* (cons 1 2)))))

;*---------------------------------------------------------------------*/
;*    extend-vector ...                                                */
;*---------------------------------------------------------------------*/
(define (extend-vector old-vec fill extend)
   (let* ((old-len (vector-length old-vec))
	  (new-len (+fx extend old-len))
	  (new-vec ($make-vector-uncollectable new-len fill)))
      (let loop ((i 0))
	 (if (=fx i old-len)
	     new-vec
	     (begin
		(vector-set-ur! new-vec i (vector-ref-ur old-vec i))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    double-vector! ...                                               */
;*---------------------------------------------------------------------*/
(define (double-vector! old-vec fill)
   (let ((new-vec (extend-vector old-vec fill (vector-length old-vec))))
      ($free-vector-uncollectable old-vec)
      new-vec))
      
;*---------------------------------------------------------------------*/
;*    double-nb-classes! ...                                           */
;*---------------------------------------------------------------------*/
(define (double-nb-classes!)
   (set! *nb-classes-max* (*fx 2 *nb-classes-max*))
   (set! *classes* (double-vector! *classes* #f))
   ;; we have to enlarge the method vector for each generic
   (let loop ((i 0))
      (when (<fx i *nb-generics*)
	 (let* ((gen (vector-ref-ur *generics* i))
		(default-bucket (generic-default-bucket gen))
		(old-method-array (generic-method-array gen)))
	    (generic-method-array-set!
	     gen
	     (double-vector! old-method-array default-bucket))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    double-nb-generics! ...                                          */
;*---------------------------------------------------------------------*/
(define (double-nb-generics!)
   (set! *nb-generics-max* (*fx 2 *nb-generics-max*))
   (set! *generics* (double-vector! *generics* #f)))

;*---------------------------------------------------------------------*/
;*    object? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (object? obj)
   (%object? obj))

;*---------------------------------------------------------------------*/
;*    object-class-num ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (object-class-num obj::object)
   (%object-class-num obj))

;*---------------------------------------------------------------------*/
;*    object-class-num-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (object-class-num-set! obj::object num::long)
   (%object-class-num-set! obj num))

;*---------------------------------------------------------------------*/
;*    object-class ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (object-class object::object)
   (vector-ref-ur *classes*
      (-fx (object-class-num object) %object-type-number)))

;*---------------------------------------------------------------------*/
;*    generic-default ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (generic-default generic)
   (procedure-ref generic 0))

(define-inline (generic-default-set! generic default)
   (procedure-set! generic 0 default))

;*---------------------------------------------------------------------*/
;*    generic-method-array ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (generic-method-array generic)
   (procedure-ref generic 1))

(define-inline (generic-method-array-set! generic method-array)
   (procedure-set! generic 1 method-array))

;*---------------------------------------------------------------------*/
;*    generic-default-bucket ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (generic-default-bucket::vector generic)
   (procedure-ref generic 2))

(define-inline (generic-default-bucket-set! generic bucket::vector)
   (procedure-set! generic 2 bucket))

;*---------------------------------------------------------------------*/
;*    method-array-ref ...                                             */
;*    -------------------------------------------------------------    */
;*    For some values of X and Y, on 64-bit machines it might be       */
;*    faster to compute the division and the rest and 32-bit values    */
;*    instead of 64-bit values.                                        */
;*---------------------------------------------------------------------*/
(define-inline (method-array-ref generic::procedure array::vector offset::int)
   (let* ((offset (-fx offset %object-type-number))
	  (mod (bit-rsh offset (bigloo-generic-bucket-pow)))
	  (rest (bit-and offset (bigloo-generic-bucket-mask))))
      (let ((bucket (vector-ref-ur array mod)))
	 (vector-ref-ur bucket rest))))

;*---------------------------------------------------------------------*/
;*    method-array-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (method-array-set! generic array offset method)
   (let* ((offset (-fx offset %object-type-number))
	  (mod (uint32->fixnum
		  (quotientu32
		     (fixnum->uint32 offset)
		     (fixnum->uint32 (bigloo-generic-bucket-size)))))
	  (rest (uint32->fixnum
		   (remainderu32
		      (fixnum->uint32 offset)
		      (fixnum->uint32 (bigloo-generic-bucket-size))))))
      (let ((bucket (vector-ref-ur array mod)))
	 (if (or (eq? method (generic-default generic))
		 (not (eq? bucket (generic-default-bucket generic))))
	     ;; the bucket is already duplicated
	     (vector-set-ur! bucket rest method)
	     ;; we have to uncompact the entry
	     (let ((bucket (copy-vector bucket (bigloo-generic-bucket-size))))
		(vector-set-ur! bucket rest method)
		(vector-set-ur! array mod bucket))))))

;*---------------------------------------------------------------------*/
;*    generic-memory-statistics ...                                    */
;*---------------------------------------------------------------------*/
(define (generic-memory-statistics)
   ;; return the overall memory space required by generic functions
   (synchronize $bigloo-generic-mutex
      (let loop ((g 0)
		 (size 0))
	 (if (=fx g *nb-generics*)
	     `((generic ,*nb-generics*)
	       (class ,*nb-classes*)
	       (mtable-size ,size)
	       (method-array-size ,(vector-length
				      (generic-method-array
					 (vector-ref-ur *generics* 0))))
	       (generic-bucket-size ,(bigloo-generic-bucket-size))
	       (max-class ,*nb-classes-max*)
	       (max-generic ,*nb-generics-max*))
	     (let* ((gen (vector-ref-ur *generics* g))
		    (dbuck (generic-default-bucket gen))
		    (dz 0)
		    (sz (apply + (map (lambda (b)
					 (cond
					    ((eq? b dbuck)
					     (set! dz (*fx (vector-length b) 4))
					     0)
					    (else
					     (*fx 4 (vector-length b)))))
				    (vector->list (generic-method-array gen)))))
		    (bz (*fx 4 (vector-length (generic-method-array gen)))))
		(loop (+fx g 1)
		   (+fx size (+fx (+fx bz dz) sz))))))))
   
;*---------------------------------------------------------------------*/
;*    generics-add-class! ...                                          */
;*    -------------------------------------------------------------    */
;*    For each generic, we add the super class method to the class.    */
;*---------------------------------------------------------------------*/
(define (generics-add-class! class-num super-num)
   (let loop ((g 0))
      (when (<fx g *nb-generics*)
	 (let* ((gen (vector-ref-ur *generics* g))
		(method-array (generic-method-array gen))
		(method (method-array-ref gen method-array super-num)))
	    (method-array-set! gen method-array class-num method)
	    (loop (+fx g 1))))))

;*---------------------------------------------------------------------*/
;*    register-class! ...                                              */
;*---------------------------------------------------------------------*/
(define (register-class! name module super hash creator allocator constructor nil shrink plain virtual)
   (synchronize $bigloo-generic-mutex
      (initialize-objects!)
      (when (and super (not (class? super)))
	 (error name "Illegal super-class for class" super))
      (when (=fx *nb-classes* *nb-classes-max*)
	 (double-nb-classes!))
      (unless (vector? plain)
	 (error "register-class!" "Fields not a vector" plain))
      (let ((k (class-exists name)))
	 (when (class? k)
	    (warning "register-class!" "Dangerous class redefinition: \"" name "@"
	       module
	       "\" (" name "@" (class-module k) ")")))
      (let* ((num   (+fx %object-type-number *nb-classes*))
	     (depth (if (class? super)
			(+fx (class-depth super) 1)
			0))
	     (class (make-class name module
		       num
		       super
		       '()
		       allocator
		       hash
		       plain
		       (if (class? super)
			   (vector-append (class-all-fields super) plain)
			   plain)
		       constructor
		       (make-class-virtual-slots-vector super virtual)
		       creator
		       nil
		       shrink
		       depth
		       #f)))
	 ;; we set the sub field of the super class
	 (when (class? super)
	    ;; add the class to its super subclasses list
	    (class-subclasses-set!
	       super (cons class (class-subclasses super))))
	 ;; we add the class in the *classes* vector (we declare the class)
	 (vector-set! *classes* *nb-classes* class)
	 ;; we increment the global class number
	 (set! *nb-classes* (+fx *nb-classes* 1))
	 ;; and we adjust the method arrays of all generic functions
	 (generics-add-class! num (if (class? super) (class-num super) num))
	 class)))

;*---------------------------------------------------------------------*/
;*    make-class-virtual-slots-vector ...                              */
;*---------------------------------------------------------------------*/
(define (make-class-virtual-slots-vector::vector super virtuals)
   
   (define (fill-vector-with-virtuals!::vector vec::vector)
      (for-each (lambda (virtual)
		   (let ((num (car virtual)))
		      (vector-set! vec num (cdr virtual))))
		(vector->list virtuals))
      vec)

   (if (not (class? super))
       (let* ((len (vector-length virtuals))
	      (vec (make-vector len)))
	  (fill-vector-with-virtuals! vec))
       (let* ((ovec (class-virtual super))
	      (olen (vector-length ovec))
	      (len  (vector-length virtuals))
	      (vec  (make-vector (+fx olen len))))
	  (let loop ((i 0))
	     (if (=fx i olen)
		 (fill-vector-with-virtuals! vec)
		 (begin
		    (vector-set! vec i (vector-ref-ur ovec i))
		    (loop (+fx i 1))))))))
	 
;*---------------------------------------------------------------------*/
;*    generic-registered? ...                                          */
;*---------------------------------------------------------------------*/
(define (generic-registered? generic::procedure)
   (vector? (generic-method-array generic)))

;*---------------------------------------------------------------------*/
;*    make-method-array ...                                            */
;*---------------------------------------------------------------------*/
(define (make-method-array def-bucket::vector)
   (let ((s (quotientfx *nb-classes-max* (bigloo-generic-bucket-size)))
	 (a (remainderfx *nb-classes-max* (bigloo-generic-bucket-size))))
      (if (>fx a 0)
	  (begin
	     (warning "make-method-array"
		      "unoptimal bigloo-generic-bucket-size: "
		      (bigloo-generic-bucket-size))
	     ($make-vector-uncollectable (+fx s 1) def-bucket))
	  ($make-vector-uncollectable s def-bucket))))

;*---------------------------------------------------------------------*/
;*    generic-no-default-behavior ...                                  */
;*---------------------------------------------------------------------*/
(define (generic-no-default-behavior . l)
   (error "generic" "No default behavior" l))

;*---------------------------------------------------------------------*/
;*    procedure->generic ...                                           */
;*---------------------------------------------------------------------*/
(define (procedure->generic proc)
   ($make-generic proc))

;*---------------------------------------------------------------------*/
;*    register-generic! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-generic! generic default class-min name)
   (synchronize $bigloo-generic-mutex
      (register-generic-sans-lock! generic default name)))

;*---------------------------------------------------------------------*/
;*    register-generic-sans-lock! ...                                  */
;*    -------------------------------------------------------------    */
;*    Adding a generic could be a two steps process. It may happen,    */
;*    because of cycle in the module graph, that we see the first      */
;*    method before the generic itself. In such a situation, we        */
;*    declare a dummy generic with a default body that is an error.    */
;*    Then, in the second time, we will have to fix the default body   */
;*    for that generic.                                                */
;*---------------------------------------------------------------------*/
(define (register-generic-sans-lock! generic default name)
   (if (not (generic-registered? generic))
       (let* ((def-met (if (procedure? default)
			   default
			   generic-no-default-behavior))
	      (def-bucket ($make-vector-uncollectable
			   (bigloo-generic-bucket-size) def-met)))
	  (when (=fx *nb-generics* *nb-generics-max*)
	     (double-nb-generics!))
	  (vector-set! *generics* *nb-generics* generic)
	  (set! *nb-generics* (+fx *nb-generics* 1))
	  (generic-default-set! generic def-met)
	  (generic-default-bucket-set! generic def-bucket)
	  (generic-method-array-set! generic (make-method-array def-bucket))
	  #unspecified)
       (begin
	  (when (procedure? default)
	     ;; We have to adjust the generic default bucket and the
	     ;; generic method array. We have to plug the new default
	     ;; function
	     (let ((old-def-bucket (generic-default-bucket generic))
		   (new-def-bucket ($make-vector-uncollectable
				    (bigloo-generic-bucket-size) default))
		   (old-default (generic-default generic)))
		(let* ((marray (generic-method-array generic))
		       (alen (vector-length marray)))
		   (let loop ((i 0))
		      (if (<fx i alen)
			  (let ((bucket (vector-ref-ur marray i)))
			     (if (eq? bucket old-def-bucket)
				 (begin
				    (vector-set! marray i new-def-bucket)
				    (loop (+fx i 1)))
				 (let laap ((j 0))
				    (cond
				       ((=fx j (bigloo-generic-bucket-size))
					(loop (+fx i 1)))
				       ((eq? (vector-ref-ur bucket j) old-default)
					(vector-set! bucket j default)
					(laap (+fx j 1)))
				       (else
					(laap (+fx j 1)))))))
			  (begin
			     (generic-default-set! generic default)
			     (generic-default-bucket-set! generic new-def-bucket)
			     
			     ($free-vector-uncollectable old-def-bucket)))))))
	  #unspecified)))

;*---------------------------------------------------------------------*/
;*    %add-method! ...                                                 */
;*---------------------------------------------------------------------*/
(define (%add-method! generic class method)
   (synchronize $bigloo-generic-mutex
      (unless (generic-registered? generic)
	 ;; we check the installation of the generic in order to
	 ;; allow cycle in module graph.
	 (register-generic-sans-lock! generic #f ""))
      (let* ((method-array (generic-method-array generic))
	     (cnum (class-num class))
	     (previous (method-array-ref generic method-array cnum))
	     (def (generic-default generic)))
	 (let loop ((clazz class))
	    (let* ((cn (class-num clazz))
		   (current (method-array-ref generic method-array cn)))
	       (if (or (eq? current def) (eq? current previous))
		   (begin
		      ;; we add the method
		      (method-array-set! generic method-array cn method)
		      ;; and we recursively iterate on subclasses
		      (for-each loop (class-subclasses clazz)))))))
      method))

;*---------------------------------------------------------------------*/
;*    generic-add-method! ...                                          */
;*---------------------------------------------------------------------*/
(define (generic-add-method! generic class method name)
   (cond
      ((not (class? class))
       (error name "Illegal class for method" class))
      ((and (not (=fx (procedure-arity generic) (procedure-arity method)))
	    (>=fx (procedure-arity generic) 0))
       (error name (format "method/generic arity mismatch, expecting ~a"
		      (procedure-arity generic))
	  (procedure-arity method)))
      (else
       (%add-method! generic class method))))

;*---------------------------------------------------------------------*/
;*    generic-add-eval-method! ...                                     */
;*    -------------------------------------------------------------    */
;*    This function is similar to ADD-METHOD! but the arity check      */
;*    differs. Since eval function of more than four arguments are     */
;*    implemented with -1-arity procedure, ADD-EVAL-METHOD must        */
;*    enforce a more permissive arity check.                           */
;*---------------------------------------------------------------------*/
(define (generic-add-eval-method! generic class method name)
   (cond
      ((not (class? class))
       (error name "Illegal class for method" class))
      ((and (not (=fx (procedure-arity generic) (procedure-arity method)))
	    (>fx (procedure-arity generic) 4)
	    (>=fx (procedure-arity method) 0))
       (error name (format "method/generic arity mismatch, expecting ~a"
		      (procedure-arity generic))
	  (procedure-arity method)))
      (else
       (%add-method! generic class method))))

;*---------------------------------------------------------------------*/
;*    find-method ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function returns the most specific method of a generic      */
;*---------------------------------------------------------------------*/
(define-inline (find-method obj generic)
   (let ((obj-class-num (object-class-num obj)))
      (method-array-ref generic (generic-method-array generic) obj-class-num)))

;*---------------------------------------------------------------------*/
;*    find-super-class-method ...                                      */
;*    -------------------------------------------------------------    */
;*    This function returns a method OR a default body.                */
;*---------------------------------------------------------------------*/
(define (find-super-class-method obj generic class)
   (let loop ((super (class-super class)))
      (if (not (class? super))
	  (generic-default generic)
	  (let ((obj-super-class-num (class-num super)))
	     (let ((method (method-array-ref generic
			      (generic-method-array generic)
			      obj-super-class-num)))
		(if method
		    method
		    (let ((new-super (class-super super)))
		       (loop new-super))))))))

;*---------------------------------------------------------------------*/
;*    find-method-from ...                                             */
;*---------------------------------------------------------------------*/
(define (find-method-from obj generic class)
   (let loop ((class class))
      (if (not (class? class))
	  (cons #f #f)
	  (let ((obj-super-class-num (class-num class)))
	     (let ((method (method-array-ref generic
			      (generic-method-array generic)
			      obj-super-class-num)))
		(if method
		    (cons class method)
		    (loop (class-super class))))))))
   
;*---------------------------------------------------------------------*/
;*    nil? ...                                                         */
;*---------------------------------------------------------------------*/
(define (nil? obj::object)
   (let ((klass (object-class obj)))
      (eq? (class-nil klass) obj)))

;*---------------------------------------------------------------------*/
;*    isa? ...                                                         */
;*    -------------------------------------------------------------    */
;*    The constant-time and thread-safe implementation of is-a?        */
;*---------------------------------------------------------------------*/
(define (isa? obj class)
   (if (object? obj)
       (let ((oclass (object-class obj)))
	  (if (eq? oclass class)
	      #t
	      (let ((odepth (class-depth oclass))
		    (cdepth (class-depth class)))
		 (if (<fx cdepth odepth)
		     (eq? (class-ancestors-ref oclass cdepth) class)
		     #f))))
       #f))

;*---------------------------------------------------------------------*/
;*    %isa/cdepth? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (%isa/cdepth? obj class cdepth)
   (when (object? obj)
      (let ((oclass (object-class obj)))
	 (eq? ($class-ancestors-ref oclass cdepth) class))))

;*---------------------------------------------------------------------*/
;*    %isa-object/cdepth? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (%isa-object/cdepth? obj class cdepth)
   (let ((oclass (object-class obj)))
      (eq? ($class-ancestors-ref oclass cdepth) class)))

;*---------------------------------------------------------------------*/
;*    %isa/final? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (%isa/final? obj class)
   (when (object? obj)
      (let ((oclass (object-class obj)))
	 (eq? oclass class))))

;*---------------------------------------------------------------------*/
;*    %isa-object/final? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (%isa-object/final? obj class)
   (let ((oclass (object-class obj)))
      (eq? oclass class)))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (object-display obj::object . port)
   (let ((port (if (pair? port) (car port) (current-output-port))))
      (object-print obj port display)))

;*---------------------------------------------------------------------*/
;*    object-write ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (object-write obj::object . port)
   (let ((port (if (pair? port) (car port) (current-output-port))))
      (object-print obj port write)))

;*---------------------------------------------------------------------*/
;*    object-hashnumber ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (object-hashnumber::int object::object)
   (%object-hashnumber object))

;*---------------------------------------------------------------------*/
;*    allocate-instance ...                                            */
;*---------------------------------------------------------------------*/
(define (allocate-instance::object cname::symbol)
   (let loop ((i 0))
      (if (=fx i *nb-classes*)
	  (error "allocate-instance" "Cannot find class" cname)
	  (let ((class (vector-ref-ur *classes* i)))
	     (if (eq? (class-name class) cname)
		 (let ((alloc (class-allocator class)))
		    (if (class-wide? class)
			;; test to be removed when class generators
			;; are removed from the compiler
			(if (=fx (procedure-arity alloc) 0)
			    (alloc)
			    (let* ((super (class-super class))
				   (o ((class-allocator super))))
			       (alloc o)))
			(alloc)))
		 (loop (+fx i 1)))))))
      
;*---------------------------------------------------------------------*/
;*    wide-object? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (wide-object? object)
   (if (object-widening object) #t #f))
 
;*---------------------------------------------------------------------*/
;*    object-print ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (object-print obj::object port print-slot::procedure)
   
   (define (class-field-write/display field)
      (let* ((name (class-field-name field))
	     (get-value (class-field-accessor field)))
	 (display " [" port)
	 (display name port)
	 (display #\: port)
	 (display #\space port)
	 (print-slot (get-value obj) port)
	 (display #\] port)))

   (let* ((class (object-class obj))
	  (class-name (class-name class))
	  (fields (class-all-fields class))
	  (len (vector-length fields)))
      (display "#|" port)
      (display class-name port)
      (if (nil? obj)
	  (display " nil|" port)
	  (let loop ((i 0))
	     (if (=fx i len)
		 (display #\| port)
		 (begin
		    (class-field-write/display (vector-ref-ur fields i))
		    (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    object-equal? ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (object-equal?::bool obj1::object obj2::object)
   (define (class-field-equal? field)
      (let ((get-value (class-field-accessor field)))
	 (equal? (get-value obj1) (get-value obj2))))
   (let ((class1 (object-class obj1))
	 (class2 (object-class obj2)))
      (when (eq? class1 class2)
	 (let ((fields (class-all-fields class1)))
	    (let loop ((i (-fx (vector-length fields) 1)))
	       (cond
		  ((=fx i -1)
		   #t)
		  ((class-field-equal? (vector-ref-ur fields i))
		   (loop (-fx i 1)))
		  (else
		   #f)))))))

;*---------------------------------------------------------------------*/
;*    exception-notify ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (exception-notify exc::obj)
   (let ((port (current-error-port)))
      (display "*** UNKNOWN EXCEPTION: " port)
      (write-circle exc port)
      (when (current-thread)
	 (display " [[" port)
	 (display (current-thread) port)
	 (display "]]" port))
      (newline port)
      (let ((stack (if (isa? exc &exception)
		       (with-access::&exception exc (stack)
			 (or stack (get-trace-stack)))
		       (get-trace-stack))))
	 (display-trace-stack stack port))))

;*---------------------------------------------------------------------*/
;*    exception-notify ::&error ...                                    */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::&error)
   (error-notify exc))

;*---------------------------------------------------------------------*/
;*    exception-notify ::&io-write-error ...                           */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::&io-write-error)
   (with-access::&io-write-error exc (obj)
      (unless (eq? obj (current-error-port))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    exception-notify ::&warning ...                                  */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::&warning)
   (warning-notify exc))

;*---------------------------------------------------------------------*/
;*    call-virtual-getter ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is used by all the class generated getters in      */
;*    order to call the correct getter of an instance. This function   */
;*    applies the good function with respect to the dynamic instance's */
;*    class.                                                           */
;*---------------------------------------------------------------------*/
(define (call-virtual-getter obj::object num::int)
   (let* ((class (object-class obj))
	  (getter (car (vector-ref-ur (class-virtual class) num))))
      (getter obj)))

;*---------------------------------------------------------------------*/
;*    call-virtual-setter ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is used by all the class generated setters in      */
;*    order to call the correct setter of an instance. This function   */
;*    applies the good function with respect to the dynamic instance's */
;*    class.                                                           */
;*---------------------------------------------------------------------*/
(define (call-virtual-setter obj::object num::int value)
   (let* ((class (object-class obj))
	  (setter (cdr (vector-ref-ur (class-virtual class) num))))
      (setter obj value)))

;*---------------------------------------------------------------------*/
;*    call-next-virtual-getter ...                                     */
;*    -------------------------------------------------------------    */
;*    This function is used by all the class generated getters in      */
;*    order to call the getter of the super class of an instance.      */
;*    This is equivalent to CALL-NEXT-METHOD for generic function.     */
;*    -------------------------------------------------------------    */
;*    CALL-NEXT-SLOT is defined inside a virtual slot only when the    */
;*    class holding the slot inherits from a class that already own    */
;*    such a slot. That is, CALL-NEXT-VIRTUAL-GETTER always succeeds.  */
;*    -------------------------------------------------------------    */
;*    The arguments are:                                               */
;*      - OBJ is the object we are fetching the virtual slot value     */
;*      - CLASS is the class of the class containing the slot that     */
;*        calls to CALL-NEXT-SLOT                                      */
;*        the definition of the virtual slot                           */
;*      - NUM is the number of the virtual slot                        */
;*---------------------------------------------------------------------*/
(define (call-next-virtual-getter class obj::object num::int)
   (let ((next-class (class-super class)))
      ((car (vector-ref-ur (class-virtual next-class) num)) obj)))

;*---------------------------------------------------------------------*/
;*    call-next-virtual-setter ...                                     */
;*    -------------------------------------------------------------    */
;*    This function is used by all the class generated setters in      */
;*    order to call the getter of the super class of an instance.      */
;*    This is equivalent to CALL-NEXT-METHOD for generic function.     */
;*    The arguments are explained for CALL-NEXT-VIRTUAL-GETTER.        */
;*---------------------------------------------------------------------*/
(define (call-next-virtual-setter class obj::object num::int value)
   (let ((next-class (class-super class)))
      ((cdr (vector-ref-ur (class-virtual next-class) num)) obj value)))

;*---------------------------------------------------------------------*/
;*    object-widening ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (object-widening o)
   ($object-widening o))

;*---------------------------------------------------------------------*/
;*    object-widening-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (object-widening-set! o v)
   ($object-widening-set! o v)
   o)

;*---------------------------------------------------------------------*/
;*    %object-widening ...                                             */
;*    -------------------------------------------------------------    */
;*    This function is used when defining a class within the           */
;*    interpreter.                                                     */
;*---------------------------------------------------------------------*/
(define (%object-widening o)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    %object-widening-set! ...                                        */
;*    -------------------------------------------------------------    */
;*    This function is used when defining a class within the           */
;*    interpreter.                                                     */
;*---------------------------------------------------------------------*/
(define (%object-widening-set! o w)
   (object-widening-set! o w)
   o)

