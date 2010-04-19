;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 25 14:20:42 1996                          */
;*    Last change :  Mon Apr 19 15:06:12 2010 (serrano)                */
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
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
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

   (extern  (macro object-widening::obj (::object)
		   "BGL_OBJECT_WIDENING")
	    (macro object-widening-set!::obj (::object ::obj)
		   "BGL_OBJECT_WIDENING_SET")
	    (macro %object-type-number::long
		   "OBJECT_TYPE")
	    (macro %object?::bool (::obj)
		   "BGL_OBJECTP")
	    (macro %object-class-num::long (::object)
		   "BGL_OBJECT_CLASS_NUM")
	    (macro %object-class-num-set!::obj (::object ::long)
		   "BGL_OBJECT_CLASS_NUM_SET")
	    ($bigloo-generic-mutex::mutex "bigloo_generic_mutex")
	    (%object-hashnumber::int (::obj) "bgl_obj_hash_number")
	    ($make-generic::procedure (::procedure) "bgl_make_generic")
	    (export bigloo-types-number "bgl_types_number"))

   (java    (class foreign
	       (field static $bigloo-generic-mutex::mutex
		      "bigloo_generic_mutex")
	       (field static %object-type-number::long
		      "OBJECT_TYPE")
	       (method static object-widening::obj (::object)
		       "BGL_OBJECT_WIDENING")
	       (method static object-widening-set!::obj (::object ::obj)
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
		       "bgl_obj_hash_number")))

   (export  (class object::object)
	    (class &condition)
	    
	    (class &exception::&condition
	       (fname (default #f))
	       (location (default #f))
	       (stack (default #f)))
	    
	    (class &error::&exception
	       (proc read-only)
	       (msg read-only)
	       (obj read-only))
	    (class &type-error::&error
	       (type read-only))
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
	    
	    (class &process-exception::&error)
	    
	    (class &security-exception::&exception
	       (message::bstring read-only (default "")))
	    (class &access-control-exception::&security-exception
	       (obj::obj read-only (default #unspecified))
	       (permission::obj read-only (default #unspecified)))
	    
	    (class &warning::&exception
	       (args read-only))
	    (class &eval-warning::&warning)

	    (inline bigloo-generic-bucket-size::int)
	    (bigloo-types-number::long)
	    *classes*
	    (inline object?::bool ::obj)
	    (inline object-class-num::long ::object)
	    (inline object-class-num-set! ::object ::long)
	    (inline object-class::obj ::object)
	    (find-class ::symbol)
	    (class?::bool ::obj)
	    (eval-class?::bool ::obj)
	    (class-super class)
	    (class-abstract?::bool class)
	    (class-subclasses class)
	    (class-num::long class)
	    (class-name::symbol class)
	    (class-hash::long class)
	    (class-fields::obj class)
	    (class-evdata::obj class)
	    (class-evdata-set! class ::obj)
	    (class-all-fields::obj class)
	    (find-class-field class ::symbol)
	    (class-constructor::obj class)
	    (class-creator::obj class)
	    (class-nil::obj class)
	    (inline class-fields?::bool fields)
	    (make-class-field::vector ::symbol o o o ::bool ::obj ::obj)
	    (class-field-no-default-value)
	    (class-field?::bool ::obj)
	    (class-field-name::symbol field)
	    (class-field-info::obj field)
	    (class-field-default-value::obj field)
	    (class-field-indexed?::bool field)
	    (class-field-virtual?::bool field)
	    (class-field-accessor::procedure field)
	    (class-field-len-accessor::procedure field)
	    (class-field-mutable?::bool field)
	    (class-field-mutator::procedure field)
	    (register-class!::obj o o ::bool o ::procedure ::procedure ::procedure ::long o o ::vector)
	    (procedure->generic::procedure ::procedure)
	    (add-generic!::obj ::procedure ::obj)
	    (add-method!::procedure ::procedure ::obj ::procedure)
	    (add-eval-method!::procedure ::procedure ::obj ::procedure)
	    (inline find-method ::object ::procedure)
	    (find-class-method class ::procedure)
	    (find-method-from::pair ::object ::procedure class)
	    (find-super-class-method ::object ::procedure class)
	    (inline generic-default::procedure ::procedure)
	    (inline generic-method-array ::procedure)
	    (inline method-array-ref ::procedure ::vector ::int)
	    (is-a?::bool ::obj class::obj)
	    (is-nil?::bool ::object)
	    (generic object-print ::object ::output-port ::procedure)
	    (generic object-display ::object . port)
	    (generic object-write ::object . port)
	    (generic object->struct::struct ::object)
	    (generic struct+object->object::object ::object ::struct)
	    (generic object-hashnumber::int ::object)
	    (generic object-equal?::bool ::object ::object)
	    (struct->object::object ::struct)
	    (allocate-instance::object ::symbol)
	    (inline wide-object?::bool ::object)
	    (inline generic-pre-method-set!::obj ::procedure ::obj)
	    (inline class-virtual::vector ::vector)
	    (call-virtual-getter ::object ::int)
	    (call-virtual-setter ::object ::int ::obj)
	    (call-next-virtual-getter ::obj ::object ::int)
	    (call-next-virtual-setter ::obj ::object ::int ::obj)
	    (%object-widening::obj ::object)
	    (%object-widening-set!::obj ::object ::obj))
	       
   (static  *nb-classes-max*
	    *nb-classes*::obj
	    *nb-generics-max*
	    *nb-generics*
	    *class-key*
	    (make-class ::symbol ::long ::long
			c o a ::procedure ::long obj o ::vector ::obj
			::procedure ::procedure ::obj ::bool)
	    (class-allocate::procedure class)
	    (inline generic-default-set! ::procedure ::procedure)
	    (inline generic-method-array-set! ::procedure ::vector))

   (pragma  (%object-class-num args-safe)
	    (%object-class-num-set! args-safe)
            (class? side-effect-free (predicate-of object) no-cfa-top nesting (effect))
	    (class-super side-effect-free no-cfa-top no-trace nesting)
	    (class-subclasses side-effect-free no-cfa-top no-trace nesting)
	    (class-constructor side-effect-free no-cfa-top no-trace nesting)
	    (class-creator side-effect-free no-cfa-top no-trace nesting)
	    (class-nil side-effect-free no-cfa-top no-trace nesting)
	    (class-num side-effect-free no-cfa-top no-trace nesting)
	    (class-name side-effect-free no-cfa-top no-trace nesting)
	    (object-class side-effect-free no-cfa-top no-trace nesting)
	    (find-super-class-method side-effect-free no-cfa-top no-trace nesting)
	    (is-a? side-effect-free no-cfa-top no-trace nesting (effect))
	    (struct->object no-cfa-top no-trace nesting)
	    (struct+object->object no-cfa-top no-trace nesting)
	    (object->struct side-effect-free no-cfa-top no-trace nesting)
	    (wide-object? side-effect-free no-cfa-top no-trace nesting)
	    (object-widening side-effect-free no-cfa-top nesting)))

;*---------------------------------------------------------------------*/
;*    bigloo-generic-bucket-size ...                                   */
;*    -------------------------------------------------------------    */
;*    This variable is used for the generic functions compaction.      */
;*    It is not obvious that the larger this number is, the more       */
;*    compact the generic function tables are. I think there is        */
;*    some need for experimentation here in order to select the        */
;*    *best* value.                                                    */
;*                                                                     */
;*    Without compaction, each generic function has a array of         */
;*    methods. The size of this array is exactly the number of         */
;*    declared classes. With compaction, the method array size         */
;*    is devided by the value of BIGLOO-GENERIC-BUCKET-SIZE.           */
;*    Each value contained in the method array is another array        */
;*    of size BIGLOO-GENERIC-BUCKET-SIZE. Looking for a method         */
;*    requires two memory read. However, this method is reasonably     */
;*    fast and effecient. The compaction comes from that all the       */
;*    bucket arrays full of DEFAULT-METHOD are shared amongst          */
;*    the generic functions method array.                              */
;*                                                                     */
;*    Because of the compaction framework, we enforce the size of      */
;*    the generic vectors to be a multiple of                          */
;*    BIGLOO-GENERIC_BUCKET_SIZE.                                      */
;*---------------------------------------------------------------------*/
(define-inline (bigloo-generic-bucket-size) 8)

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
(define (make-class name num min super sub max alloc ha fd constr virt inst nil pred evdata abstract)
   (vector name          ;;  0 : the class name
	   num           ;;  1 : the class number
	   min           ;;  2 : the min-number in the class inheritance tree
	   super         ;;  3 : the unique super class
	   sub           ;;  4 : the subclasses
	   max           ;;  5 : the max-num in the class inheritance tree
	   alloc         ;;  6 : the class allocator
	   ha            ;;  7 : the class hashing function
	   fd            ;;  8 : the class fields
	   constr        ;;  9 : the class constructor
	   virt          ;; 10 : the class virtual getter and setter
	   inst          ;; 11 : the function that creates instances
	   nil           ;; 12 : the function that return the NIL object
	   pred          ;; 13 : the class predicate
	   evdata        ;; 14 : field used when declaring a class within eval
	   abstract      ;; 15 : is the class abstract
	   *class-key*)) ;; 16 : a stamp to implement class?

;*---------------------------------------------------------------------*/
;*    find-class ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-class::obj cname)
   (let loop ((i 0))
      (if (=fx i *nb-classes*)
	  (error "find-class" "Can't find class" cname)
	  (let ((cla (vector-ref-ur *classes* i)))
	     (if (eq? (class-name cla) cname)
		 cla
		 (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    class? ...                                                       */
;*---------------------------------------------------------------------*/
(define (class? obj)
   (and (vector? obj)
	(=fx (vector-length obj) 17) (eq? (vector-ref obj 16) *class-key*)))

;*---------------------------------------------------------------------*/
;*    eval-class? ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-class? obj)
   (and (class? obj) (class-evdata obj)))

;*---------------------------------------------------------------------*/
;*    class-name ...                                                   */
;*---------------------------------------------------------------------*/
(define (class-name class)
   (vector-ref class 0))

;*---------------------------------------------------------------------*/
;*    class-num ...                                                    */
;*---------------------------------------------------------------------*/
(define (class-num class)
   (vector-ref-ur class 1))

;*---------------------------------------------------------------------*/
;*    class-min-num ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (class-min-num class)
   (vector-ref-ur class 2))

;*---------------------------------------------------------------------*/
;*    class-min-num-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (class-min-num-set! class v)
   (vector-set-ur! class 2 v))

;*---------------------------------------------------------------------*/
;*    class-max-num ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (class-max-num class)
   (vector-ref-ur class 5))

;*---------------------------------------------------------------------*/
;*    class-max-num-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (class-max-num-set! class v)
   (vector-set-ur! class 5 v))

;*---------------------------------------------------------------------*/
;*    class-virtual ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (class-virtual class)
   (vector-ref-ur class 10))

;*---------------------------------------------------------------------*/
;*    class-evdata ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-evdata class)
   (vector-ref-ur class 14))

;*---------------------------------------------------------------------*/
;*    class-evdata-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (class-evdata-set! class data)
   (vector-set-ur! class 14 data))


;*---------------------------------------------------------------------*/
;*    class-fields ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-fields class)
   (if (class? class)
       (vector-ref class 8)
       (error 'class-fields
	      (bigloo-type-error-msg "runtime type error"
				     "Class"
				     (find-runtime-type class))
	      class)))

;*---------------------------------------------------------------------*/
;*    class-all-fields ...                                             */
;*---------------------------------------------------------------------*/
(define (class-all-fields class)
   (let ((fields (let ((fields (class-fields class)))
		    (if (class-fields? fields)
			fields
			'())))
	 (super (class-super class)))
      (if (class? super)
	  (append (class-all-fields super) fields)
	  fields)))

;*---------------------------------------------------------------------*/
;*    find-class-field ...                                             */
;*---------------------------------------------------------------------*/
(define (find-class-field class name::symbol)
   (define (search fields)
      (cond
         ((null? fields)
          #f)
         ((eq? (class-field-name (car fields)) name)
          (car fields))
         (else
          (search (cdr fields)))))
   (let loop ((class class))
      (if (class? class)
	  (let ((fields (class-fields class)))
	     (if (class-fields? fields)
		 (let ((res (search fields)))
		    (or res (loop (class-super class))))))
	  #f)))

;*---------------------------------------------------------------------*/
;*    make-class-field ...                                             */
;*---------------------------------------------------------------------*/
(define (make-class-field name getter setter indexed virtual info default)
   (vector name getter setter indexed virtual make-class-field info default))

;*---------------------------------------------------------------------*/
;*    class-field? ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-field? obj)
   (and (vector? obj)
	(=fx (vector-length obj) 8)
	(eq? (vector-ref obj 5) make-class-field)))
	 
;*---------------------------------------------------------------------*/
;*    class-fields? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (class-fields? fields)
   (or (pair? fields) (null? fields)))

;*---------------------------------------------------------------------*/
;*    class-field-name ...                                             */
;*---------------------------------------------------------------------*/
(define (class-field-name::symbol field)
   (if (class-field? field)
       (vector-ref field 0)
       (error 'class-field-name "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-indexed? ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-indexed?::bool field)
   (if (class-field? field)
       (procedure? (vector-ref field 3))
       (error 'class-field-indexed? "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-virtual? ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-virtual?::bool field)
   (if (class-field? field)
       (vector-ref field 4)
       (error 'class-field-virtual? "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-accessor ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-accessor::procedure field)
   (if (class-field? field)
       (vector-ref field 1)
       (error 'class-field-accessor "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-mutable? ...                                         */
;*---------------------------------------------------------------------*/
(define (class-field-mutable?::bool field)
   (if (class-field? field)
       (procedure? (vector-ref field 2))
       (error 'class-field-mutable? "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-mutator ...                                          */
;*---------------------------------------------------------------------*/
(define (class-field-mutator::procedure field)
   (if (class-field? field)
       (vector-ref field 2)
       (error 'class-field-mutator "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-len-accessor ...                                     */
;*---------------------------------------------------------------------*/
(define (class-field-len-accessor::procedure field)
   (if (class-field? field)
       (vector-ref field 3)
       (error 'class-field-len-accessor "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-info ...                                             */
;*---------------------------------------------------------------------*/
(define (class-field-info field)
   (if (class-field? field)
       (vector-ref field 6)
       (error 'class-field-info "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-field-default-value ...                                    */
;*---------------------------------------------------------------------*/
(define (class-field-default-value field)
   (if (class-field? field)
       (vector-ref field 7)
       (error 'class-field-default-value "Not a class field" field)))

;*---------------------------------------------------------------------*/
;*    class-super ...                                                  */
;*---------------------------------------------------------------------*/
(define (class-super class)
   (vector-ref class 3))
		     
;*---------------------------------------------------------------------*/
;*    class-abstract? ...                                              */
;*---------------------------------------------------------------------*/
(define (class-abstract? class)
   (vector-ref class 15))
		     
;*---------------------------------------------------------------------*/
;*    class-subclasses ...                                             */
;*---------------------------------------------------------------------*/
(define (class-subclasses class)
   (vector-ref class 4))
		     
;*---------------------------------------------------------------------*/
;*    class-subclasses-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (class-subclasses-set! class sub)
   (vector-set-ur! class 4 sub))

;*---------------------------------------------------------------------*/
;*    class-ancestors ...                                              */
;*---------------------------------------------------------------------*/
(define (class-ancestors class)
   (vector-ref-ur class 5))

;*---------------------------------------------------------------------*/
;*    class-allocate ...                                               */
;*---------------------------------------------------------------------*/
(define (class-allocate class)
   (vector-ref-ur class 6))

;*---------------------------------------------------------------------*/
;*    class-hash ...                                                   */
;*---------------------------------------------------------------------*/
(define (class-hash class)
   (vector-ref-ur class 7))

;*---------------------------------------------------------------------*/
;*    class-constructor ...                                            */
;*---------------------------------------------------------------------*/
(define (class-constructor class)
   (vector-ref class 9))

;*---------------------------------------------------------------------*/
;*    class-creator ...                                                */
;*---------------------------------------------------------------------*/
(define (class-creator class)
   (vector-ref class 11))

;*---------------------------------------------------------------------*/
;*    class-nil ...                                                    */
;*---------------------------------------------------------------------*/
(define (class-nil class)
   (vector-ref class 12))

;*---------------------------------------------------------------------*/
;*    Classes                                                          */
;*    -------------------------------------------------------------    */
;*    See the initialize-objects! function to understand these         */
;*    stranges affections.                                             */
;*---------------------------------------------------------------------*/
(define *nb-classes-max*  *nb-classes-max*)  
(define *nb-classes*      *nb-classes*)
(define *classes*         *classes*)

;*---------------------------------------------------------------------*/
;*    Generics                                                         */
;*---------------------------------------------------------------------*/
(define *nb-generics-max* *nb-generics-max*)
(define *nb-generics*     *nb-generics*)
(define *generics*        *generics*)
(define *class-key*       *class-key*)

;*---------------------------------------------------------------------*/
;*    initialized-objects? ...                                         */
;*---------------------------------------------------------------------*/
(define (initialized-objects?)
   (fixnum? *nb-classes*))

;*---------------------------------------------------------------------*/
;*    initialize-objects! ...                                          */
;*    -------------------------------------------------------------    */
;*    Due to some bootstrap pbms we have to suppose this module        */
;*    is unitialized before using it. This function makes the          */
;*    initialization.                                                  */
;*---------------------------------------------------------------------*/
(define (initialize-objects!)
   (if (initialized-objects?)
       'done
       (begin
	  (set! *nb-classes* 0)
	  (set! *nb-classes-max* 50)
	  (set! *classes* (make-vector *nb-classes-max* #f))
	  (set! *nb-generics-max* 50)
	  (set! *nb-generics* 0)
	  (set! *generics* (make-vector *nb-generics-max* #f))
	  (unless (pair? *class-key*)
	     (set! *class-key* (cons 1 2))))))

;*---------------------------------------------------------------------*/
;*    extend-vector ...                                                */
;*---------------------------------------------------------------------*/
(define (extend-vector old-vec fill extend)
   (let* ((old-len (vector-length old-vec))
	  (new-len (+fx extend old-len))
	  (new-vec (make-vector new-len fill)))
      (let loop ((i 0))
	 (if (=fx i old-len)
	     new-vec
	     (begin
		(vector-set-ur! new-vec i (vector-ref-ur old-vec i))
		(loop (+fx i 1)))))))
      
;*---------------------------------------------------------------------*/
;*    double-vector ...                                                */
;*---------------------------------------------------------------------*/
(define (double-vector old-vec fill)
   (extend-vector old-vec fill (vector-length old-vec)))
      
;*---------------------------------------------------------------------*/
;*    double-nb-classes! ...                                           */
;*---------------------------------------------------------------------*/
(define (double-nb-classes!)
   (set! *nb-classes-max* (*fx 2 *nb-classes-max*))
   (set! *classes* (double-vector *classes* #f))
   ;; we have to enlarge the method vector for each generic
   (let loop ((i 0))
      (if (<fx i *nb-generics*)
	  (let* ((the-generic (vector-ref *generics* i))
		 (default-bucket (generic-default-bucket the-generic))
		 (old-method-array (generic-method-array the-generic)))
	     (generic-method-array-set! the-generic
					(double-vector old-method-array
						       default-bucket))
	     (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    double-nb-generics! ...                                          */
;*---------------------------------------------------------------------*/
(define (double-nb-generics!)
   (set! *nb-generics-max* (*fx 2 *nb-generics-max*))
   (set! *generics* (double-vector *generics* #f)))

;*---------------------------------------------------------------------*/
;*    object? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (object? obj)
   (%object? obj))

;*---------------------------------------------------------------------*/
;*    object-class-num ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (object-class-num obj)
   (%object-class-num obj))

;*---------------------------------------------------------------------*/
;*    object-class-num-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (object-class-num-set! obj num)
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

;*---------------------------------------------------------------------*/
;*    generic-default-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (generic-default-set! generic default)
   (procedure-set! generic 0 default))

;*---------------------------------------------------------------------*/
;*    generic-method-array ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (generic-method-array generic)
   (procedure-ref generic 1))

;*---------------------------------------------------------------------*/
;*    generic-method-array-set! ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (generic-method-array-set! generic method-array)
   (procedure-set! generic 1 method-array))

;*---------------------------------------------------------------------*/
;*    generic-pre-method-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (generic-pre-method-set! generic pre-method)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    generic-default-bucket ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (generic-default-bucket::vector generic)
   (procedure-ref generic 2))

;*---------------------------------------------------------------------*/
;*    generic-default-bucket-set! ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (generic-default-bucket-set! generic bucket::vector)
   (procedure-set! generic 2 bucket))

;*---------------------------------------------------------------------*/
;*    method-array-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (method-array-ref generic::procedure array::vector offset::int)
   (let* ((offset (-fx offset %object-type-number))
	  (mod (quotientfx offset (bigloo-generic-bucket-size)))
	  (rest (remainderfx offset (bigloo-generic-bucket-size))))
      (let ((bucket (vector-ref-ur array mod)))
	 (vector-ref-ur bucket rest))))

;*---------------------------------------------------------------------*/
;*    method-array-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (method-array-set! generic array offset val)
   (let* ((offset (-fx offset %object-type-number))
	  (mod (quotientfx offset (bigloo-generic-bucket-size)))
	  (rest (remainderfx offset (bigloo-generic-bucket-size))))
      (let ((bucket (vector-ref-ur array mod)))
	 (if (or (eq? val (generic-default generic))
		 (not (eq? bucket (generic-default-bucket generic))))
	     (vector-set-ur! bucket rest val)
	     ;; we have to uncompact the entry
	     (let ((bucket (copy-vector bucket (bigloo-generic-bucket-size))))
		(vector-set-ur! bucket rest val)
		(vector-set-ur! array mod bucket))))))
   
;*---------------------------------------------------------------------*/
;*    generics-add-class! ...                                          */
;*    -------------------------------------------------------------    */
;*    For each generic, we add the super class method to the class.    */
;*---------------------------------------------------------------------*/
(define (generics-add-class! class-num super-num)
   (let loop ((g 0))
      (if (=fx g *nb-generics*)
	  'done
	  (let* ((the-generic (vector-ref *generics* g))
		 (method-array (generic-method-array the-generic))
		 (method (method-array-ref the-generic
					   method-array
					   super-num)))
	     (method-array-set! the-generic method-array class-num method)
	     (loop (+fx g 1))))))

;*---------------------------------------------------------------------*/
;*    class-field-no-default-value ...                                 */
;*---------------------------------------------------------------------*/
(define (class-field-no-default-value)
   ;; this value can't be gensymed because it has to traverse libraries
   'slot-no-default-value__17_5_1996)

;*---------------------------------------------------------------------*/
;*    register-class! ...                                              */
;*---------------------------------------------------------------------*/
(define (register-class! name super abstract creator allocate nil predicate hash def constructor virtual)
   (with-lock $bigloo-generic-mutex
      (lambda ()
	 (initialize-objects!)
	 (if (and super (not (class? super)))
	     (error 'add-class! "Illegal super class for class" name))
	 (if (=fx *nb-classes* *nb-classes-max*)
	     (double-nb-classes!))
	 (let* ((num   (+fx %object-type-number *nb-classes*))
		(class (make-class name
				   num
				   -1
				   super
				   '()
				   -1
				   allocate
				   hash
				   def
				   constructor
				   (make-class-virtual-slots-vector super virtual)
				   creator
				   nil
				   predicate
				   #f
				   abstract)))
	    ;; we set the sub field of the super class
	    (if (class? super)
		(begin
		   ;; we add the class to its super subclasses list
		   (class-subclasses-set! super
					  (cons class (class-subclasses super)))
		   ;; and then, we renumber the tree
		   (class-hierarchy-numbering! class super))
		(begin
		   (class-min-num-set! class 1)
		   (class-max-num-set! class 1)))
	    ;; we add the class in the *classes* vector (we declare the class)
	    (vector-set! *classes* *nb-classes* class)
	    ;; we increment the global class number
	    (set! *nb-classes* (+fx *nb-classes* 1))
	    ;; and we ajust the method arrays of all generic functions
	    (generics-add-class! num (if (class? super) (class-num super) num))
	    class))))

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
		    (vector-set! vec i (vector-ref ovec i))
		    (loop (+fx i 1))))))))
	 
;*---------------------------------------------------------------------*/
;*    generic-added? ...                                               */
;*---------------------------------------------------------------------*/
(define (generic-added? generic::procedure)
   (vector? (generic-method-array generic)))

;*---------------------------------------------------------------------*/
;*    make-method-array ...                                            */
;*---------------------------------------------------------------------*/
(define (make-method-array def-bucket::vector)
   (let ((s (+fx 1 (quotientfx *nb-classes-max* (bigloo-generic-bucket-size)))))
      (make-vector s def-bucket)))

;*---------------------------------------------------------------------*/
;*    generic-no-default-behavior ...                                  */
;*---------------------------------------------------------------------*/
(define (generic-no-default-behavior . l)
   (error 'generic "No default behavior" l))

;*---------------------------------------------------------------------*/
;*    procedure->generic ...                                           */
;*---------------------------------------------------------------------*/
(define (procedure->generic proc)
   ($make-generic proc))

;*---------------------------------------------------------------------*/
;*    add-generic! ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-generic! generic default)
   (with-lock $bigloo-generic-mutex
      (lambda ()
	 (add-generic-sans-lock! generic default))))

;*---------------------------------------------------------------------*/
;*    add-generic-sans-lock! ...                                       */
;*    -------------------------------------------------------------    */
;*    Adding a generic could be a two steps process. It may happens,   */
;*    because of graph in the module graph, that we see the first      */
;*    method before the generic itself. It such a situation, we        */
;*    declare a dummy generic with a default body that is an error.    */
;*    Then, in the second time, we will have to fix the default body   */
;*    for that generic.                                                */
;*---------------------------------------------------------------------*/
(define (add-generic-sans-lock! generic default)
   (if (not (generic-added? generic))
       (let* ((def-met (if (procedure? default)
			   default
			   generic-no-default-behavior))
	      (def-bucket (make-vector (bigloo-generic-bucket-size) def-met)))
	  (if (=fx *nb-generics* *nb-generics-max*)
	      (double-nb-generics!))
	  (vector-set! *generics* *nb-generics* generic)
	  (set! *nb-generics* (+fx *nb-generics* 1))
	  (generic-default-set! generic def-met)
	  (generic-default-bucket-set! generic def-bucket)
	  (generic-method-array-set! generic (make-method-array def-bucket))
	  #unspecified)
       (begin
	  (if (procedure? default)
	      ;; We have to adjust the generic default bucket and the
	      ;; generic method array. We have to plug the new default
	      ;; function
	      (let ((old-def-bucket (generic-default-bucket generic))
		    (new-def-bucket (make-vector (bigloo-generic-bucket-size)
						 default))
		    (old-default (generic-default generic)))
		 (generic-default-set! generic default)
		 (generic-default-bucket-set! generic new-def-bucket)
		 (let* ((marray (generic-method-array generic))
			(alen (vector-length marray)))
		    (let loop ((i 0))
		       (if (<fx i alen)
			   (let ((bucket (vector-ref marray i)))
			      (if (eq? bucket old-def-bucket)
				  (begin
				     (vector-set! marray i new-def-bucket)
				     (loop (+fx i 1)))
				  (let laap ((j 0))
				     (cond
					((=fx j (bigloo-generic-bucket-size))
					 (loop (+fx i 1)))
					((eq? (vector-ref bucket j)
					      old-default)
					 (vector-set! bucket j default)
					 (laap (+fx j 1)))
					(else
					 (laap (+fx j 1))))))))))))
	  #unspecified)))

;*---------------------------------------------------------------------*/
;*    %add-method! ...                                                 */
;*---------------------------------------------------------------------*/
(define (%add-method! generic class method)
   (with-lock $bigloo-generic-mutex
      (lambda ()
	 (if (not (generic-added? generic))
	     ;; we check the installation of the generic in order to
	     ;; allow cycle in module graph.
	     (add-generic-sans-lock! generic #f))
	 (let* ((method-array (generic-method-array generic))
		(cnum (class-num class))
		(previous (method-array-ref generic method-array cnum))
		(def (generic-default generic)))
	    (let loop ((class class))
	       (let* ((cn (class-num class))
		      (current (method-array-ref generic method-array cn)))
		  (if (or (eq? current def) (eq? current previous))
		      (begin
			 ;; we add the method
			 (method-array-set! generic method-array cn method)
			 ;; and we recursivly iterate on subclasses
			 (for-each loop (class-subclasses class)))))))
	 method)))

;*---------------------------------------------------------------------*/
;*    add-method! ...                                                  */
;*---------------------------------------------------------------------*/
(define (add-method! generic class method)
   (cond
      ((not (class? class))
       (error 'add-method! "Illegal class" class))
      ((not (=fx (procedure-arity generic) (procedure-arity method)))
       (error 'add-method! "arity mismatch" (cons generic method)))
      (else
       (%add-method! generic class method))))

;*---------------------------------------------------------------------*/
;*    add-eval-method! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function is similar to ADD-METHOD! but the arity check      */
;*    differs. Since eval function of more than four arguments are     */
;*    implemented with -1-arity procedure, ADD-EVAL-METHOD must        */
;*    enforce a more permissive arity check.                           */
;*---------------------------------------------------------------------*/
(define (add-eval-method! generic class method)
   (cond
      ((not (class? class))
       (error 'add-eval-method! "Illegal class" class))
      ((and (not (=fx (procedure-arity generic) (procedure-arity method)))
	    (>fx (procedure-arity generic) 4)
	    (not (=fx (procedure-arity method) -1)))
       (error 'add-eval-method! "arity mismatch" (cons generic method)))
      (else
       (%add-method! generic class method))))

;*---------------------------------------------------------------------*/
;*    find-class-method ...                                            */
;*---------------------------------------------------------------------*/
(define (find-class-method class generic)
   (let ((obj-class-num (class-num class)))
      (method-array-ref generic (generic-method-array generic) obj-class-num)))

;*---------------------------------------------------------------------*/
;*    find-method ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function returns the exact method of a generic              */
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
;*    is-a? ...                                                        */
;*    -------------------------------------------------------------    */
;*    The constant time implementation of is-a?                        */
;*---------------------------------------------------------------------*/
(define (is-a? obj class)
   (if (object? obj)
       ;; it is an object, we check if OBJ inherits of CLASS
       (let ((oclass (object-class obj)))
	  (if (eq? oclass class)
	      #t
	      (let ((omin (class-min-num oclass))
		    (cmin (class-min-num class))
		    (cmax (class-max-num class)))
		 (if (>=fx omin cmin)
		     (<=fx omin cmax)
		     #f))))
       ;; not even a class instance
       #f))

;*---------------------------------------------------------------------*/
;*    is-nil? ...                                                      */
;*---------------------------------------------------------------------*/
(define (is-nil? obj::object)
   (let ((klass (object-class obj)))
      (eq? ((class-nil klass)) obj)))

;*---------------------------------------------------------------------*/
;*    class-hierarchy-numbering! ...                                   */
;*    -------------------------------------------------------------    */
;*    We tried to avoid as much as possible complete tree traversal.   */
;*---------------------------------------------------------------------*/
(define (class-hierarchy-numbering! class super)
   (let* ((super-min (class-min-num super))
	  (super-max (class-max-num super))
	  (subclasses (class-subclasses super))
	  (new-num (if (null? (cdr subclasses))
		       (+fx 1 (class-min-num super))
		       (+fx 1 (class-max-num (cadr subclasses))))))
      ;; test the validity of the class number
      (when (<fx new-num 0)
	 (error 'class-hierarchy-numbering
		"Cannot allocate fresh positive number for class"
		(class-name class)))
      ;; we set the class type
      (class-min-num-set! class new-num)
      (class-max-num-set! class new-num)
      (when (>fx new-num super-max)
	 ;; we have to re-number a part of the tree
	 (class-hierarchy-up-renumber! super))))

;*---------------------------------------------------------------------*/
;*    class-hierarchy-up-renumber ...                                  */
;*---------------------------------------------------------------------*/
(define (class-hierarchy-up-renumber! class)
   ;; we increment the max number for the current class
   (let* ((old-num (class-max-num class))
	  (new-num (cond
		      ((>fx old-num (/fx (maxvalfx) 2))
		       (+fx 10 old-num))
		      ((>fx old-num (/fx (maxvalfx) 4))
		       (+fx 100 old-num))
		      ((<fx (-fx old-num (class-min-num class)) 512)
		       (+fx old-num 1024))
		      (else
		       (*fx old-num 2))))
	  (super (class-super class)))
      (when (<fx new-num 0)
	 (error 'class-hierarchy-up-renumber!
		"Cannot renumber class hierarchy"
		(class-name class)))
      (class-max-num-set! class new-num)
      ;; we propagate this to its super classes and sisters
      (if (class? super)
	  (let loop ((sisters (class-subclasses super))
		     (old-sisters '()))
	     ;; we have to collect its older sisters
	     (cond
		((or (null? sisters) (eq? (car sisters) class))
		 ;; we have to renumber its older sisters
		 (let liip ((old-sisters old-sisters)
			    (sister-num (+fx new-num 1)))
		    (if (null? old-sisters)
			;; we are done, we loop using the super class, only
			;; if we have introduced a new unbound super class
			;; max-number
			(if (>fx (class-max-num (car (class-subclasses super)))
				 (class-max-num super))
			    (class-hierarchy-up-renumber! super))
			(liip (cdr old-sisters)
			      (class-hierarchy-down-renumber! (car old-sisters)
							      sister-num)))))
		(else
		 (loop (cdr sisters)
		       (cons (car sisters) old-sisters))))))))

;*---------------------------------------------------------------------*/
;*    class-hierarchy-down-renumber! ...                               */
;*---------------------------------------------------------------------*/
(define (class-hierarchy-down-renumber! class num)
   ;; we set the min number for the class and we walk thru its children
   (class-min-num-set! class num)
   (let liip ((classes (reverse (class-subclasses class)))
	      (max-num num))
      (if (null? classes)
	  (begin
	     (class-max-num-set! class max-num)
	     (+fx 1 max-num))
	  (liip (cdr classes)
		(class-hierarchy-down-renumber! (car classes)
						(+fx 1 max-num))))))

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
;*    object->struct ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (object->struct::struct object::object)
   (error 'object->struct "This object can't be converted" object))

;*---------------------------------------------------------------------*/
;*    struct+object->object ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (struct+object->object::object object::object struct::struct)
   (error 'struct+object->object "This structure can't be converted" struct))

;*---------------------------------------------------------------------*/
;*    object-hashnumber ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (object-hashnumber::int object::object)
   (%object-hashnumber object))

;*---------------------------------------------------------------------*/
;*    struct->object ...                                               */
;*---------------------------------------------------------------------*/
(define (struct->object::object struct::struct)
   (struct+object->object (allocate-instance (struct-key struct)) struct))

;*---------------------------------------------------------------------*/
;*    allocate-instance ...                                            */
;*---------------------------------------------------------------------*/
(define (allocate-instance::object cname::symbol)
   (let loop ((i 0))
      (if (=fx i *nb-classes*)
	  (error 'allocate-instance "Can't find class" cname)
	  (let ((class (vector-ref-ur *classes* i)))
	     (if (eq? (class-name class) cname)
		 ((class-allocate class))
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
	 ;; we now print its specific fields
	 (if (not (class-field-indexed? field))
	     ;; this is not an indexed field
	     (begin
		(display #\space port)
		(print-slot (get-value obj) port)
		(display #\] port))
	     ;; this is an indexed field
	     (let* ((get-len (class-field-len-accessor field))
		    (len     (get-len obj)))
		(let loop ((i 0))
		   (if (=fx i len)
		       (display #\] port)
		       (begin
			  (display #\space port)
			  (print-slot (get-value obj i) port)
			  (loop (+fx i 1)))))))))
   (let* ((class (object-class obj))
	  (class-name (class-name class))
	  (fields (class-fields class)))
      (display "#|" port)
      (display class-name port)
      (if (is-nil? obj)
	  (display " nil|" port)
	  (if (class-fields? fields)
	      (let loop ((fields fields)
			 (class class))
		 (cond
		    ((null? fields)
		     (let ((super (class-super class)))
			(if (class? super)
			    ;; we have to print the super class fields
			    (loop (class-fields super) super)
			    (display #\| port))))
		    ((eq? fields #unspecified)
		     (display "..." port)
		     (loop '() class))
		    (else
		     (class-field-write/display (car fields))
		     (loop (cdr fields) class))))
	      (display #\| port)))))

;*---------------------------------------------------------------------*/
;*    object-equal? ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (object-equal?::bool obj1::object obj2::object)
   (define (class-field-equal? field)
      (let ((get-value (class-field-accessor field)))
	 (if (not (class-field-indexed? field))
	     ;; this is not an indexed field, some it is a simple check
	     (equal? (get-value obj1) (get-value obj2))
	     ;; this field is indexed, we have to check all its values
	     (let* ((get-len (class-field-len-accessor field))
		    (len1    (get-len obj1))
		    (len2    (get-len obj2)))
		(and (=fx len1 len2)
		     (let loop ((i 0))
			(cond
			   ((=fx i len1)
			    #t)
			   ((equal? (get-value obj1 i) (get-value obj2 i))
			    (loop (+fx i 1)))
			   (else
			    #f))))))))
   (let ((class1 (object-class obj1))
	 (class2 (object-class obj2)))
      (cond
	 ((not (eq? class1 class2))
	  #f)
	 (else
	  (let ((fields (class-fields class1)))
	     (if (not (class-fields? fields))
		 #f
		 (let loop ((fields fields)
			    (class  class1))
		    (cond
		       ((null? fields)
			(let ((super (class-super class)))
			   (if (class? super)
			       ;; we have now to check the super class fields
			       (let ((fields (class-fields super)))
				  (if (class-fields? fields)
				      (loop fields super)
				      #f))
			       ;; ok we are done with return value #t
			       #t)))
		       ((class-field-equal? (car fields))
			(loop (cdr fields) class))
		       (else
			#f)))))))))

;*---------------------------------------------------------------------*/
;*    call-virtual-getter ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is used by all the class generated getters in      */
;*    order to call the correct getter of an instance. This function   */
;*    applies the good function with respect to the dynamic instance's */
;*    class.                                                           */
;*---------------------------------------------------------------------*/
(define (call-virtual-getter obj::object num::int)
   (let* ((class  (object-class obj))
	  (getter (car (vector-ref (class-virtual class) num))))
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
   (let* ((class  (object-class obj))
	  (setter (cdr (vector-ref (class-virtual class) num))))
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
      ((car (vector-ref (class-virtual next-class) num)) obj)))

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
      ((cdr (vector-ref (class-virtual next-class) num)) obj value)))

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

