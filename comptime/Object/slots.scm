;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/slots.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 18 12:48:07 1996                          */
;*    Last change :  Tue Jan  4 08:13:40 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We build the class slots                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_slots
   
   (include "Tools/trace.sch")
   
   (import  tools_error
	    tools_location
	    tools_shape
	    type_type
	    type_cache
	    type_env
	    tvector_tvector
	    engine_param
	    object_class
	    ast_var
	    ast_ident
	    ast_glo-decl)
   
   (export  (class slot
	       (id::symbol read-only)
	       ;; the slot identifier
	       (name::bstring read-only)
	       ;; the source of that field (for reporting errors)
	       (src read-only)
	       ;; the class that slot belongs to
	       (class-owner read-only)
	       ;; the slot name
	       (type read-only)
	       ;; the slot type
	       (read-only?::bool read-only (default #f))
	       ;; the slot default value
	       (default-value read-only (default (slot-no-default-value-mark)))
	       ;; virtual slot number (> 0 if virtual)
	       (virtual-num (default -1))
	       ;; the virtual slot getter
	       (getter (default #f))
	       ;; the virtual slot setter
	       (setter (default #f))
	       ;; for an indexed slot, indexed holds a tvec structure
	       ;; that can be used to generate JVM accessors
	       (indexed read-only (default #f))
	       ;; some user information associated to that slot
	       (user-info read-only (default #unspecified)))
	    
	    (slot-no-default-value-mark)
	    (slot-default?::bool ::slot)
	    (slot-virtual?::bool ::slot)
	    (make-class-slots ::tclass ::obj ::obj ::int ::obj)
	    (make-java-class-slots ::jclass ::obj ::obj ::obj)
	    (get-local-virtual-slots-number ::tclass ::pair-nil)
	    (make-class-make-formals ::pair-nil)
	    (make-class-make-typed-formals ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    shape ::slot ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (shape s::slot)
   (with-access::slot s (id name class-owner type)
      (string-append "<" (symbol->string id) "(" name ")"
		     " owner=" (shape class-owner)
		     " type=" (shape type) ">")))

;*---------------------------------------------------------------------*/
;*    *slot-no-default-value-mark* ...                                 */
;*---------------------------------------------------------------------*/
(define *slot-no-default-value-mark*
   ;; this value can't be gensymed because it has to traverse libraries
   'slot-no-default-value__17_5_1996)

;*---------------------------------------------------------------------*/
;*    slot-no-default-value-mark ...                                   */
;*---------------------------------------------------------------------*/
(define (slot-no-default-value-mark)
   *slot-no-default-value-mark*)

;*---------------------------------------------------------------------*/
;*    slot-default? ...                                                */
;*---------------------------------------------------------------------*/
(define (slot-default? slot)
   (not (eq? (slot-default-value slot) (slot-no-default-value-mark))))

;*---------------------------------------------------------------------*/
;*    slot-virtual? ...                                                */
;*---------------------------------------------------------------------*/
(define (slot-virtual? slot)
   (>=fx (slot-virtual-num slot) 0))

;*---------------------------------------------------------------------*/
;*    ensure-type-defined! ...                                         */
;*---------------------------------------------------------------------*/
(define (ensure-type-defined! type::type src)
   (if (not (type-init? type))
       (user-error "Can't find type definition" (type-id type) src)))

;*---------------------------------------------------------------------*/
;*    make-class-slots ...                                             */
;*---------------------------------------------------------------------*/
(define (make-class-slots class slots super vnum src)
   
   (define (find-default-attr attr)
      ;; seek for the default attribute of slots
      (if (not (pair? attr))
	  (slot-no-default-value-mark)
	  (match-case (car attr)
	     ((default ?value)
	      value)
	     (else
	      (find-default-attr (cdr attr))))))
   
   (define (find-info-attr attr)
      ;; seek for the slot user info attribute
      (if (not (pair? attr))
	  #f
	  (match-case (car attr)
	     ((info ?-)
	      (car attr))
	     (else
	      (find-info-attr (cdr attr))))))
   
   (define (find-virtual-attr attr)
      ;; seek for the virtual getter and setter definitions
      (let loop ((attr attr)
		 (get  #f)
		 (set  #f))
	 (if (not (pair? attr))
	     (values get set)
	     (match-case (car attr)
		((get ?get)
		 (loop (cdr attr) get set))
		((set ?set)
		 (loop (cdr attr) get set))
		(else
		 (loop (cdr attr) get set))))))
   
   (define (slot-member? slot slot-list)
      ;; this function returns true if there is a slot with the same name
      ;; in the list
      (let ((id (slot-id slot)))
	 (let loop ((slot-list slot-list))
	    (cond
	       ((null? slot-list)
		#f)
	       ((eq? (slot-id (car slot-list)) id)
		(car slot-list))
	       (else
		(loop (cdr slot-list)))))))
   
   (define (unique-virtual-slots slots)
      ;; this function removes overriden duplicated virtual slots
      (let loop ((slots slots)
		 (res   '()))
	 (cond
	    ((null? slots)
	     res)
	    ((null? (car slots))
	     ;; this is an error, skip because the compilation will be
	     ;; stopped soon
	     (loop (cdr slots) '()))
	    ((slot-virtual? (car slots))
	     (let ((other (slot-member? (car slots) res)))
		(if (slot? other)
		    (let ((loc (find-location src))
			  (id  (slot-id (car slots))))
		       (if (eq? (slot-class-owner (car slots))
				(slot-class-owner other))
			   (user-error/location
			    loc
			    (type-id class)
			    "Illegal duplicated virtual slot"
			    id)
			   (let ((new-num (slot-virtual-num (car slots))))
			      ;; we overriding a virtual slot we have to
			      ;; ajust the slot numbering
			      (slot-virtual-num-set! other new-num)
			      ;; ...and we emit a warning
			      (when (and (>= (bigloo-warning) 2)
					 *warning-overriden-slots*)
				 (user-warning/location
				  loc
				  (type-id class)
				  (format "Overriden virtual slot of class \"~a\"" (type-id (slot-class-owner (car slots))))
				  id))
			      (loop (cdr slots) res))))
		    (loop (cdr slots) (cons (car slots) res)))))
	    (else
	     (loop (cdr slots) (cons (car slots) res))))))
   
   (define (make-indexed-slots s slot-id attr)
      ;; indexed slot.
      (multiple-value-bind (vget vset)
	 (find-virtual-attr attr)
	 (if (or vget vset)
	     (user-error/location (type-id class)
				  "virtual slot can't be indexed"
				  (car slot-id)
				  '())))
      (let* ((ftype (find-slot-type slot-id src))
	     (iid (gensym))
	     (itype (declare-subtype! iid
				      (symbol->string iid)
				      (list 'obj)
				      'bigloo))
	     (sval (instantiate::slot
		      (id (car slot-id))
		      (name (scheme-symbol->c-string (car slot-id)))
		      (class-owner class)
		      (src s)
		      (type ftype)
		      (read-only? (memq 'read-only attr))
		      (default-value (find-default-attr attr))
		      (indexed (widen!::tvec itype
				  (item-type ftype)))
		      (user-info (find-info-attr attr))))
	     (slen (let ((len-id (symbol-append (car slot-id) '-len)))
		      (instantiate::slot
			 (id len-id)
			 (name (scheme-symbol->c-string len-id))
			 (type *long*)
			 (src s)
			 (class-owner class)
			 (read-only? #t)))))
	 (type-init?-set! itype #t)
	 (values sval slen)))
   
   (define (make-attribute-slot s slot-id attr vget vset vnum)
      ;; direct slot with attribute. Because of the presence of
      ;; the attributes, this slot may be virtual
      (let ((reado? (memq 'read-only attr)))
	 (cond
	    ((and vset (not vget))
	     (user-error/location (find-location s)
				  (type-id class)
				  "Illegal virtual slot (missing getter)"
				  (car slot-id)
				  '()))
	    ((and vset reado?)
	     (user-error/location (find-location s)
				  (type-id class)
				  "Illegal virtual slot (read-only)"
				  (car slot-id)
				  '()))
	    ((and vget (not vset) (not reado?))
	     (user-error/location (find-location s)
				  (type-id class)
				  "Illegal virtual slot (missing setter)"
				  (car slot-id)
				  '()))
	    (else
	     (instantiate::slot
		(id (car slot-id))
		(name (scheme-symbol->c-string (car slot-id)))
		(src s)
		(class-owner class)
		(type (find-slot-type slot-id s))
		(read-only? reado?)
		(default-value (find-default-attr attr))
		(virtual-num (if vget vnum -1))
		(getter vget)
		(setter vset)
		(user-info (find-info-attr attr)))))))
   
   (define (check-super-slot nslot sslots class)
      (unless (slot-virtual? nslot)
	 (let ((id (slot-id nslot)))
	    (when (any? (lambda (ss) (eq? (slot-id ss) id)) sslots)
	       (user-error/location (find-location (slot-src nslot))
				    (tclass-id class)
				    "slot already defined in super class"
				    id)))))

   (define (check-super-slots nslots sslots class)
      (for-each (lambda (slot)
		   (check-super-slot slot sslots class))
		nslots))
   
   (define (make-direct-slot slot-id)
      ;; plain direct slot
      (instantiate::slot
	 (id (car slot-id))
	 (name (scheme-symbol->c-string (car slot-id)))
	 (src slot-id)
	 (class-owner class)
	 (type (find-slot-type slot-id src))))
   
   (trace (ast 2) "make-class-slots: " slots " " vnum #\Newline)
   (let loop ((slots slots)
	      (nslots '())
	      (vnum  vnum))
      (if (null? slots)
	  (let ((sslots (cond
			   ((not (type? super))
			    '())
			   ((not (tclass? super))
			    '())
			   (else
			    (tclass-slots super)))))
	     ;; check that this class does not re-define a super class slot
	     (check-super-slots nslots sslots class)
	     ;; this is mandatory that unique-virtual-slots is called
	     ;; on a reversed list. This function fixes up virtual slots
	     ;; number. If the list is the wrong direction, numbers will get
	     ;; mixed up.
	     (unique-virtual-slots (append nslots (reverse sslots))))
	  (let ((s (car slots)))
	     (match-case s
		((* (id ?id) . ?attr)
		 (multiple-value-bind (sval slen)
		    (make-indexed-slots s id attr)
		    (loop (cdr slots)
			  (cons* sval slen nslots)
			  vnum)))
		(((id ?id) . ?attr)
		 (multiple-value-bind (vget vset)
		    (find-virtual-attr attr)
		    (loop (cdr slots)
			  (cons (make-attribute-slot s id attr vget vset vnum)
				nslots)
			  (if (or vget vset) (+ vnum 1) vnum))))
		((id ?id)
		 (loop (cdr slots)
		       (cons (make-direct-slot id) nslots)
		       vnum))
		(else
		 (user-error/location (find-location s)
				      (tclass-id class)
				      "Illegal slot"
				      s)))))))

;*---------------------------------------------------------------------*/
;*    make-java-class-slots ...                                        */
;*---------------------------------------------------------------------*/
(define (make-java-class-slots class slots super src)
   (trace (ast 2) "make-java-class-slots: " slots " " 0 #\Newline)
   (let ((loc (find-location src)))
      (define (make-java-class-slot ident jname reado? s)
	 (let* ((loc (find-location/loc s loc))
		(slot-id (parse-id ident loc)))
	    (instantiate::slot
	       (id (car slot-id))
	       (name jname)
	       (type (find-slot-type slot-id src))
	       (read-only? reado?)
	       (class-owner class)
	       (src s))))
      (define (declare-static-field! id jname reado? src)
	 (let* ((loc (find-location/loc src loc))
		(slot-id (parse-id id loc))
		(l-id (if (eq? (jclass-id class) 'foreign)
			  (car slot-id)
			  (symbol-append (jclass-id class) '- (car slot-id))))
		(t-id (if (eq? (cdr slot-id) *_*)
			  *obj*
			  (cdr slot-id)))
		(qc-id jname))
	    (ensure-type-defined! t-id src)
	    (let ((g (declare-global-cvar! l-id qc-id
					   (type-id t-id) reado? src #f)))
	       (global-module-set! g (jclass-id class))
	       (global-jvm-type-name-set! g (jclass-name class))
	       g)))
      (let loop ((slots slots)
		 (res   (cond
			   ((not (type? super))
			    '())
			   ((not (jclass? super))
			    '())
			   (else
			    (reverse (jclass-slots super))))))
	 (if (null? slots)
	     ;; this is mandatory that unique-virtual-slots is called
	     ;; on a reversed list. This function fixs up virtual slots
	     ;; number. If the list is the wrong direction, numbers will get
	     ;; mixed up.
	     res
	     (let* ((s (car slots))
		    (src (car s))
		    (id (cadr s))
		    (jname (caddr s))
		    (mod (cadddr s))
		    (reado? (memq 'final mod)))
		(if (memq 'static mod)
		    (begin
		       (declare-static-field! id jname reado? src)
		       (loop (cdr slots) res))
		    (loop (cdr slots)
			  (cons (make-java-class-slot id jname reado? src)
				res))))))))

;*---------------------------------------------------------------------*/
;*    find-slot-type ...                                               */
;*---------------------------------------------------------------------*/
(define (find-slot-type slot-id src)
   (let ((t (if (eq? (cdr slot-id) *_*) *obj* (cdr slot-id))))
      (ensure-type-defined! t src)
      t))

;*---------------------------------------------------------------------*/
;*    scheme-symbol->c-string ...                                      */
;*---------------------------------------------------------------------*/
(define (scheme-symbol->c-string symbol)
   (if *ast-case-sensitive*
       (id->name symbol)
       (string-downcase (id->name symbol))))

;*---------------------------------------------------------------------*/
;*    get-local-virtual-slots-number ...                               */
;*    -------------------------------------------------------------    */
;*    This function takes a list of slots and a class and it computes  */
;*    how many virtual slots are created by this class.                */
;*    -------------------------------------------------------------    */
;*    The number of virtual slots is just the maximun virtual number   */
;*    found (+ 1). It is not the number of virtual slots because of    */
;*    inheritance mecanism. That is if a class override a virtual slot */
;*    definition. That slot does not count as a new entire virtual     */
;*    slot.                                                            */
;*---------------------------------------------------------------------*/
(define (get-local-virtual-slots-number class::tclass slots)
   (let loop ((slots slots)
	      (num   -1))
      (cond
	 ((null? slots)
	  (+fx num 1))
	 ((slot-virtual? (car slots))
	  (let ((lnum (slot-virtual-num (car slots))))
	     (loop (cdr slots)
		   (if (>fx lnum num)
		       lnum
		       num))))
	 (else
	  (loop (cdr slots) num)))))

;*---------------------------------------------------------------------*/
;*    make-class-make-formals ...                                      */
;*---------------------------------------------------------------------*/
(define (make-class-make-formals slots)
   (let loop ((slots   slots)
	      (formals '()))
      (cond
	 ((null? slots)
	  (reverse! formals))
	 ((slot-virtual? (car slots))
	  (loop (cdr slots) formals))
	 (else
	  (loop (cdr slots)
		(cons (mark-symbol-non-user! (gensym (slot-id (car slots))))
		      formals))))))
	  
;*---------------------------------------------------------------------*/
;*    make-class-make-typed-formals ...                                */
;*---------------------------------------------------------------------*/
(define (make-class-make-typed-formals ids slots)
   (let loop ((slots   slots)
	      (ids     ids)
	      (formals '()))
      (cond
	 ((null? slots)
	  (reverse! formals))
	 ((slot-virtual? (car slots))
	  (loop (cdr slots) ids formals))
	 (else
	  (loop (cdr slots)
		(cdr ids)
		(cons (make-typed-ident (mark-symbol-non-user! (car ids))
					(type-id (slot-type (car slots))))
		      formals))))))
	  
      
