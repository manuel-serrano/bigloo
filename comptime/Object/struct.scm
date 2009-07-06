;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/struct.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 11:52:53 1996                          */
;*    Last change :  Thu May 12 09:22:37 2005 (serrano)                */
;*    Copyright   :  1996-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The object<->struct conversion                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_struct
   (import  tools_error
	    tools_misc
	    type_type
	    type_env
	    type_cache
	    type_tools
	    ast_var
	    ast_env
	    ast_ident
	    tools_error
	    engine_param
	    backend_backend
	    object_class
	    object_tools
	    object_slots
	    ast_private)
   (export  (gen-plain-class<->struct ::type ::pair)
	    (gen-wide-class<->struct ::type ::pair)))

;*---------------------------------------------------------------------*/
;*    gen-plain-class<->struct ...                                     */
;*---------------------------------------------------------------------*/
(define (gen-plain-class<->struct class src-def)
   (with-access::tclass class (id slots abstract?)
      (if abstract?
	  '()
	  (list (gen-plain-class->struct id class slots src-def)
		(gen-struct->plain-class id class slots src-def)))))
   
;*---------------------------------------------------------------------*/
;*    gen-wide-class<->struct ...                                      */
;*---------------------------------------------------------------------*/
(define (gen-wide-class<->struct class src-def)
   (with-access::tclass class (id slots)
      (list (gen-wide-class->struct id class slots src-def)
	    (gen-struct->wide-class id class slots src-def))))
   
;*---------------------------------------------------------------------*/
;*    save-slot ...                                                    */
;*---------------------------------------------------------------------*/
(define (save-slot oname sname cname i slot)
   (define (save-immediat-slot)
      `(struct-set! ,sname
		    ,i
		    (,(symbol-append cname '- (slot-id slot))
		     ,oname)))
   (define (save-indexed-slot)
      (let ((vec  (gensym 'vec))
	    (j    (gensym 'j))
	    (loop (gensym 'loop))
	    (len  (gensym 'len)))
	 `(let ((,len (,(symbol-append cname '- (slot-id slot) '-len) ,oname)))
	     (let ((,vec (make-vector ,len)))
		(labels ((,loop (,j) (if (=fx ,j ,len)
					 (struct-set! ,sname ,i ,vec)
					 (begin
					    (vector-set-ur!
					     ,vec
					     ,j
					     (,(symbol-append cname
							      '-
							      (slot-id slot)
							      '-ref)
					      ,oname
					      ,j))
					    (,loop (+fx ,j 1))))))
		   (,loop 0))))))
   (cond
      ((slot-virtual? slot)
       #unspecified)
      ((slot-indexed slot)
       (save-indexed-slot))
      (else
       (save-immediat-slot))))

;*---------------------------------------------------------------------*/
;*    slots-length ...                                                 */
;*---------------------------------------------------------------------*/
(define (slots-length slots)
   (let loop ((slots slots)
	      (len   0))
      (cond
	 ((null? slots)
	  len)
	 ((slot-virtual? (car slots))
	  (loop (cdr slots) len))
	 (else
	  (loop (cdr slots) (+fx 1 len))))))

;*---------------------------------------------------------------------*/
;*    gen-plain-class->struct ...                                      */
;*---------------------------------------------------------------------*/
(define (gen-plain-class->struct cname type slots src-def)
   (let ((len   (+fx 1 (slots-length slots)))
	 (oname (gensym 'obj))
	 (sname (gensym 'res)))
      (epairify
       `(define-method (object->struct::struct ,(make-typed-ident oname cname))
	   (let ((,sname (make-struct ',cname ,len #unspecified)))
	      (begin
		 (struct-set! ,sname 0 #f)
		 ,@(let loop ((i     1)
			      (slots slots)
			      (res   '()))
		      (cond
			 ((=fx i len)
			  (reverse! res))
			 ((slot-virtual? (car slots))
			  (loop (+fx i 1)
				(cdr slots)
				res))
			 (else
			  (loop (+fx i 1)
				(cdr slots)
				(cons (save-slot oname
						 sname
						 cname
						 i
						 (car slots))
				      res)))))
		 ,sname)))
       src-def)))

;*---------------------------------------------------------------------*/
;*    gen-wide-class->struct ...                                       */
;*---------------------------------------------------------------------*/
(define (gen-wide-class->struct cname type slots src-def)
   (let* ((len    (slots-length slots))
	  (oname  (gensym 'obj))
	  (res    (gensym 'res))
	  (tres   (symbol-append res '::struct))
	  (aux    (gensym 'aux)))
      (epairify
       `(define-method (object->struct::struct ,(make-typed-ident oname cname))
	   (let ((,tres (call-next-method)))
	      (let ((,aux (make-struct ',cname ,len #unspecified)))
		 ,@(let loop ((i     0)
			      (slots slots)
			      (res   '()))
		      (cond
			 ((=fx i len)
			  (reverse! res))
			 ((slot-virtual? (car slots))
			  (loop (+fx i 1)
				(cdr slots)
				res))
			 (else
			  (loop (+fx i 1)
				(cdr slots)
				(cons (save-slot oname aux cname i (car slots))
				      res)))))
		 (struct-set! ,res 0 ,aux)
		 ;; we now swap the structures' keys
		 (struct-key-set! ,aux (struct-key ,res))
		 (struct-key-set! ,res ',cname)
		 ;; and we return res
		 ,res)))
       src-def)))

;*---------------------------------------------------------------------*/
;*    restore-slot ...                                                 */
;*---------------------------------------------------------------------*/
(define (restore-slot oname sname cname type i slot)
   (let* ((loop          (gensym 'loop))
	  (runner        (gensym 'i))
	  (v             (gensym 'v))
	  (len           (gensym 'len))
	  (runner-typed  (symbol-append runner '::long))) 
      (cond
	 ((slot-virtual? slot)
	  #unspecified)
	 ((slot-indexed slot)
	  ;; I'm lazy I'm note sure it is important to support -gbdb2 and
	  ;; dynamically indexed field (anyhow, I have never been using
	  ;; dynamically indexed slots). The problem is that -gbdb2 impose
	  ;; to every objects to be tagged. Currently dynamic slot chunks
	  ;; are untagged.
	  ;; for an indexed field we have to make a
	  ;; malloc call and to fill all the field slots
	  (let* ((f (gensym 'f))
		 (tf (make-typed-ident f (type-id (slot-type slot)))))
	     `(let ((,v (struct-ref s ,i)))
		 (let ((,(symbol-append len '::long) (vector-length ,v)))
		    ,(make-pragma-indexed-init-set! type slot 'o len)
		    ;; this loop fill the field slots
		    (labels ((,loop (,runner-typed)
				    (if (=fx ,runner ,len)
					'done
					(let ((,tf (vector-ref-ur ,v ,runner)))
					   ,(make-pragma-indexed-set!
					     type
					     slot
					     'o
					     f
					     runner)
					   (,loop (+fx ,runner 1))))))
		       (,loop 0))))))
	 (else
	  `(let ((,(make-typed-ident v (type-id (slot-type slot)))
		  (struct-ref s ,i)))
	      ,(make-pragma-direct-set! type slot 'o v))))))

;*---------------------------------------------------------------------*/
;*    gen-struct->plain-class ...                                      */
;*    -------------------------------------------------------------    */
;*    This function fills `object' and returns it.                     */
;*    -------------------------------------------------------------    */
;*    This function is very similar to the make-object function        */
;*    execpted that it founds the value in the structure rather than   */
;*    being provided as actual values.                                 */
;*---------------------------------------------------------------------*/
(define (gen-struct->plain-class cname type slots src-def)
   (epairify
    `(define-method (struct+object->object::object ,(make-typed-ident 'o cname)
						   s::struct)
	(begin
	   (object-widening-set! o (struct-ref s 0))
	   ,@(let loop ((i     1)
			(slots slots)
			(res   '()))
		(cond
		   ((null? slots)
		    (reverse! res))
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
			  (cdr slots)
			  res))
		   (else
		    (let ((new (restore-slot 'o 's cname type i (car slots))))
		       (loop (+fx i 1)
			     (cdr slots)
			     (cons new res))))))
	   o))
    src-def))

;*---------------------------------------------------------------------*/
;*    gen-struct->wide-class ...                                       */
;*---------------------------------------------------------------------*/
(define (gen-struct->wide-class cname type slots src-def)
   (let* ((old       (gensym 'old))
	  (len       (slots-length slots))
	  (aux       (gensym 'aux))
	  (taux      (symbol-append aux '::struct))
	  (new       (gensym 'new))
	  (tid       (type-id type))
	  (holder    (tclass-holder type))
	  (widening  (symbol-append (tclass-widening type) '- tid)))
      (define (cast type expr)
	 (if (backend-pragma-support (the-backend))
	     `(,(make-typed-ident 'pragma (type-id type))
	       ,(string-append "((" (type-name type) ")($1))")
	       ,expr)
	     (make-private-sexp 'cast (type-id type) expr)))
      (epairify
       `(define-method (struct+object->object::object
			,(make-typed-ident 'o cname)
			s::struct)
	   (let ((,old  (call-next-method))
		 (,taux (struct-ref s 0)))
	      (let ((,(make-typed-ident new tid)
		     ,(cast type old)))
		 (object-class-num-set! ,new
					(class-num
					 (@ ,(global-id holder)
					    ,(global-module holder))))
		 (object-widening-set!
		  ,new
		  (,widening ,@(let loop ((i     0)
					  (slots slots)
					  (ref*  '()))
				  (cond
				     ((=fx i len)
				      (reverse! ref*))
				     ((slot-virtual? (car slots))
				      (loop (+fx i 1)
					    (cdr slots)
					    ref*))
				     (else
				      (loop (+fx i 1)
					    (cdr slots)
					    (cons `(struct-ref ,aux ,i)
						  ref*)))))))
		 ,new)))
       src-def)))

	  
	  
