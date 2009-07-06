;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/decl.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 29 14:38:38 1995                          */
;*    Last change :  Sat Nov 20 14:20:12 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The declaration handling                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_decl
   (include "Parser/coord.sch"
	    "Translate/ast.sch"
	    "Translate/type.sch")
   (import  translate_function
	    translate_type
	    translate_tspec
	    engine_param
	    tools_speek)
   (export  (get-decl-ident                  <decl>)
	    (get-decl-para-decl              <decl>)
	    (storage-class-spec-of-decl-spec <decl-spec>)
	    (correct-storage-class-spec?     <sspec>)
	    (type-spec-of-decl-spec          <decl-spec>)
	    (type+decl->type                 <type> <decl>)
	    (type+adecl->type                <type> <adecl>)))

;*---------------------------------------------------------------------*/
;*    get-decl-ident ...                                               */
;*---------------------------------------------------------------------*/
(define (get-decl-ident decl)
   (define (get-decl2-ident decl2)
      (cond
	 ((decl2-id decl2)
	  (decl2-id decl2))
	 ((decl2-a-decl decl2)
	  (get-decl-ident (decl2-a-decl decl2)))
	 ((decl2-a-decl2 decl2)
	  (get-decl2-ident (decl2-a-decl2 decl2)))
	 (else
	  (error "internal-error" "get-decl2-ident" decl2))))
   (get-decl2-ident (decl-a-decl2 decl)))

;*---------------------------------------------------------------------*/
;*    get-decl-para-decl ...                                           */
;*---------------------------------------------------------------------*/
(define (get-decl-para-decl decl)
   (verbose 3 "get-decl-para-decl: " decl #\Newline)
   (let loop ((decl  decl)
	      (pdecl #f))
      (verbose 3
	       "   get-decl-para-decl[loop]: " decl #\Newline
	       "                     pdecl : " pdecl #\Newline)
      (match-case decl
	 (#{decl ?- ?- ?- ?decl2}
	  (if (not decl2)
	      pdecl
	      (match-case decl2
		 (#{decl2 ?- ?- #f #f ?decl2 #f ?ptl ?pil}
		  (cond
		     ((not decl2)
		      pdecl)
		     (else
		      (let ((pdecl (cond
				      (ptl
				       (list 'parameter-type-list ptl))
				      (pil
				       (list 'parameter-identifier-list pil))
				      (else
				       pdecl))))
			 (match-case decl2
			    (#{decl2 ?- ?- ?- #f #f #f #f #f}
			     pdecl)
			    (#{decl2 ?- ?- #f ?decl #f #f #f #f}
			     (if (not decl)
				 pdecl
				 (loop decl pdecl)))
			    (else
			     pdecl))))))
		 (else
		  pdecl))))
	 (else
	  pdecl))))
   
;*---------------------------------------------------------------------*/
;*    storage-class-spec-of-decl-spec ...                              */
;*    -------------------------------------------------------------    */
;*    Return all the storage class specifier from a declaration        */
;*    specifier.                                                       */
;*---------------------------------------------------------------------*/
(define (storage-class-spec-of-decl-spec decl-spec)
   (let loop ((dspec decl-spec)
	      (sspec '()))
      (cond
	 ((null? dspec)
	  sspec)
	 ((storage-class-spec? (car dspec))
	  (loop (cdr dspec) (cons (car dspec) sspec)))
	 (else
	  (loop (cdr dspec) sspec)))))
	  
;*---------------------------------------------------------------------*/
;*    correct-storage-class-spec? ...                                  */
;*---------------------------------------------------------------------*/
(define (correct-storage-class-spec? storage-spec)
   (or (null? storage-spec)
       (and (pair? storage-spec) (null? (cdr storage-spec)))))

;*---------------------------------------------------------------------*/
;*    type-spec-of-decl-spec ...                                       */
;*    -------------------------------------------------------------    */
;*    Return all the storage class specifier from a declaration        */
;*    specifier.                                                       */
;*---------------------------------------------------------------------*/
(define (type-spec-of-decl-spec decl-spec)
   (let loop ((dspec decl-spec)
	      (tspec '()))
      (cond
	 ((null? dspec)
	  (reverse! tspec))
	 ((type-spec? (car dspec))
	  (loop (cdr dspec) (cons (car dspec) tspec)))
	 (else
	  (loop (cdr dspec) tspec)))))
	  
;*---------------------------------------------------------------------*/
;*    type+decl->type ...                                              */
;*---------------------------------------------------------------------*/
(define (type+decl->type type decl)
   (if (decl-a-ptr decl)
       (type+decl2->type (type+ptr->type type (decl-a-ptr decl))
			 (decl-a-decl2 decl))
       (type+decl2->type type
			 (decl-a-decl2 decl))))

;*---------------------------------------------------------------------*/
;*    type+ptr->type ...                                               */
;*---------------------------------------------------------------------*/
(define (type+ptr->type type ptr)
   (let ((type-spec-list (ptr-type-spec-list ptr))
	 (pointer        (ptr-pointer ptr)))
      (cond
	 ((and (not type-spec-list) (not pointer))
	  (make-ptr-to type))
	 ((not type-spec-list)
	  (make-ptr-to (type+ptr->type type pointer)))
	 ((not pointer)
	  (make-ptr-to (tspec->type type-spec-list)))
	 (else
	  (make-ptr-to (type+tspec-coumpound->type
			(type+ptr->type type pointer)
			type-spec-list))))))

;*---------------------------------------------------------------------*/
;*    type+decl2->type ...                                             */
;*---------------------------------------------------------------------*/
(define (type+decl2->type type decl2 )
   (verbose 2
	    "type+decl2->type: " #\Newline
	    "     type : " type #\Newline
	    "     decl2: " decl2 #\Newline)
   (cond
      ((decl2-a-decl decl2)
       (type+decl->type type (decl2-a-decl decl2)))
      ((decl2-array decl2)
       (let ((type (make-array type (decl2-array decl2))))
	  (type+decl2->type type (decl2-a-decl2 decl2) )))
      ((decl2-parameter-type-list decl2)
       (detect-struct-or-union type)
       (let ((para (parameter-type-list->types
		    (decl2-parameter-type-list decl2))))
	  (type+decl2->type (make-function type para)
			    (decl2-a-decl2 decl2))))
      (else
       type)))
    
;*---------------------------------------------------------------------*/
;*    type+adecl->type ...                                             */
;*---------------------------------------------------------------------*/
(define (type+adecl->type type adecl)
   (if (not (adecl? adecl))
       type
       (let ((type (if (adecl-a-ptr adecl)
		       (type+ptr->type type (adecl-a-ptr adecl))
		       type)))
	  (type+adecl2->type type (adecl-a-adecl2 adecl)))))

;*---------------------------------------------------------------------*/
;*    type+adecl2->type ...                                            */
;*---------------------------------------------------------------------*/
(define (type+adecl2->type type adecl2)
   (cond
      ((not (adecl2? adecl2))
       type)
      ((adecl2-a-adecl adecl2)
       (type+adecl->type type (adecl2-a-adecl adecl2)))
      ((adecl2-array adecl2)
       (let ((type (if (null? (adecl2-array adecl2))
		       (make-ptr-to type)
		       (make-array type (adecl2-array adecl2)))))
	  (type+adecl2->type type (adecl2-a-adecl2 adecl2))))
      ((adecl2-parameter-type-list adecl2)
       (detect-struct-or-union type)
       (let ((para (parameter-type-list->types 
		    (adecl2-parameter-type-list adecl2))))
	  (type+adecl2->type (make-function type para)
			     (adecl2-a-adecl2 adecl2))))
      (else
       type)))
