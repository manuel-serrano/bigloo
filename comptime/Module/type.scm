;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/type.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 10:05:27 1996                          */
;*    Last change :  Fri Nov 18 07:10:35 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The type clauses compilation.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_type
   (include "Ast/unit.sch"
	    "Tools/trace.sch")
   (import  module_module
	    type_type
	    type_env
	    type_coercion
	    tvector_tvector
	    tvector_access
	    tools_error
	    tools_shape
	    (find-location tools_location))
   (export  (make-type-compiler)
	    (tvector-finalizer)
	    (module-tvector-clause ::symbol ::symbol ::obj ::obj)
	    (delay-tvector! tv clause ::bool)))

;*---------------------------------------------------------------------*/
;*    make-type-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-type-compiler)
   (instantiate::ccomp
      (id 'type)
      (producer (lambda (c) (type-producer/consumer #f c)))
      (consumer type-producer/consumer)
      (finalizer type-finalizer)))

;*---------------------------------------------------------------------*/
;*    type-producer/consumer ...                                       */
;*---------------------------------------------------------------------*/
(define (type-producer/consumer import clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (proto) (type-parser import proto clause)) protos)
       '())
      (else
       (user-error "Parse error" "Illegal `type' clause" clause '()))))

;*---------------------------------------------------------------------*/
;*    type-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-parser import clause clauses)
   (trace (ast 2) "type-parser: " clause " " clauses #\Newline)
   (match-case clause
      (((and ?id (? symbol?)) (and ?name (? string?)))
       ;; the simple type declaration (with default class)
       (declare-type! id name 'bigloo))
      (((and ?id (? symbol?)) (and ?name (? string?)) (and ?class (? symbol?)))
       ;; the simple type declaration
       (declare-type! id name class))
      ((magic (and ?id (? symbol?))
	      (and ?name (? string?))
	      (and ?class (? symbol?)))
       ;; a magic type
       (let ((type (declare-type! id name class)))
	  (type-magic?-set! type #t)
	  type))
      ((subtype (and (? symbol?) ?child) (and ?name (? string?))
		(and ?parent (? pair?)))
       (let loop ((walk  parent)
		  (class #unspecified))
	  (cond
	     ((null? walk)
	      (declare-subtype! child name parent class))
	     ((not (symbol? (car walk)))
	      (user-error "Parse error" "Illegal type declaration" clause))
	     (else
	      (let ((tparent (find-type (car walk))))
		 (cond
		    ((not (type? tparent))
		     (user-error "Subtype" "Unknow parent type" clause))
		    ((and (symbol? class)
			  (not (eq? class (type-class tparent))))
		     (user-error "Subtype"
				 "Parents are of different classes"
				 clause))
		    (else
		     (loop (cdr walk)
			   (type-class tparent)))))))))
      ((tvector (and (? symbol?) ?id) ((and (? symbol?) ?item-type)))
       (delay-tvector-type! id item-type clause import))
      ((coerce (and (? symbol?) ?from) (and (? symbol?) ?to) ?check ?coerce)
       (if (and (let loop ((check check))
		   (cond
		      ((null? check)
		       #t)
		      ((not (symbol? (car check)))
		       (match-case (car check)
			  ((lambda (?-) ?-)
			   (loop (cdr check)))
			  ((@ (? symbol?) (? symbol?))
			   (loop (cdr check)))
			  (else
			   (user-error "Coercion"
			      "Illegal coerce clause"
			      clause #f))))
		      (else
		       (loop (cdr check)))))
		(let loop ((coerce coerce))
		   (cond
		      ((null? coerce)
		       #t)
		      ((match-case (car coerce)
			  ((? symbol?) #f)
			  ((@ (? symbol?) (? symbol?)) #f)
			  ((lambda (?-) . ?-) #f)
			  (else #t))
		       (user-error "Coercion" "Illegal clause" clause #f))
		      (else
		       (loop (cdr coerce))))))
	   (let* ((loc (find-location clause))
		  (tfrom (use-type! from loc))
		  (tto (use-type! to loc))
		  (checks (if (null? check)
			      (list (cons #t tfrom))
			      (map (lambda (c) (cons c tfrom)) check)))
		  (coerces (if (null? coerce)
			       (list (cons #t tto))
			       (map (lambda (c) (cons c tto)) coerce))))
	      (cond
		 ((not (type? tfrom))
		  (user-error "type coercion" "Unknow type" from '()))
		 ((not (type? tto))
		  (user-error "type coercion" "Unknow type" to '()))
		 (else
		  (add-coercion! tfrom tto checks coerces))))
	   '()))
      (else
       (user-error "Parse error" "Illegal type declaration" clause '()))))

;*---------------------------------------------------------------------*/
;*    parse-tvector-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (module-tvector-clause id item-type clause import)
   (delay-tvector! (declare-tvector-type! id item-type clause) clause import))

;*---------------------------------------------------------------------*/
;*    type-finalizer ...                                               */
;*---------------------------------------------------------------------*/
(define (type-finalizer)
   (let ((tvf (tvector-finalizer)))
      (if (unit? tvf)
	  (list tvf)
	  'void)))

;*---------------------------------------------------------------------*/
;*    *tvector-types* ...                                              */
;*---------------------------------------------------------------------*/
(define *tvector-types* '())

;*---------------------------------------------------------------------*/
;*    delay-tvector-type!  ...                                         */
;*---------------------------------------------------------------------*/
(define (delay-tvector-type! id item-type clause import)
   (set! *tvector-types*
	 (cons (list id item-type clause import) *tvector-types*)))

;*---------------------------------------------------------------------*/
;*    *tvectors* ...                                                   */
;*---------------------------------------------------------------------*/
(define *tvectors* '())

;*---------------------------------------------------------------------*/
;*    delay-tvector!  ...                                              */
;*---------------------------------------------------------------------*/
(define (delay-tvector! tv clause import)
   (set! *tvectors* (cons (list tv clause import) *tvectors*))
   tv)

;*---------------------------------------------------------------------*/
;*    tvector-finalizer ...                                            */
;*---------------------------------------------------------------------*/
(define (tvector-finalizer)
   ;; bind the delayed types
   (for-each (lambda (t) (apply module-tvector-clause t)) *tvector-types*)
   (set! *tvector-types* '())
   ;; bind the new accessors
   (if (null? *tvectors*)
       'void
       (let ((accesses (append-map (lambda (tv)
				      (apply make-tvector-accesses tv))
				   *tvectors*)))
	  (let ((res (unit (gensym 'tvector)
			   6
			   (reverse! accesses)
			   #t #f)))
	     (set! *tvectors* '())
	     res))))
