;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/type.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 28 10:33:46 1995                          */
;*    Last change :  Fri Nov  4 12:10:46 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The type translation                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_type
   (include "Translate/ast.sch"
	    "Translate/type.sch")
   (import  engine_param
	    tools_error
	    tools_speek
	    translate_tspec
	    translate_expr)
   (export  (translate-types!)
	    (make-simple      <string> <string> <list>)
	    (make-typedef     <string> <type>)
	    (make-...         <type>)
	    (make-ptr-to      <type>)
	    (make-array       <type> <ast>)
	    (make-function    <type> <type>*)
	    (make-cstruct     <string> <string> <string> <symbol> <list>)
	    (make-enum        <string> <string> <string> <list>)
	    (replace-$        <string> <string>)
	    ($-in-name?       <string>)))

;*---------------------------------------------------------------------*/
;*    *Tenv*                                                           */
;*---------------------------------------------------------------------*/
(define *Tenv* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *Tlist* ...                                                      */
;*    -------------------------------------------------------------    */
;*    This variable is used to retreive type in their definition       */
;*    order.                                                           */
;*---------------------------------------------------------------------*/
(define *Tlist* '())

;*---------------------------------------------------------------------*/
;*    find-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-type id)
   (hashtable-get *Tenv* id))

;*---------------------------------------------------------------------*/
;*    bind-type! ...                                                   */
;*---------------------------------------------------------------------*/
(define (bind-type! type)
   (set! *Tlist* (cons type *Tlist*))
   (hashtable-put! *Tenv* (type-id type) type))
      
;*---------------------------------------------------------------------*/
;*    reset-type-list! ...                                             */
;*---------------------------------------------------------------------*/
(define (reset-type-list!)
   (set! *Tlist* '()))
   
;*---------------------------------------------------------------------*/
;*    string-sans-$ ...                                                */
;*    -------------------------------------------------------------    */
;*    This function allocates a new string where `$' are replaced      */
;*    by ` '.                                                          */
;*---------------------------------------------------------------------*/
(define (string-sans-$ string)
   (let ((new (string-copy string)))
      (let loop ((i (-fx (string-length new) 1)))
	 (cond
	    ((=fx i -1)
	     new)
	    ((char=? (string-ref new i) #\$)
	     (string-set! new i #\space)
	     (loop (-fx i 1)))
	    (else
	     (loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    type-name-sans-$ ...                                             */
;*---------------------------------------------------------------------*/
(define (type-name-sans-$ type)
   (let ((tname (type-c-name type)))
      (if (type-$ type)
	  (string-sans-$ tname)
	  tname)))

;*---------------------------------------------------------------------*/
;*    $-in-name? ...                                                   */
;*    -------------------------------------------------------------    */
;*    Is a name contains a `$' ?                                       */
;*---------------------------------------------------------------------*/
(define ($-in-name? name)
   (let loop ((i (-fx (string-length name) 1)))
      (cond
	 ((=fx i -1)
	  #f)
	 ((char=? (string-ref name i) #\$)
	  #t)
	 (else
	  (loop (-fx i 1))))))

;*---------------------------------------------------------------------*/
;*    *-name? ...                                                      */
;*    -------------------------------------------------------------    */
;*    Is a name stopped on a `*' ?                                     */
;*---------------------------------------------------------------------*/
(define (*-name? name)
   (char=? (string-ref name (-fx (string-length name) 1)) #\*))

;*---------------------------------------------------------------------*/
;*    replace-$ ...                                                    */
;*---------------------------------------------------------------------*/
(define (replace-$ string rplac)
   (replace-char #\$ string rplac))

;*---------------------------------------------------------------------*/
;*    replace-space ...                                                */
;*---------------------------------------------------------------------*/
(define (replace-space string rplac)
   (replace-char #\space string rplac))

;*---------------------------------------------------------------------*/
;*    replace-char ...                                                 */
;*---------------------------------------------------------------------*/
(define (replace-char char string rplac)
   (let* ((len-string (string-length string))
	  (len-rplac  (string-length rplac))
	  (len        (-fx (+fx len-string len-rplac) 1))
	  (new        (make-string len)))
      (let loop ((r 0)
		 (w 0))
	 (cond
	    ((=fx r len-string)
	     (if (=fx w r)
		 string
		 new))
	    ((char=? (string-ref string r) char)
	     ;; we insert rplac
	     (let liip ((w  w)
			(rr 0))
		(if (=fx rr len-rplac)
		    (loop (+fx r 1) w)
		    (begin
		       (string-set! new w (string-ref rplac rr))
		       (liip (+fx w 1) (+fx rr 1))))))
	    (else
	     (string-set! new w (string-ref string r))
	     (loop (+fx r 1) (+fx w 1)))))))

;*---------------------------------------------------------------------*/
;*    make-ptr-type-name ...                                           */
;*---------------------------------------------------------------------*/
(define (make-ptr-type-name type)
   (let ((tname (type-c-name type)))
      (cond
	 ((not (type-$ type))
	  (if (*-name? tname)
	      (string-append tname "*")
	      (string-append tname " *")))
	 (else
	  (replace-$ tname "(*$)")))))

;*---------------------------------------------------------------------*/
;*    compose-type-name ...                                            */
;*---------------------------------------------------------------------*/
(define (compose-type-name type dim)
   (let ((tname (type-c-name type)))
      (cond
	 ((not (type-$ type))
	  (if (*-name? tname)
	      (string-append tname dim)
	      (string-append tname " " dim)))
	 (else
	  (replace-$ tname dim)))))

;*---------------------------------------------------------------------*/
;*    make-array-type-name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-array-type-name type dim)
   (compose-type-name type dim))

;*---------------------------------------------------------------------*/
;*    make-function-type-name ...                                      */
;*---------------------------------------------------------------------*/
(define (make-function-type-name to from-name)
   (compose-type-name to (string-append "($(" from-name "))")))

;*---------------------------------------------------------------------*/
;*    make-... ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-... type)
   (let ((previous-id (type-id type)))
      (type-simple-t (string-append ". " previous-id)
		     "..."
		     #f
		     '(...))))

;*---------------------------------------------------------------------*/
;*    make-simple ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-simple id c-name ids)
   (let ((old (find-type id)))
      (if (type? old)
	  old
	  (let ((new (type-simple-t id c-name ($-in-name? c-name) ids)))
	     (bind-type! new)
	     new))))
   
;*---------------------------------------------------------------------*/
;*    make-typedef ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-typedef id alias)
   (let ((type (type-typedef-t id id ($-in-name? id) alias)))
      (bind-type! type)
      type)) 

;*---------------------------------------------------------------------*/
;*    make-ptr-to ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-ptr-to type)
   ;; first we check if we are not introducing a simple char *
   (cond
      ((and (simple-t? type) (string=? (simple-t-id type) "char"))
       (simple->type 'char* "char *"))
      ((and (simple-t? type) (string=? (simple-t-id type) "FILE"))
       (simple->type 'file* "FILE *"))
      ((function-t? type)
       (make-ptr-to-function type))
      (else
       (let* ((type-id  (string-append (type-id type) "*"))
	      (ptr-type (find-type type-id)))
	  (if (type? ptr-type)
	      ptr-type
	      (let ((ptr-type (type-pointer-t type-id
					      (make-ptr-type-name type)
					      (type-$ type)
					      type)))
		 (bind-type!  ptr-type)
		 ptr-type))))))

;*---------------------------------------------------------------------*/
;*    make-ptr-to-function ...                                         */
;*---------------------------------------------------------------------*/
(define (make-ptr-to-function type)
   (let* ((type-id  (string-append "*" (type-id type)))
	  (ptr-type (find-type type-id)))
      (if (type? ptr-type)
	  ptr-type
	  (let* ((fun-type (find-type type-id))
		 (ptr-type (type-pointer-t type-id
					   (make-ptr-type-name type)
					   (type-$ type)
					   type)))
	     (bind-type!  ptr-type)
	     ptr-type))))

;*---------------------------------------------------------------------*/
;*    make-array ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-array type constant-expr)
   (let* ((expr-string
	   (if (null? constant-expr) "" (expr->scheme-id constant-expr)))
	  (type-id     (string-append (type-id type) "-array"
				      (if (null? constant-expr) ""
					  (string-append "-" expr-string))))
	  (array-type  (find-type type-id)))
      (if (type? array-type)
	  array-type
	  (let ((array-type (type-array-t type-id
					  (make-array-type-name
					   type
					   (string-append "$[ "
							  expr-string
							  " ]"))
					  #t
					  type)))
	     (bind-type!  array-type)
	     array-type))))

;*---------------------------------------------------------------------*/
;*    make-function ...                                                */
;*---------------------------------------------------------------------*/
(define (make-function to from)
   (let* ((from-id-name (cond
			   ((null? from)
			    (cons "" ""))
			   ((null? (cdr from))
			    (cons (type-id (car from))
				  (if (type-$ (car from))
				      (replace-$ (type-c-name (car from)) "")
				      (type-c-name (car from)))))
			   (else
			    (let loop ((from from))
			       (if (null? (cdr from))
				   (cons (replace-space
					  (type-id (car from))
					  "..")
					 (if (type-$ (car from))
					     (replace-$
					      (type-c-name (car from))
					      "")
					     (type-c-name (car from))))
				   (let ((rec (loop (cdr from))))
				      (cons
				       (string-append (type-id (car from))
						      ","
						      (car rec))
				       (string-append (if (type-$ (car from))
	 						  (replace-$
							   (type-c-name
							    (car from))
							   "")
							  (type-c-name
							   (car from)))
						      ","
						      (cdr rec)))))))))
	  (from-id      (car from-id-name))
	  (from-name    (cdr from-id-name))
	  (type-id      (string-append from-id "->" (type-id to))))
      (let ((old-type (find-type type-id)))
	 (if (type? old-type)
	     old-type
	     (let ((fun-type (type-function-t
			      type-id
			      (make-function-type-name to from-name)
			      #t
			      to
			      from)))
		(bind-type! fun-type)
		fun-type)))))

;*---------------------------------------------------------------------*/
;*    make-cstruct ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-cstruct id c-name tag class fields)
   (let ((old-type (find-type id)))
      (if (type? old-type)
	  old-type
	  (let ((new-type (type-struct-t id c-name #f tag class fields)))
	     (bind-type! new-type)
	     new-type))))

;*---------------------------------------------------------------------*/
;*    make-enum ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-enum id c-name tag fields)
   (let ((old-type (find-type id)))
      (if (type? old-type)
	  old-type
	  (let ((new-type (type-enum-t id c-name #f tag fields)))
	     (bind-type! new-type)
	     new-type))))

;*---------------------------------------------------------------------*/
;*    translate-type ...                                               */
;*---------------------------------------------------------------------*/
(define (translate-type type)
   (define (translate-typedef-t type)
      (let ((alias (typedef-t-alias type)))
	 (fprint *oport* "   (type " (type-id type) " " (type-id alias) " \""
		 (type-c-name type) "\")")))
   (define (get-aliased-type type)
      (if (typedef-t? type)
	  (get-aliased-type (typedef-t-alias type))
	  type))
   (define (translate-pointer-t ptr)
      (if (or (struct-t? (pointer-t-type ptr))
	      (and (typedef-t? (pointer-t-type ptr))
		   (struct-t? (get-aliased-type (pointer-t-type ptr)))))
	  ;; there is no need to emit a type because, Bigloo will
	  ;; automatically creates a pointer to the structure.
	  'nothing
	  (if (member (type-c-name ptr) *no-type*)
	      'nothing
	      (fprint *oport*
		      "   (type "
		      (type-id ptr)
		      " (pointer "
		      (type-id (pointer-t-type ptr))
		      ") \""
		      (type-c-name ptr)
		      "\")"))))
   (define (translate-pointer-function-t ptr)
      (if (member (type-c-name ptr) *no-type*)
	  'nothing
	  (fprint *oport*
		  "   (type "
		  (type-id ptr)
		  " (function "
		  (type-id (function-t-to (pointer-t-type ptr)))
		  " "
		  (map type-id (function-t-from (pointer-t-type ptr)))
		  ") \""
		  (type-c-name ptr)
		  "\")")))
   (define (translate-function-t fun)
      (if (member (type-c-name fun) *no-type*)
	  'nothing
	  (fprint *oport*
		  "   (type "
		  (type-id fun)
		  " \""
		  (type-c-name fun)
		  "\")")))
   (define (translate-struct-t struct)
      (if (null? (struct-t-fields struct))
	  (warning "cigloo"
		   "incomplete struct type -- "
		   (struct-t-c-name struct)))
      (if (member (type-c-name struct) *no-type*)
	  'nothing
	  (begin
	     (fprin *oport*
		    "   (type "
		    (type-id struct)
		    (if (eq? (struct-t-class struct) 'struct)
			" (struct"
			" (union"))
	     (for-each (lambda (field)
			  (fprin *oport*
				 " ("
				 (cdr field)
				 "::"
				 (type-id (car field))
				 " \""
				 (cdr field)
				 "\")"))
		       (struct-t-fields struct))
	     (fprint *oport*
		     ") \""
		     (type-c-name struct)
		     "\")"))))
   (define (translate-enum-t enum)
      (if (null? (enum-t-fields enum))
	  (warning "cigloo"
		   "incomplete enum type -- "
		   (enum-t-c-name enum)))
      (cond
	 ((member (type-c-name enum) *no-type*)
	  'nothing)
	 (*int-enum*
	  (fprint *oport*
		  "   (type "
		  (type-id enum)
		  " int "
		  "\""
		  (type-c-name enum)
		  "\")")
	  (for-each (lambda (field)
		       (fprint *oport*
			       "   (macro "
			       (ident-id field)
			       "::int \""
			       (ident-id field)
			       "\")"))
		    (enum-t-fields enum)))
	 (else
	  (fprin *oport*
		 "   (type "
		 (type-id enum)
		 " (enum")
	  (for-each (lambda (field)
		       (fprin *oport*
			      " ("
			      (ident-id field)
			      " \""
			      (ident-id field)
			      "\")"))
		    (enum-t-fields enum))
	  (fprint *oport* ") \""
		  (type-c-name enum)
		  "\")")
	  (if *enum-macros*
	      (for-each (lambda (field)
			   (fprint *oport*
				   "   (macro "
				   (ident-id field)
				   "::"
				   (type-id enum)
				   " \""
				   (ident-id field)
				   "\")"))
			(enum-t-fields enum))))))
   (define (translate-array-t array)
      (if (member (type-c-name array) *no-type*)
	  'nothing
	  (fprint *oport*
		  "   (type "
		  (type-id array)
		  " (array "
		  (type-id (array-t-type array))
		  ") \""
		  (type-c-name array)
		  "\")")))
   (type-case type
	      ((typedef-t)
	       (translate-typedef-t type))
	      ((simple-t)
	       #unspecified)
	      ((pointer-t)
	       (if (function-t? (pointer-t-type type))
		   (translate-pointer-function-t type)
		   (translate-pointer-t type)))
	      ((function-t)
	       (translate-function-t type))
	      ((array-t)
	       (translate-array-t type))
	      ((struct-t)
	       (translate-struct-t type))
	      ((enum-t)
	       (translate-enum-t type))
	      (else
	       #unspecified)))

;*---------------------------------------------------------------------*/
;*    translate-types! ...                                             */
;*---------------------------------------------------------------------*/
(define (translate-types!)
   (for-each translate-type (reverse! *Tlist*))
   (reset-type-list!)
   #unspecified)
