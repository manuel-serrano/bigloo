;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/checksum.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Aug 21 08:38:45 1997                          */
;*    Last change :  Sat Jun 14 06:50:12 2014 (serrano)                */
;*    Copyright   :  1997-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We compute checksum for modules in order to be able to check,    */
;*    at module initialization time, that modules are coherent. Only   */
;*    exported and extern values (bindings and classes) are considered */
;*    for checksumming.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_checksum
   (export (module-checksum::long mclause::pair ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    module-checksum ...                                              */
;*---------------------------------------------------------------------*/
(define (module-checksum module include-path)
   (clause-checksum (symbol->number (cadr module)) (cddr module) include-path))

;*---------------------------------------------------------------------*/
;*    include-checksum ...                                             */
;*---------------------------------------------------------------------*/
(define (include-checksum include checksum include-path)
   (define (directives? exp)
      (match-case exp
	 ((directives . ?-)
	  #t)
	 (else
	  #f)))
   (let ((fi (find-file/path include *load-path*)))
      (if (and (string? fi) (file-exists? fi))
	  (let* ((port (open-input-file fi))
		 (dir  (unwind-protect (let ((exp (read port)))
					  (if (directives? exp)
					      exp
					      #f))
				       (close-input-port port))))
	     (if dir
		 (clause-checksum (atom->number checksum include)
				  (cdr dir)
				  include-path)
		 checksum))
	  (error 'include-checksum "Can't find include file" include))))
	      
;*---------------------------------------------------------------------*/
;*    clause-checksum ...                                              */
;*---------------------------------------------------------------------*/
(define (clause-checksum checksum clauses include-path)
   (let loop ((clauses  clauses)
	      (checksum checksum))
      (match-case clauses
	 (()
	  checksum)
	 (((export . ?export) . ?rest)
          (loop rest (list->number checksum export)))
	 ((((or foreign extern java) . ?export) . ?rest)
	  (loop rest (extern-clause-checksum checksum export include-path)))
	 (((include . ?files) . ?rest)
	  (let laap ((files    files)
		     (checksum checksum))
	     (if (null? files)
		 (loop rest checksum)
		 (laap (cdr files) (include-checksum (car files)
						     checksum
						     include-path)))))
	 (else
	  (loop (cdr clauses) checksum)))))

;*---------------------------------------------------------------------*/
;*    symbol->number ...                                               */
;*---------------------------------------------------------------------*/
(define (symbol->number symbol)
   (define (type-component str::string)
      (let ((len (-fx (string-length str) 1)))
	 (let loop ((i 0)
		    (armed? #f))
	    
	    (cond
	       ((=fx i len)
		str)
	       ((char=? (string-ref str i) #\:)
		(if armed?
		    (substring str (+fx i 1) (+fx len 1))
		    (loop (+fx i 1) #t)))
	       (else
		(loop (+fx i 1) #f))))))
   (get-hashnumber (type-component (symbol->string symbol))))

;*---------------------------------------------------------------------*/
;*    keyword->number ...                                              */
;*---------------------------------------------------------------------*/
(define (keyword->number keyword)
   0)

;*---------------------------------------------------------------------*/
;*    atom->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (atom->number checksum clause)
   (cond
      ((fixnum? clause)
       (bit-xor checksum clause))
      ((flonum? clause)
       (atom->number checksum (real->string clause)))
      ((char? clause)
       (bit-xor checksum (+fx 23 (char->integer clause))))
      ((cnst? clause)
       (bit-xor checksum (+fx 90 (cnst->integer clause))))
      ((string? clause)
       (bit-xor checksum (+fx 4 (get-hashnumber clause))))
      ((symbol? clause)
       (bit-xor checksum (+fx 150 (symbol->number clause))))
      ((keyword? clause)
       (bit-xor checksum (+fx 151 (keyword->number clause))))
      ((pair? clause)
       (list->number checksum clause))
      ((elong? clause)
       (bit-xor checksum (elong->fixnum clause)))
      ((llong? clause)
       (bit-xor checksum
		(bit-xorllong (bit-rshllong clause 32)
			      (bit-rshllong (bit-lshllong clause 32) 32))))
      ((int8? clause)
       (bit-xor checksum (int8->fixnum clause)))
      ((uint8? clause)
       (bit-xor checksum (uint8->fixnum clause)))
      ((int16? clause)
       (bit-xor checksum (int16->fixnum clause)))
      ((uint16? clause)
       (bit-xor checksum (uint16->fixnum clause)))
      ((int32? clause)
       (bit-xor checksum (int32->fixnum clause)))
      ((uint32? clause)
       (bit-xor checksum (uint32->fixnum clause)))
      ((int64? clause)
       (bit-xor checksum (int64->fixnum clause)))
      ((uint64? clause)
       (bit-xor checksum (uint64->fixnum clause)))
      ((int32? clause)
       (bit-xor checksum (int32->fixnum clause)))
      (else
       (warning "module checksum:Unknown clause" clause " -- "
		(find-runtime-type clause))
       0)))

;*---------------------------------------------------------------------*/
;*    list->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->number checksum clause)
   (match-case clause
      (()
       checksum)
      (((or default assert info) . ?-)
       0)
      (else
       (if (pair? clause)
	   (list->number (atom->number checksum (car clause)) (cdr clause))
	   (atom->number checksum clause)))))
	 
;*---------------------------------------------------------------------*/
;*    extern-clause-checksum ...                                       */
;*---------------------------------------------------------------------*/
(define (extern-clause-checksum checksum clauses include-path)
   (let loop ((clauses  clauses)
	      (checksum checksum))
      (match-case clauses
	 (()
	  checksum)
	 ;; checksum value needs to change if timestamp of C include files
	 ;; changes
	 (((include ?file) . ?rest)
	  (let* ((fullname (find-file/path file include-path))
		 (time (if (and (string? fullname) (file-exists? fullname))
			   (flonum->fixnum
			    (elong->flonum
			     (file-modification-time fullname)))
			   0)))
	     (loop (cdr clauses)
		   (atom->number checksum time))))
	 (else
	  (loop (cdr clauses)
		(atom->number checksum (car clauses)))))))
			 
