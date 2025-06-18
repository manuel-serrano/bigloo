(module util
   (main main))

(define idents (make-hashtable))

(define (main x)
   (let ((module (apply append
		    (filter (lambda (e)
			       (match-case e
				  ((type . ?-) #t)
				  (else #f)))
		       (cddr (call-with-input-file (cadr x) read)))
		    (map (lambda (f)
			    (call-with-input-file f
			       (lambda (p)
				  (let ((m (cddr (read p))))
				     (filter-map (lambda (c)
						    (match-case c
						       ((import ?mod ?id ?decl)
							(parse-import mod id decl))))
					(cdr m))))))
		       (cddr x)))))
      (pp `(module $__bigloo_dummy_module ,@module))))

(define (parse-import mod id decl)
   (unless (hashtable-contains? idents id)
      (hashtable-put! idents id #t)
      (match-case decl
	 ((func . ?s)
	  (when (should-generate? mod)
	     (parse-func id s)))
	 ((global . ?s) 
	  (when (should-generate? mod)
	     (parse-global id s))))))

(define cnt 0)
   
(define (parse-func id sig)
   
   (define (emit-unimplemented)
      (set! cnt (+fx cnt 1))
      ;; '((throw $unimplemented))
      `(call $not_implemented (i32.const ,cnt)))
   
   (unless (string=? id "bigloo_main")
      (let ((retty 'void))
	 `(func ,(car sig)
	     (export ,id)
	     ,@(filter (lambda (d)
			  (match-case d
			     ((type ???-) #t)
			     ((result ?type)
			      (set! retty type)
			      #t)
			     ((param ???-) #t)
			     (else #f)))
		  (cdr sig))
	     ,(emit-unimplemented)
	     ,@(if (eq? retty 'void)
		   '()
		   (list (emit-default-value retty sig)))))))

(define (parse-global id sig)
    (match-case sig
        ((?name ?type)
	 `(global ,name (export ,id) ,type ,(emit-default-value type sig)))
        (else
	 (error "parse-global" "Unknown global signature." sig))))

(define (should-generate? mod)
    (not (string-prefix? "__js" mod)))

(define (result-type result)
    (match-case result
        ((result ?type) type)
        (else 'eqref)))

(define (emit-default-value type sig)
   (case type
      ;; TODO: implement types
      ((i32) '(i32.const 0))
      ((i64) '(i64.const 0))
      ((f32) '(f32.const 0))
      ((f64) '(f64.const 0))
      ((eqref) '(global.get $BUNSPEC))
      (else
       (match-case type
	  ((or (mut (ref ?type)) (ref ?type))
	   (case type
	      ((eq)
	       '(global.get $BUNSPEC))
	      ((i31)
	       '(ref.i31 (i32.const 0)))
	      (else
 	       `(global.get ,(symbol-append type '-default-value)))))
	  (else
	   (error "gen_dummy_wasm_import"
	      (format "No default init value for builtin type: ~a" type)
	      sig))))))
