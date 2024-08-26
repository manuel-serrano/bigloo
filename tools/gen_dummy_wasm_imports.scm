(module util
   (main main))

(define already-generated (make-hashtable))
(define (main x)
    (let ((module '(module $dummy_module)))
        (append! module (call-with-input-file (cadr x) port->sexp-list))
        (for-each (lambda (f)
            (append! module (call-with-input-file f
            (lambda (p)
                (let* ((m (read p)))
                    (filter-map (lambda (c)
                        (match-case c
                            ((import ?mod ?id ?decl) (parse-import mod id decl))))
                    (cdr m)))))))
        (cddr x))
        (pp module)))

(define (parse-import mod id import)
    (match-case import
        ((func . ?s) 
            (when (and (should-generate? mod) (not (hashtable-contains? already-generated id)))
                (hashtable-put! already-generated id #t)
                (parse-func id s)))
        ((global . ?s) 
            (when (and (should-generate? mod) (not (hashtable-contains? already-generated id)))
                (hashtable-put! already-generated id #t)
                (parse-global id s)))))

(define (parse-func id sig)
    (define (emit-unimplemented)
        ;; '((throw $unimplemented))
        '())

    (if (string=? id "bigloo_main")
        #f ;; FIXME: why we need a special case for bigloo_main, try to remove it.
        (let ((retty 'void))
            `(func ,(car sig)
                (export ,id)
                ,@(filter 
                    (lambda (d)
                        (match-case d
                            ((type ???-) #t)
                            ((result ?type)
                                (set! retty type)
                                #t)
                            ((param ???-) #t)
                            (else #f)))
                    (cdr sig))
                ,@(emit-unimplemented)
                ,@(if (eq? retty 'void)
                    '()
                    (list (emit-default-value retty)))))))

(define (parse-global id sig)
    (match-case sig
        ((?name ?type) `(global ,name (export ,id) ,type ,(emit-default-value type)))
        (else (error "parse-global" "Unknown global signature." sig))))

(define (should-generate? mod)
    (not (string-prefix? "__js" mod)))

(define (result-type result)
    (match-case result
        ((result ?type) type)
        (else 'eqref)))

(define (emit-default-value type)
    (case type
        ('i32 '(i32.const 0))
        ('i64 '(i64.const 0))
        ('f32 '(f32.const 0))
        ('f64 '(f64.const 0))
        ('eqref '(global.get $BUNSPEC))
        (else '(ref.null none))
    ))
