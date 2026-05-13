(module ex9
   (export ex9ma ex9mb ex9f))

(define-expander ex9ma
   (lambda (x e)
      `(cons ,(e (cadr x) e) 'ex9m)))

(define-expander ex9mb
   (lambda (x e)
      `(cons ,(e (cadr x) e) 'ex9mb)))

(define (ex9f)
   (ex9ma 340))
