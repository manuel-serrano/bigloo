(module ex15
   (cond-expand
      (wasm (library browser)))
   (export ex15f))

(define (ex15f)
   (cond-expand
      (wasm (typeof get-element-by-id))
      (else (typeof ex15f))))
