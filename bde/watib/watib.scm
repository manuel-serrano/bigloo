;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Command line interface gluing all the phases together.

(module watib
   (main main)
   (library pthread srfi1)
   (import (misc_parse   "Misc/parse.scm")
           (misc_list    "Misc/list.scm")
           (val_validate "Val/validate.scm")
           (opt_optimise "Opt/optimise.scm")
           (asm_binary   "Asm/binary.scm")))

;; the following is a hack as indices taken as number are not replaced with
;; their new index
(define (merge-files l)
   (define (get-mfs f)
      (call-with-input-file f
         (lambda (ip)
            (match-case (read ip #t)
               ((or (module (? ident?) . ?mfs) (module . ?mfs)) mfs)
               (?m (error/location "watib" "expected module" m
                                   (cadr (cer m)) (caddr (cer m))))))))

   (econcat (map get-mfs l)))

(define (main argv)
   (define input-files '())
   (define nthreads 1)
   (define keep-going #f)
   (define output-file "a.out.wasm")
   (define silent #f)
   (define opt? #t)
   (define validate-only #f)

   (define (parse-args args)
      (args-parse args
         ((("--help" "-h") (help "Display this help message and quit"))
          (args-parse-usage #f))
         ((("--keep-going" "-k") (help "Continue when encountering an error"))
          (set! keep-going #t))
         (("--stop-after" ?n (help "Stop after encountering N errors"))
          (set! keep-going (string->number n)))
         (("-s" (help "Display less verbose error messages"))
          (set! silent #t))
         (("-j" ?n (help "Use multiple threads"))
          (set! nthreads (string->number n)))
         ((("--validate-only" "-v") (help "Validate only"))
          (set! validate-only #t))
         (("-O0" (help "Disable optimisations"))
          (set! opt? #f))
         (("-O1" (help "Toggle optimisations (default)"))
          (set! opt? #t))
         (("-o" ?file (help "Output binary format to FILE"))
          (set! output-file file))
         (else
          (set! input-files (cons else input-files)))))

   (define (watib m)
      (let ((p (with-handler
          (lambda (e)
             (when (isa? e &watib-validate-error)
                (exit 1))
             (raise e))
          (valid-file m nthreads keep-going silent))))
     (cond
        ((not p)
         (exit 1))
        (validate-only
         (exit 0))
        (else
         (when opt?
            (opt-file! p nthreads))
         (call-with-output-file output-file
            (lambda (op) (asm-file! p op)))))))

   (parse-args (cdr argv))

   (if (null? input-files)
       (watib (read (current-input-port) #t))
       (watib `(module ,@(merge-files input-files)))))
