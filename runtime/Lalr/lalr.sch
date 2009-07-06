;; ---------------------------------------------------------------------- ;;
;; FICHIER               : larl.sch                                       ;;
;; DATE DE CREATION      : Thu Jun 29 15:09:21 1995                       ;;
;; DERNIERE MODIFICATION : Tue Jul  4 09:44:35 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Quelques macros pour le générateur LALR(1) ...                         ;;
;; ---------------------------------------------------------------------- ;;
;;; ---------- SYSTEM DEPENDENT SECTION -----------------

(define-macro (BITS-PER-WORD) 28)
(define-macro (logical-or x . y) `(bit-or ,x ,@y))

;;; ---------- END OF SYSTEM DEPENDENT SECTION ------------

;; - Macros pour la gestion des vecteurs de bits

(define-macro (set-bit v b)
  `(let ((x (quotient ,b (BITS-PER-WORD)))
	 (y (expt 2 (remainder ,b (BITS-PER-WORD)))))
     (vector-set! ,v x (logical-or (vector-ref ,v x) y))))

(define-macro (bit-union v1 v2 n)
  `(do ((i 0 (+ i 1)))
       ((= i ,n))
     (vector-set! ,v1 i (logical-or (vector-ref ,v1 i) 
				    (vector-ref ,v2 i)))))

;; - Macro pour les structures de donnees

(define-macro (new-core)              `(make-vector 4 0))
(define-macro (set-core-number! c n)  `(vector-set! ,c 0 ,n))
(define-macro (set-core-acc-sym! c s) `(vector-set! ,c 1 ,s))
(define-macro (set-core-nitems! c n)  `(vector-set! ,c 2 ,n))
(define-macro (set-core-items! c i)   `(vector-set! ,c 3 ,i))
(define-macro (core-number c)         `(vector-ref ,c 0))
(define-macro (core-acc-sym c)        `(vector-ref ,c 1))
(define-macro (core-nitems c)         `(vector-ref ,c 2))
(define-macro (core-items c)          `(vector-ref ,c 3))

(define-macro (new-shift)              `(make-vector 3 0))
(define-macro (set-shift-number! c x)  `(vector-set! ,c 0 ,x))
(define-macro (set-shift-nshifts! c x) `(vector-set! ,c 1 ,x))
(define-macro (set-shift-shifts! c x)  `(vector-set! ,c 2 ,x))
(define-macro (shift-number s)         `(vector-ref ,s 0))
(define-macro (shift-nshifts s)        `(vector-ref ,s 1))
(define-macro (shift-shifts s)         `(vector-ref ,s 2))

(define-macro (new-red)                `(make-vector 3 0))
(define-macro (set-red-number! c x)    `(vector-set! ,c 0 ,x))
(define-macro (set-red-nreds! c x)     `(vector-set! ,c 1 ,x))
(define-macro (set-red-rules! c x)     `(vector-set! ,c 2 ,x))
(define-macro (red-number c)           `(vector-ref ,c 0))
(define-macro (red-nreds c)            `(vector-ref ,c 1))
(define-macro (red-rules c)            `(vector-ref ,c 2))

(define-macro (new-set nelem)
  `(make-vector ,nelem 0))

(define-macro (vector-map f v)
  `(let ((vm-n (- (vector-length ,v) 1)))
    (let loop ((vm-low 0) (vm-high vm-n))
      (if (= vm-low vm-high)
	  (vector-set! ,v vm-low (,f (vector-ref ,v vm-low) vm-low))
	  (let ((vm-middle (quotient (+ vm-low vm-high) 2)))
	    (loop vm-low vm-middle)
	    (loop (+ vm-middle 1) vm-high))))))


