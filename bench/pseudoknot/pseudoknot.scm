(module pseudoknot (main main))

;*---------------------------------------------------------------------*/
;*    defmacro ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (defmacro name+args . body)
   `(define-macro ,name+args ,@body))

;*---------------------------------------------------------------------*/
;*    quotientfx ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (quotientfx x y)
   `(/fx ,x ,y))

; file: "nucleic2.scm"
;
; author: marc feeley (feeley@iro.umontreal.ca)
;
; last modified: june 6, 1994
;
; this program is a modified version of the program described in the paper:
;
;   m. feeley, m. turcotte, g. lapalme, "using multilisp for solving
;   constraint satisfaction problems: an application to nucleic acid 3d
;   structure determination" published in the journal "lisp and symbolic
;   computation".
;
; the differences between this program and the original are described in
; the paper:
;
;   "???" published in the "journal of functional programming".

; -- system dependent code ----------------------------------------------------

; the code in this section is not portable.  it must be adapted to
; the scheme system you are using.

; ********** gambit 2.2

; add a single-quote at the start of this line if you are not using gambit

(defmacro (float+ x . l)
   (cond 
      ((null? l)
       x)
      ((null? (cdr l))
       `(+fl ,x ,(car l)))
      (else
       `(+fl ,x (float+ ,@l)))))

(defmacro (float- . l)
   (cond
      ((null? (cdr l))
       `(negfl ,(car l)))
      ((null? (cdr (cdr l)))
       `(-fl ,(car l) ,(cadr l)))
      (else
       `(-fl ,(car l) (float+ ,@(cdr l))))))

(defmacro (float* x . l)
   (cond
      ((null? l)
       x)
      ((null? (cdr l))
       `(*fl ,x ,(car l)))
      (else
       `(*fl ,x (float* ,@l)))))

(defmacro (float/ x . y)
    (if (pair? y)
	(let loop ((result `(/fl ,x ,(car y)))
		   (z (cdr y)))
	     (if (pair? z)
		 (loop `(/fl ,result ,(car z))
		       (cdr z))
		 result))
	`(/fl 1.0 ,x)))

(defmacro (float=   x y) `(=fl ,x ,y))
(defmacro (float<   x y) `(<fl ,x ,y))
(defmacro (float<=  x y) `(<=fl ,x ,y))
(defmacro (float>   x y) `(>fl ,x ,y))
(defmacro (float>=  x y) `(>=fl ,x ,y))

(defmacro (floatsin   x) `(sinfl  ,x))
(defmacro (floatcos   x) `(cosfl  ,x))
(defmacro (floatatan  x) `(atanfl ,x))
(defmacro (floatsqrt  x) `(sqrtfl ,x))


; -- structure definition macro -----------------------------------------------

; the macro "def-struct" provides a simple mechanism to define record
; structures out of vectors.  the first argument to "def-struct" is a boolean
; indicating whether the vector should be tagged (to allow the type of the
; structure to be tested).  the second argument is the name of the structure.
; the remaining arguments are the names of the structure's fields.  a call
; to "def-struct" defines macros to
;
;  1) construct a record object of this type
;  2) fetch and store each field
;  3) test a record to see if it is of this type (only if tags are used)
;  4) define subclasses of this record with additional fields
;
; the call "(def-struct #t foo a b c)" will define the following macros:
;
;  (make-foo x y)                -- make a record
;  (make-constant-foo x y)       -- make a record (args must be constants)
;  (foo? x)                      -- test a record
;  (foo-a x)                     -- get field "a"
;  (foo-b x)                     -- get field "b"
;  (foo-a-set! x y)              -- mutate field "a"
;  (foo-b-set! x y)              -- mutate field "b"
;  (def-foo tag? name fields...) -- define subclass of "foo"

(defmacro (def-struct tag? name . fields)
  `(def-substruct () () 0 ,tag? ,name ,@fields))

(defmacro (def-substruct sup-fields sup-tags sup-length tag? name . fields)

  (define (err)
    (error "ill-formed `def-substruct'") #f)

  (define (sym . strings)
    (string->symbol (apply string-append strings)))

  (if (symbol? name)
    (let* ((name-str (symbol->string name))
           (tag (sym "." name-str "."))
           (all-tags (append sup-tags
                             (if tag?
                               (list (cons tag sup-length))
                               '()))))
      (let loop ((l1 fields)
                 (l2 '())
                 (l3 '())
                 (i (+ sup-length (if tag? 1 0))))
        (if (pair? l1)
          (let ((rest (cdr l1)) (field (car l1)))
            (if (symbol? field)
              (let* ((field-str (symbol->string field))
                     (field-ref (sym name-str "-" field-str))
                     (field-set! (sym name-str "-" field-str "-set!")))
                (loop rest
                      (cons `(defmacro (,field-set! x y)
                               `(vector-set! ,x ,,i ,y))
                            (cons `(defmacro (,field-ref x)
                                     `(vector-ref ,x ,,i))
                                  l2))
                      (cons (cons field i) l3)
                      (+ i 1)))
              (err)))
          (let ((all-fields (append sup-fields (reverse l3))))
            `(begin
               ,@l2
               (define ,(sym "fields-of-" name-str)
                 ',all-fields)
               (defmacro (,(sym "def-" name-str) tag? name . fields)
                 `(def-substruct ,',all-fields ,',all-tags ,',i
                                 ,tag? ,name ,@fields))
               (defmacro (,(sym "make-constant-" name-str) . rest)
                 (define (add-tags i tags lst)
                   (cond ((null? tags)
                          lst)
                         ((eq? i (cdar tags))
                          (cons (caar tags)
                                (add-tags (+ i 1) (cdr tags) lst)))
                         (else
                          (cons (car lst)
                                (add-tags (+ i 1) tags (cdr lst))))))
                 `'#(,@(add-tags 0 ',all-tags rest)))
               (defmacro (,(sym "make-" name-str) . rest)
                 (define (add-tags i tags lst)
                   (cond ((null? tags)
                          lst)
                         ((eq? i (cdar tags))
                          (cons `',(caar tags)
                                (add-tags (+ i 1) (cdr tags) lst)))
                         (else
                          (cons (car lst)
                                (add-tags (+ i 1) tags (cdr lst))))))
                 `(vector ,@(add-tags 0 ',all-tags rest)))
               ,@(if tag?
                   `((defmacro (,(sym name-str "?") x)
                       `(eq? (vector-ref ,x ,,sup-length) ',',tag)))
                   '())
               ',name)))))
    (err)))


; -- math utilities -----------------------------------------------------------

(define constant-pi          3.14159265358979323846)
(define constant-minus-pi   -3.14159265358979323846)
(define constant-pi/2        1.57079632679489661923)
(define constant-minus-pi/2 -1.57079632679489661923)

(define (math-atan2 y x)
  (cond ((float> x 0.0)
         (floatatan (float/ y x)))
        ((float< y 0.0)
         (if (float= x 0.0)
           constant-minus-pi/2
           (float+ (floatatan (float/ y x)) constant-minus-pi)))
        (else
         (if (float= x 0.0)
           constant-pi/2
           (float+ (floatatan (float/ y x)) constant-pi)))))

; -- points -------------------------------------------------------------------

(def-struct #f pt x y z)

(define (pt-sub p1 p2)
  (make-pt (float- (pt-x p1) (pt-x p2))
           (float- (pt-y p1) (pt-y p2))
           (float- (pt-z p1) (pt-z p2))))

(define (pt-dist p1 p2)
  (let ((dx (float- (pt-x p1) (pt-x p2)))
        (dy (float- (pt-y p1) (pt-y p2)))
        (dz (float- (pt-z p1) (pt-z p2))))
    (floatsqrt (float+ (float* dx dx) (float* dy dy) (float* dz dz)))))

(define (pt-phi p)
  (let* ((x (pt-x p))
         (y (pt-y p))
         (z (pt-z p))
         (b (math-atan2 x z)))
    (math-atan2 (float+ (float* (floatcos b) z) (float* (floatsin b) x)) y)))

(define (pt-theta p)
  (math-atan2 (pt-x p) (pt-z p)))

; -- coordinate transformations -----------------------------------------------

; the notation for the transformations follows "paul, r.p. (1981) robot
; manipulators.  mit press." with the exception that our transformation
; matrices don't have the perspective terms and are the transpose of
; paul's one.  see also "m\"antyl\"a, m. (1985) an introduction to
; solid modeling, computer science press" appendix a.
;
; the components of a transformation matrix are named like this:
;
;  a  b  c
;  d  e  f
;  g  h  i
; tx ty tz
;
; the components tx, ty, and tz are the translation vector.

(def-struct #f tfo a b c d e f g h i tx ty tz)

(define tfo-id  ; the identity transformation matrix
  '#(1.0 0.0 0.0
     0.0 1.0 0.0
     0.0 0.0 1.0
     0.0 0.0 0.0))

; the function "tfo-apply" multiplies a transformation matrix, tfo, by a
; point vector, p.  the result is a new point.

(define (tfo-apply tfo p)
  (let ((x (pt-x p))
        (y (pt-y p))
        (z (pt-z p)))
    (make-pt
     (float+ (float* x (tfo-a tfo)) 
             (float* y (tfo-d tfo)) 
             (float* z (tfo-g tfo)) 
             (tfo-tx tfo))
     (float+ (float* x (tfo-b tfo)) 
             (float* y (tfo-e tfo))
             (float* z (tfo-h tfo))
             (tfo-ty tfo))
     (float+ (float* x (tfo-c tfo)) 
             (float* y (tfo-f tfo))
             (float* z (tfo-i tfo))
             (tfo-tz tfo)))))

; the function "tfo-combine" multiplies two transformation matrices a and b.
; the result is a new matrix which cumulates the transformations described
; by a and b.

(define (tfo-combine a b)
  (make-tfo
   (float+ (float* (tfo-a a) (tfo-a b))
           (float* (tfo-b a) (tfo-d b))
           (float* (tfo-c a) (tfo-g b)))
   (float+ (float* (tfo-a a) (tfo-b b))
           (float* (tfo-b a) (tfo-e b))
           (float* (tfo-c a) (tfo-h b)))
   (float+ (float* (tfo-a a) (tfo-c b))
           (float* (tfo-b a) (tfo-f b))
           (float* (tfo-c a) (tfo-i b)))
   (float+ (float* (tfo-d a) (tfo-a b))
           (float* (tfo-e a) (tfo-d b))
           (float* (tfo-f a) (tfo-g b)))
   (float+ (float* (tfo-d a) (tfo-b b))
           (float* (tfo-e a) (tfo-e b))
           (float* (tfo-f a) (tfo-h b)))
   (float+ (float* (tfo-d a) (tfo-c b))
           (float* (tfo-e a) (tfo-f b))
           (float* (tfo-f a) (tfo-i b)))
   (float+ (float* (tfo-g a) (tfo-a b))
           (float* (tfo-h a) (tfo-d b))
           (float* (tfo-i a) (tfo-g b)))
   (float+ (float* (tfo-g a) (tfo-b b))
           (float* (tfo-h a) (tfo-e b))
           (float* (tfo-i a) (tfo-h b)))
   (float+ (float* (tfo-g a) (tfo-c b))
           (float* (tfo-h a) (tfo-f b))
           (float* (tfo-i a) (tfo-i b)))
   (float+ (float* (tfo-tx a) (tfo-a b))
           (float* (tfo-ty a) (tfo-d b))
           (float* (tfo-tz a) (tfo-g b))
           (tfo-tx b))
   (float+ (float* (tfo-tx a) (tfo-b b))
           (float* (tfo-ty a) (tfo-e b))
           (float* (tfo-tz a) (tfo-h b))
           (tfo-ty b))
   (float+ (float* (tfo-tx a) (tfo-c b))
           (float* (tfo-ty a) (tfo-f b))
           (float* (tfo-tz a) (tfo-i b))
           (tfo-tz b))))

; the function "tfo-inv-ortho" computes the inverse of a homogeneous
; transformation matrix.

(define (tfo-inv-ortho tfo)
  (let* ((tx (tfo-tx tfo))
         (ty (tfo-ty tfo))
         (tz (tfo-tz tfo)))
    (make-tfo
     (tfo-a tfo) (tfo-d tfo) (tfo-g tfo)
     (tfo-b tfo) (tfo-e tfo) (tfo-h tfo)
     (tfo-c tfo) (tfo-f tfo) (tfo-i tfo)
     (float- (float+ (float* (tfo-a tfo) tx)
                     (float* (tfo-b tfo) ty)
                     (float* (tfo-c tfo) tz)))
     (float- (float+ (float* (tfo-d tfo) tx)
                     (float* (tfo-e tfo) ty)
                     (float* (tfo-f tfo) tz)))
     (float- (float+ (float* (tfo-g tfo) tx)
                     (float* (tfo-h tfo) ty)
                     (float* (tfo-i tfo) tz))))))

; given three points p1, p2, and p3, the function "tfo-align" computes
; a transformation matrix such that point p1 gets mapped to (0,0,0), p2 gets
; mapped to the y axis and p3 gets mapped to the yz plane.

(define (tfo-align p1 p2 p3)
  (let* ((x1 (pt-x p1))       (y1 (pt-y p1))       (z1 (pt-z p1))
         (x3 (pt-x p3))       (y3 (pt-y p3))       (z3 (pt-z p3))
         (x31 (float- x3 x1)) (y31 (float- y3 y1)) (z31 (float- z3 z1))
         (rotpy (pt-sub p2 p1))
         (phi (pt-phi rotpy))
         (theta (pt-theta rotpy))
         (sinp (floatsin phi))
         (sint (floatsin theta))
         (cosp (floatcos phi))
         (cost (floatcos theta))
         (sinpsint (float* sinp sint))
         (sinpcost (float* sinp cost))
         (cospsint (float* cosp sint))
         (cospcost (float* cosp cost))
         (rotpz 
          (make-pt 
           (float- (float* cost x31)
                   (float* sint z31))
           (float+ (float* sinpsint x31)
                   (float* cosp y31)
                   (float* sinpcost z31))
           (float+ (float* cospsint x31)
                   (float- (float* sinp y31))
                   (float* cospcost z31))))
         (rho (pt-theta rotpz))
         (cosr (floatcos rho))
         (sinr (floatsin rho))
         (x (float+ (float- (float* x1 cost))
                    (float* z1 sint)))
         (y (float- (float- (float- (float* x1 sinpsint))
                            (float* y1 cosp))
                    (float* z1 sinpcost)))
         (z (float- (float+ (float- (float* x1 cospsint))
                            (float* y1 sinp))
                    (float* z1 cospcost))))
    (make-tfo
     (float- (float* cost cosr) (float* cospsint sinr))
     sinpsint
     (float+ (float* cost sinr) (float* cospsint cosr))
     (float* sinp sinr)
     cosp
     (float- (float* sinp cosr))
     (float- (float- (float* sint cosr)) (float* cospcost sinr))
     sinpcost
     (float+ (float- (float* sint sinr)) (float* cospcost cosr))
     (float- (float* x cosr) (float* z sinr))
     y
     (float+ (float* x sinr) (float* z cosr)))))

; -- nucleic acid conformations data base -------------------------------------

; numbering of atoms follows the paper:
;
; iupac-iub joint commission on biochemical nomenclature (jcbn)
; (1983) abbreviations and symbols for the description of
; conformations of polynucleotide chains.  eur. j. biochem 131,
; 9-15.
;
; in the atom names, we have used "*" instead of "'".

; define part common to all 4 nucleotide types.

(def-struct #f nuc
  dgf-base-tfo  ; defines the standard position for wc and wc-dumas
  p-o3*-275-tfo ; defines the standard position for the connect function
  p-o3*-180-tfo
  p-o3*-60-tfo
  p o1p o2p o5* c5* h5* h5** c4* h4* o4* c1* h1* c2* h2** o2* h2* c3*
  h3* o3* n1 n3 c2 c4 c5 c6)

; define remaining atoms for each nucleotide type.

(def-nuc #t ra n6 n7 n9 c8 h2 h61 h62 h8)
(def-nuc #t rc n4 o2 h41 h42 h5 h6)
(def-nuc #t rg n2 n7 n9 c8 o6 h1 h21 h22 h8)
(def-nuc #t ru o2 o4 h3 h5 h6)

; database of nucleotide conformations:

(define ra
  (make-constant-ra
    #( -0.0018  -0.8207   0.5714  ; dgf-base-tfo
        0.2679  -0.5509  -0.7904
        0.9634   0.1517   0.2209
        0.0073   8.4030   0.6232)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  5.4550   8.2120  -2.8810) ; c5* 
    #(  5.4546   8.8508  -1.9978) ; h5* 
    #(  5.7588   8.6625  -3.8259) ; h5**
    #(  6.4970   7.1480  -2.5980) ; c4* 
    #(  7.4896   7.5919  -2.5214) ; h4* 
    #(  6.1630   6.4860  -1.3440) ; o4* 
    #(  6.5400   5.1200  -1.4190) ; c1* 
    #(  7.2763   4.9681  -0.6297) ; h1* 
    #(  7.1940   4.8830  -2.7770) ; c2* 
    #(  6.8667   3.9183  -3.1647) ; h2**
    #(  8.5860   5.0910  -2.6140) ; o2* 
    #(  8.9510   4.7626  -1.7890) ; h2* 
    #(  6.5720   6.0040  -3.6090) ; c3* 
    #(  5.5636   5.7066  -3.8966) ; h3* 
    #(  7.3801   6.3562  -4.7350) ; o3* 
    #(  4.7150   0.4910  -0.1360) ; n1  
    #(  6.3490   2.1730  -0.6020) ; n3  
    #(  5.9530   0.9650  -0.2670) ; c2  
    #(  5.2900   2.9790  -0.8260) ; c4  
    #(  3.9720   2.6390  -0.7330) ; c5  
    #(  3.6770   1.3160  -0.3660) ; c6  
    #(  2.4280   0.8450  -0.2360) ; n6  
    #(  3.1660   3.7290  -1.0360) ; n7  
    #(  5.3170   4.2990  -1.1930) ; n9  
    #(  4.0100   4.6780  -1.2990) ; c8  
    #(  6.6890   0.1903  -0.0518) ; h2  
    #(  1.6470   1.4460  -0.4040) ; h61 
    #(  2.2780  -0.1080  -0.0280) ; h62 
    #(  3.4421   5.5744  -1.5482) ; h8  
  ))

(define ra01
  (make-constant-ra
    #( -0.0043  -0.8175   0.5759  ; dgf-base-tfo
        0.2617  -0.5567  -0.7884
        0.9651   0.1473   0.2164
        0.0359   8.3929   0.5532)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  5.4352   8.2183  -2.7757) ; c5* 
    #(  5.3830   8.7883  -1.8481) ; h5* 
    #(  5.7729   8.7436  -3.6691) ; h5**
    #(  6.4830   7.1518  -2.5252) ; c4* 
    #(  7.4749   7.5972  -2.4482) ; h4* 
    #(  6.1626   6.4620  -1.2827) ; o4* 
    #(  6.5431   5.0992  -1.3905) ; c1* 
    #(  7.2871   4.9328  -0.6114) ; h1* 
    #(  7.1852   4.8935  -2.7592) ; c2* 
    #(  6.8573   3.9363  -3.1645) ; h2**
    #(  8.5780   5.1025  -2.6046) ; o2* 
    #(  8.9516   4.7577  -1.7902) ; h2* 
    #(  6.5522   6.0300  -3.5612) ; c3* 
    #(  5.5420   5.7356  -3.8459) ; h3* 
    #(  7.3487   6.4089  -4.6867) ; o3* 
    #(  4.7442   0.4514  -0.1390) ; n1  
    #(  6.3687   2.1459  -0.5926) ; n3  
    #(  5.9795   0.9335  -0.2657) ; c2  
    #(  5.3052   2.9471  -0.8125) ; c4  
    #(  3.9891   2.5987  -0.7230) ; c5  
    #(  3.7016   1.2717  -0.3647) ; c6  
    #(  2.4553   0.7925  -0.2390) ; n6  
    #(  3.1770   3.6859  -1.0198) ; n7  
    #(  5.3247   4.2695  -1.1710) ; n9  
    #(  4.0156   4.6415  -1.2759) ; c8  
    #(  6.7198   0.1618  -0.0547) ; h2  
    #(  1.6709   1.3900  -0.4039) ; h61 
    #(  2.3107  -0.1627  -0.0373) ; h62 
    #(  3.4426   5.5361  -1.5199) ; h8  
  ))

(define ra02
  (make-constant-ra
    #(  0.5566   0.0449   0.8296  ; dgf-base-tfo
        0.5125   0.7673  -0.3854
       -0.6538   0.6397   0.4041
       -9.1161  -3.7679  -2.9968)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.5778   6.6594  -4.0364) ; c5* 
    #(  4.9220   7.1963  -4.9204) ; h5* 
    #(  3.7996   5.9091  -4.1764) ; h5**
    #(  5.7873   5.8869  -3.5482) ; c4* 
    #(  6.0405   5.0875  -4.2446) ; h4* 
    #(  6.9135   6.8036  -3.4310) ; o4* 
    #(  7.7293   6.4084  -2.3392) ; c1* 
    #(  8.7078   6.1815  -2.7624) ; h1* 
    #(  7.1305   5.1418  -1.7347) ; c2* 
    #(  7.2040   5.1982  -0.6486) ; h2**
    #(  7.7417   4.0392  -2.3813) ; o2* 
    #(  8.6785   4.1443  -2.5630) ; h2* 
    #(  5.6666   5.2728  -2.1536) ; c3* 
    #(  5.1747   5.9805  -1.4863) ; h3* 
    #(  4.9997   4.0086  -2.1973) ; o3* 
    #( 10.3245   8.5459   1.5467) ; n1  
    #(  9.8051   6.9432  -0.1497) ; n3  
    #( 10.5175   7.4328   0.8408) ; c2  
    #(  8.7523   7.7422  -0.4228) ; c4  
    #(  8.4257   8.9060   0.2099) ; c5  
    #(  9.2665   9.3242   1.2540) ; c6  
    #(  9.0664  10.4462   1.9610) ; n6  
    #(  7.2750   9.4537  -0.3428) ; n7  
    #(  7.7962   7.5519  -1.3859) ; n9  
    #(  6.9479   8.6157  -1.2771) ; c8  
    #( 11.4063   6.9047   1.1859) ; h2  
    #(  8.2845  11.0341   1.7552) ; h61 
    #(  9.6584  10.6647   2.7198) ; h62 
    #(  6.0430   8.9853  -1.7594) ; h8  
  ))

(define ra03
  (make-constant-ra
    #( -0.5021   0.0731   0.8617  ; dgf-base-tfo
       -0.8112   0.3054  -0.4986
       -0.2996  -0.9494  -0.0940
        6.4273  -5.1944  -3.7807)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.1214   6.7116  -1.9049) ; c5* 
    #(  3.3465   5.9610  -2.0607) ; h5* 
    #(  4.0789   7.2928  -0.9837) ; h5**
    #(  5.4170   5.9293  -1.8186) ; c4* 
    #(  5.4506   5.3400  -0.9023) ; h4* 
    #(  5.5067   5.0417  -2.9703) ; o4* 
    #(  6.8650   4.9152  -3.3612) ; c1* 
    #(  7.1090   3.8577  -3.2603) ; h1* 
    #(  7.7152   5.7282  -2.3894) ; c2* 
    #(  8.5029   6.2356  -2.9463) ; h2**
    #(  8.1036   4.8568  -1.3419) ; o2* 
    #(  8.3270   3.9651  -1.6184) ; h2* 
    #(  6.7003   6.7565  -1.8911) ; c3* 
    #(  6.5898   7.5329  -2.6482) ; h3* 
    #(  7.0505   7.2878  -0.6105) ; o3* 
    #(  9.6740   4.7656  -7.6614) ; n1  
    #(  9.0739   4.3013  -5.3941) ; n3  
    #(  9.8416   4.2192  -6.4581) ; c2  
    #(  7.9885   5.0632  -5.6446) ; c4  
    #(  7.6822   5.6856  -6.8194) ; c5  
    #(  8.5831   5.5215  -7.8840) ; c6  
    #(  8.4084   6.0747  -9.0933) ; n6  
    #(  6.4857   6.3816  -6.7035) ; n7  
    #(  6.9740   5.3703  -4.7760) ; n9  
    #(  6.1133   6.1613  -5.4808) ; c8  
    #( 10.7627   3.6375  -6.4220) ; h2  
    #(  7.6031   6.6390  -9.2733) ; h61 
    #(  9.1004   5.9708  -9.7893) ; h62 
    #(  5.1705   6.6830  -5.3167) ; h8  
  ))

(define ra04
  (make-constant-ra
    #( -0.5426  -0.8175   0.1929  ; dgf-base-tfo
        0.8304  -0.5567  -0.0237
        0.1267   0.1473   0.9809
       -0.5075   8.3929   0.2229)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  5.4352   8.2183  -2.7757) ; c5* 
    #(  5.3830   8.7883  -1.8481) ; h5* 
    #(  5.7729   8.7436  -3.6691) ; h5**
    #(  6.4830   7.1518  -2.5252) ; c4* 
    #(  7.4749   7.5972  -2.4482) ; h4* 
    #(  6.1626   6.4620  -1.2827) ; o4* 
    #(  6.5431   5.0992  -1.3905) ; c1* 
    #(  7.2871   4.9328  -0.6114) ; h1* 
    #(  7.1852   4.8935  -2.7592) ; c2* 
    #(  6.8573   3.9363  -3.1645) ; h2**
    #(  8.5780   5.1025  -2.6046) ; o2* 
    #(  8.9516   4.7577  -1.7902) ; h2* 
    #(  6.5522   6.0300  -3.5612) ; c3* 
    #(  5.5420   5.7356  -3.8459) ; h3* 
    #(  7.3487   6.4089  -4.6867) ; o3* 
    #(  3.6343   2.6680   2.0783) ; n1  
    #(  5.4505   3.9805   1.2446) ; n3  
    #(  4.7540   3.3816   2.1851) ; c2  
    #(  4.8805   3.7951   0.0354) ; c4  
    #(  3.7416   3.0925  -0.2305) ; c5  
    #(  3.0873   2.4980   0.8606) ; c6  
    #(  1.9600   1.7805   0.7462) ; n6  
    #(  3.4605   3.1184  -1.5906) ; n7  
    #(  5.3247   4.2695  -1.1710) ; n9  
    #(  4.4244   3.8244  -2.0953) ; c8  
    #(  5.0814   3.4352   3.2234) ; h2  
    #(  1.5423   1.6454  -0.1520) ; h61 
    #(  1.5716   1.3398   1.5392) ; h62 
    #(  4.2675   3.8876  -3.1721) ; h8  
  ))

(define ra05
  (make-constant-ra
    #( -0.5891   0.0449   0.8068  ; dgf-base-tfo
        0.5375   0.7673   0.3498
       -0.6034   0.6397  -0.4762
       -0.3019  -3.7679  -9.5913)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.5778   6.6594  -4.0364) ; c5* 
    #(  4.9220   7.1963  -4.9204) ; h5* 
    #(  3.7996   5.9091  -4.1764) ; h5**
    #(  5.7873   5.8869  -3.5482) ; c4* 
    #(  6.0405   5.0875  -4.2446) ; h4* 
    #(  6.9135   6.8036  -3.4310) ; o4* 
    #(  7.7293   6.4084  -2.3392) ; c1* 
    #(  8.7078   6.1815  -2.7624) ; h1* 
    #(  7.1305   5.1418  -1.7347) ; c2* 
    #(  7.2040   5.1982  -0.6486) ; h2**
    #(  7.7417   4.0392  -2.3813) ; o2* 
    #(  8.6785   4.1443  -2.5630) ; h2* 
    #(  5.6666   5.2728  -2.1536) ; c3* 
    #(  5.1747   5.9805  -1.4863) ; h3* 
    #(  4.9997   4.0086  -2.1973) ; o3* 
    #( 10.2594  10.6774  -1.0056) ; n1  
    #(  9.7528   8.7080  -2.2631) ; n3  
    #( 10.4471   9.7876  -1.9791) ; c2  
    #(  8.7271   8.5575  -1.3991) ; c4  
    #(  8.4100   9.3803  -0.3580) ; c5  
    #(  9.2294  10.5030  -0.1574) ; c6  
    #(  9.0349  11.3951   0.8250) ; n6  
    #(  7.2891   8.9068   0.3121) ; n7  
    #(  7.7962   7.5519  -1.3859) ; n9  
    #(  6.9702   7.8292  -0.3353) ; c8  
    #( 11.3132  10.0537  -2.5851) ; h2  
    #(  8.2741  11.2784   1.4629) ; h61 
    #(  9.6733  12.1368   0.9529) ; h62 
    #(  6.0888   7.3990   0.1403) ; h8  
  ))

(define ra06
  (make-constant-ra
    #( -0.9815   0.0731  -0.1772  ; dgf-base-tfo
        0.1912   0.3054  -0.9328
       -0.0141  -0.9494  -0.3137
        5.7506  -5.1944   4.7470)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.1214   6.7116  -1.9049) ; c5* 
    #(  3.3465   5.9610  -2.0607) ; h5* 
    #(  4.0789   7.2928  -0.9837) ; h5**
    #(  5.4170   5.9293  -1.8186) ; c4* 
    #(  5.4506   5.3400  -0.9023) ; h4* 
    #(  5.5067   5.0417  -2.9703) ; o4* 
    #(  6.8650   4.9152  -3.3612) ; c1* 
    #(  7.1090   3.8577  -3.2603) ; h1* 
    #(  7.7152   5.7282  -2.3894) ; c2* 
    #(  8.5029   6.2356  -2.9463) ; h2**
    #(  8.1036   4.8568  -1.3419) ; o2* 
    #(  8.3270   3.9651  -1.6184) ; h2* 
    #(  6.7003   6.7565  -1.8911) ; c3* 
    #(  6.5898   7.5329  -2.6482) ; h3* 
    #(  7.0505   7.2878  -0.6105) ; o3* 
    #(  6.6624   3.5061  -8.2986) ; n1  
    #(  6.5810   3.2570  -5.9221) ; n3  
    #(  6.5151   2.8263  -7.1625) ; c2  
    #(  6.8364   4.5817  -5.8882) ; c4  
    #(  7.0116   5.4064  -6.9609) ; c5  
    #(  6.9173   4.8260  -8.2361) ; c6  
    #(  7.0668   5.5163  -9.3763) ; n6  
    #(  7.2573   6.7070  -6.5394) ; n7  
    #(  6.9740   5.3703  -4.7760) ; n9  
    #(  7.2238   6.6275  -5.2453) ; c8  
    #(  6.3146   1.7741  -7.3641) ; h2  
    #(  7.2568   6.4972  -9.3456) ; h61 
    #(  7.0437   5.0478 -10.2446) ; h62 
    #(  7.4108   7.6227  -4.8418) ; h8  
  ))

(define ra07
  (make-constant-ra
    #(  0.2379   0.1310  -0.9624  ; dgf-base-tfo
       -0.5876  -0.7696  -0.2499
       -0.7734   0.6249  -0.1061
       30.9870 -26.9344  42.6416)
    #(  0.7529   0.1548   0.6397  ; p-o3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; p-o3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; p-o3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; p   
    #( 42.5400   8.0450  44.8330) ; o1p 
    #( 42.2470   9.6920  42.9910) ; o2p 
    #( 40.2550   8.2030  43.7340) ; o5* 
    #( 39.3505   8.4697  42.6565) ; c5* 
    #( 39.1377   7.5433  42.1230) ; h5* 
    #( 39.7203   9.3119  42.0717) ; h5**
    #( 38.0405   8.9195  43.2869) ; c4* 
    #( 37.3687   9.3036  42.5193) ; h4* 
    #( 37.4319   7.8146  43.9387) ; o4* 
    #( 37.1959   8.1354  45.3237) ; c1* 
    #( 36.1788   8.5202  45.3970) ; h1* 
    #( 38.1721   9.2328  45.6504) ; c2* 
    #( 39.1555   8.7939  45.8188) ; h2**
    #( 37.7862  10.0617  46.7013) ; o2* 
    #( 37.3087   9.6229  47.4092) ; h2* 
    #( 38.1844  10.0268  44.3367) ; c3* 
    #( 39.1578  10.5054  44.2289) ; h3* 
    #( 37.0547  10.9127  44.3441) ; o3* 
    #( 34.8811   4.2072  47.5784) ; n1  
    #( 35.1084   6.1336  46.1818) ; n3  
    #( 34.4108   5.1360  46.7207) ; c2  
    #( 36.3908   6.1224  46.6053) ; c4  
    #( 36.9819   5.2334  47.4697) ; c5  
    #( 36.1786   4.1985  48.0035) ; c6  
    #( 36.6103   3.2749  48.8452) ; n6  
    #( 38.3236   5.5522  47.6595) ; n7  
    #( 37.3887   7.0024  46.2437) ; n9  
    #( 38.5055   6.6096  46.9057) ; c8  
    #( 33.3553   5.0152  46.4771) ; h2  
    #( 37.5730   3.2804  49.1507) ; h61 
    #( 35.9775   2.5638  49.1828) ; h62 
    #( 39.5461   6.9184  47.0041) ; h8  
  ))

(define ra08
  (make-constant-ra
    #(  0.1084  -0.0895  -0.9901  ; dgf-base-tfo
        0.9789  -0.1638   0.1220
       -0.1731  -0.9824   0.0698
       -2.9039  47.2655  33.0094)
    #(  0.7529   0.1548   0.6397  ; p-o3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; p-o3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; p-o3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; p   
    #( 42.5400   8.0450  44.8330) ; o1p 
    #( 42.2470   9.6920  42.9910) ; o2p 
    #( 40.2550   8.2030  43.7340) ; o5* 
    #( 39.4850   8.9301  44.6977) ; c5* 
    #( 39.0638   9.8199  44.2296) ; h5* 
    #( 40.0757   9.0713  45.6029) ; h5**
    #( 38.3102   8.0414  45.0789) ; c4* 
    #( 37.7842   8.4637  45.9351) ; h4* 
    #( 37.4200   7.9453  43.9769) ; o4* 
    #( 37.2249   6.5609  43.6273) ; c1* 
    #( 36.3360   6.2168  44.1561) ; h1* 
    #( 38.4347   5.8414  44.1590) ; c2* 
    #( 39.2688   5.9974  43.4749) ; h2**
    #( 38.2344   4.4907  44.4348) ; o2* 
    #( 37.6374   4.0386  43.8341) ; h2* 
    #( 38.6926   6.6079  45.4637) ; c3* 
    #( 39.7585   6.5640  45.6877) ; h3* 
    #( 37.8238   6.0705  46.4723) ; o3* 
    #( 33.9162   6.2598  39.7758) ; n1  
    #( 34.6709   6.5759  42.0215) ; n3  
    #( 33.7257   6.5186  41.0858) ; c2  
    #( 35.8935   6.3324  41.5018) ; c4  
    #( 36.2105   6.0601  40.1932) ; c5  
    #( 35.1538   6.0151  39.2537) ; c6  
    #( 35.3088   5.7642  37.9649) ; n6  
    #( 37.5818   5.8677  40.0507) ; n7  
    #( 37.0932   6.3197  42.1810) ; n9  
    #( 38.0509   6.0354  41.2635) ; c8  
    #( 32.6830   6.6898  41.3532) ; h2  
    #( 36.2305   5.5855  37.5925) ; h61 
    #( 34.5056   5.7512  37.3528) ; h62 
    #( 39.1318   5.8993  41.2285) ; h8  
  ))

(define ra09
  (make-constant-ra
    #(  0.8467   0.4166  -0.3311  ; dgf-base-tfo
       -0.3962   0.9089   0.1303
        0.3552   0.0209   0.9346
      -42.7319 -26.6223 -29.8163)
    #(  0.7529   0.1548   0.6397  ; p-o3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; p-o3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; p-o3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; p   
    #( 42.5400   8.0450  44.8330) ; o1p 
    #( 42.2470   9.6920  42.9910) ; o2p 
    #( 40.2550   8.2030  43.7340) ; o5* 
    #( 39.3505   8.4697  42.6565) ; c5* 
    #( 39.1377   7.5433  42.1230) ; h5* 
    #( 39.7203   9.3119  42.0717) ; h5**
    #( 38.0405   8.9195  43.2869) ; c4* 
    #( 37.6479   8.1347  43.9335) ; h4* 
    #( 38.2691  10.0933  44.0524) ; o4* 
    #( 37.3999  11.1488  43.5973) ; c1* 
    #( 36.5061  11.1221  44.2206) ; h1* 
    #( 37.0364  10.7838  42.1836) ; c2* 
    #( 37.8636  11.0489  41.5252) ; h2**
    #( 35.8275  11.3133  41.7379) ; o2* 
    #( 35.6214  12.1896  42.0714) ; h2* 
    #( 36.9316   9.2556  42.2837) ; c3* 
    #( 37.1778   8.8260  41.3127) ; h3* 
    #( 35.6285   8.9334  42.7926) ; o3* 
    #( 38.1482  15.2833  46.4641) ; n1  
    #( 37.3641  13.0968  45.9007) ; n3  
    #( 37.5032  14.1288  46.7300) ; c2  
    #( 37.9570  13.3377  44.7113) ; c4  
    #( 38.6397  14.4660  44.3267) ; c5  
    #( 38.7473  15.5229  45.2609) ; c6  
    #( 39.3720  16.6649  45.0297) ; n6  
    #( 39.1079  14.3351  43.0223) ; n7  
    #( 38.0132  12.4868  43.6280) ; n9  
    #( 38.7058  13.1402  42.6620) ; c8  
    #( 37.0731  14.0857  47.7306) ; h2  
    #( 39.8113  16.8281  44.1350) ; h61 
    #( 39.4100  17.3741  45.7478) ; h62 
    #( 39.0412  12.9660  41.6397) ; h8  
  ))

(define ra10
  (make-constant-ra
    #(  0.7063   0.6317  -0.3196  ; dgf-base-tfo
       -0.0403  -0.4149  -0.9090
       -0.7068   0.6549  -0.2676
        6.4402 -52.1496  30.8246)
    #(  0.7529   0.1548   0.6397  ; p-o3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; p-o3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; p-o3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; p   
    #( 42.5400   8.0450  44.8330) ; o1p 
    #( 42.2470   9.6920  42.9910) ; o2p 
    #( 40.2550   8.2030  43.7340) ; o5* 
    #( 39.4850   8.9301  44.6977) ; c5* 
    #( 39.0638   9.8199  44.2296) ; h5* 
    #( 40.0757   9.0713  45.6029) ; h5**
    #( 38.3102   8.0414  45.0789) ; c4* 
    #( 37.7099   7.8166  44.1973) ; h4* 
    #( 38.8012   6.8321  45.6380) ; o4* 
    #( 38.2431   6.6413  46.9529) ; c1* 
    #( 37.3505   6.0262  46.8385) ; h1* 
    #( 37.8484   8.0156  47.4214) ; c2* 
    #( 38.7381   8.5406  47.7690) ; h2**
    #( 36.8286   8.0368  48.3701) ; o2* 
    #( 36.8392   7.3063  48.9929) ; h2* 
    #( 37.3576   8.6512  46.1132) ; c3* 
    #( 37.5207   9.7275  46.1671) ; h3* 
    #( 35.9985   8.2392  45.9032) ; o3* 
    #( 39.9117   2.2278  48.8527) ; n1  
    #( 38.6207   3.6941  47.4757) ; n3  
    #( 38.9872   2.4888  47.9057) ; c2  
    #( 39.2961   4.6720  48.1174) ; c4  
    #( 40.2546   4.5307  49.0912) ; c5  
    #( 40.5932   3.2189  49.4985) ; c6  
    #( 41.4938   2.9317  50.4229) ; n6  
    #( 40.7195   5.7755  49.5060) ; n7  
    #( 39.1730   6.0305  47.9170) ; n9  
    #( 40.0413   6.6250  48.7728) ; c8  
    #( 38.5257   1.5960  47.4838) ; h2  
    #( 41.9907   3.6753  50.8921) ; h61 
    #( 41.6848   1.9687  50.6599) ; h62 
    #( 40.3571   7.6321  49.0452) ; h8  
  ))

(define ras 
  (list ra01 ra02 ra03 ra04 ra05 ra06 ra07 ra08 ra09 ra10))

(define rc
  (make-constant-rc
    #( -0.0359  -0.8071   0.5894  ; dgf-base-tfo
       -0.2669   0.5761   0.7726
       -0.9631  -0.1296  -0.2361
        0.1584   8.3434   0.5434)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2430  -8.2420   2.8260) ; c5* 
    #(  5.1974  -8.8497   1.9223) ; h5* 
    #(  5.5548  -8.7348   3.7469) ; h5**
    #(  6.3140  -7.2060   2.5510) ; c4* 
    #(  7.2954  -7.6762   2.4898) ; h4* 
    #(  6.0140  -6.5420   1.2890) ; o4* 
    #(  6.4190  -5.1840   1.3620) ; c1* 
    #(  7.1608  -5.0495   0.5747) ; h1* 
    #(  7.0760  -4.9560   2.7270) ; c2* 
    #(  6.7770  -3.9803   3.1099) ; h2**
    #(  8.4500  -5.1930   2.5810) ; o2* 
    #(  8.8309  -4.8755   1.7590) ; h2* 
    #(  6.4060  -6.0590   3.5580) ; c3* 
    #(  5.4021  -5.7313   3.8281) ; h3* 
    #(  7.1570  -6.4240   4.7070) ; o3* 
    #(  5.2170  -4.3260   1.1690) ; n1  
    #(  4.2960  -2.2560   0.6290) ; n3  
    #(  5.4330  -3.0200   0.7990) ; c2  
    #(  2.9930  -2.6780   0.7940) ; c4  
    #(  2.8670  -4.0630   1.1830) ; c5  
    #(  3.9570  -4.8300   1.3550) ; c6  
    #(  2.0187  -1.8047   0.5874) ; n4  
    #(  6.5470  -2.5560   0.6290) ; o2  
    #(  1.0684  -2.1236   0.7109) ; h41 
    #(  2.2344  -0.8560   0.3162) ; h42 
    #(  1.8797  -4.4972   1.3404) ; h5  
    #(  3.8479  -5.8742   1.6480) ; h6  
  ))

(define rc01
  (make-constant-rc
    #( -0.0137  -0.8012   0.5983  ; dgf-base-tfo
       -0.2523   0.5817   0.7733
       -0.9675  -0.1404  -0.2101
        0.2031   8.3874   0.4228)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2416  -8.2422   2.8181) ; c5* 
    #(  5.2050  -8.8128   1.8901) ; h5* 
    #(  5.5368  -8.7738   3.7227) ; h5**
    #(  6.3232  -7.2037   2.6002) ; c4* 
    #(  7.3048  -7.6757   2.5577) ; h4* 
    #(  6.0635  -6.5092   1.3456) ; o4* 
    #(  6.4697  -5.1547   1.4629) ; c1* 
    #(  7.2354  -5.0043   0.7018) ; h1* 
    #(  7.0856  -4.9610   2.8521) ; c2* 
    #(  6.7777  -3.9935   3.2487) ; h2**
    #(  8.4627  -5.1992   2.7423) ; o2* 
    #(  8.8693  -4.8638   1.9399) ; h2* 
    #(  6.3877  -6.0809   3.6362) ; c3* 
    #(  5.3770  -5.7562   3.8834) ; h3* 
    #(  7.1024  -6.4754   4.7985) ; o3* 
    #(  5.2764  -4.2883   1.2538) ; n1  
    #(  4.3777  -2.2062   0.7229) ; n3  
    #(  5.5069  -2.9779   0.9088) ; c2  
    #(  3.0693  -2.6246   0.8500) ; c4  
    #(  2.9279  -4.0146   1.2149) ; c5  
    #(  4.0101  -4.7892   1.4017) ; c6  
    #(  2.1040  -1.7437   0.6331) ; n4  
    #(  6.6267  -2.5166   0.7728) ; o2  
    #(  1.1496  -2.0600   0.7287) ; h41 
    #(  2.3303  -0.7921   0.3815) ; h42 
    #(  1.9353  -4.4465   1.3419) ; h5  
    #(  3.8895  -5.8371   1.6762) ; h6  
  ))

(define rc02
  (make-constant-rc
    #(  0.5141   0.0246   0.8574  ; dgf-base-tfo
       -0.5547  -0.7529   0.3542
        0.6542  -0.6577  -0.3734
       -9.1111  -3.4598  -3.2939)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  4.3825  -6.6585   4.0489) ; c5* 
    #(  4.6841  -7.2019   4.9443) ; h5* 
    #(  3.6189  -5.8889   4.1625) ; h5**
    #(  5.6255  -5.9175   3.5998) ; c4* 
    #(  5.8732  -5.1228   4.3034) ; h4* 
    #(  6.7337  -6.8605   3.5222) ; o4* 
    #(  7.5932  -6.4923   2.4548) ; c1* 
    #(  8.5661  -6.2983   2.9064) ; h1* 
    #(  7.0527  -5.2012   1.8322) ; c2* 
    #(  7.1627  -5.2525   0.7490) ; h2**
    #(  7.6666  -4.1249   2.4880) ; o2* 
    #(  8.5944  -4.2543   2.6981) ; h2* 
    #(  5.5661  -5.3029   2.2009) ; c3* 
    #(  5.0841  -6.0018   1.5172) ; h3* 
    #(  4.9062  -4.0452   2.2042) ; o3* 
    #(  7.6298  -7.6136   1.4752) ; n1  
    #(  8.6945  -8.7046  -0.2857) ; n3  
    #(  8.6943  -7.6514   0.6066) ; c2  
    #(  7.7426  -9.6987  -0.3801) ; c4  
    #(  6.6642  -9.5742   0.5722) ; c5  
    #(  6.6391  -8.5592   1.4526) ; c6  
    #(  7.9033 -10.6371  -1.3010) ; n4  
    #(  9.5840  -6.8186   0.6136) ; o2  
    #(  7.2009 -11.3604  -1.3619) ; h41 
    #(  8.7058 -10.6168  -1.9140) ; h42 
    #(  5.8585 -10.3083   0.5822) ; h5  
    #(  5.8197  -8.4773   2.1667) ; h6  
  ))

(define rc03
  (make-constant-rc
    #( -0.4993   0.0476   0.8651  ; dgf-base-tfo
        0.8078  -0.3353   0.4847
        0.3132   0.9409   0.1290
        6.2989  -5.2303  -3.8577)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  3.9938  -6.7042   1.9023) ; c5* 
    #(  3.2332  -5.9343   2.0319) ; h5* 
    #(  3.9666  -7.2863   0.9812) ; h5**
    #(  5.3098  -5.9546   1.8564) ; c4* 
    #(  5.3863  -5.3702   0.9395) ; h4* 
    #(  5.3851  -5.0642   3.0076) ; o4* 
    #(  6.7315  -4.9724   3.4462) ; c1* 
    #(  7.0033  -3.9202   3.3619) ; h1* 
    #(  7.5997  -5.8018   2.4948) ; c2* 
    #(  8.3627  -6.3254   3.0707) ; h2**
    #(  8.0410  -4.9501   1.4724) ; o2* 
    #(  8.2781  -4.0644   1.7570) ; h2* 
    #(  6.5701  -6.8129   1.9714) ; c3* 
    #(  6.4186  -7.5809   2.7299) ; h3* 
    #(  6.9357  -7.3841   0.7235) ; o3* 
    #(  6.8024  -5.4718   4.8475) ; n1  
    #(  7.9218  -5.5700   6.8877) ; n3  
    #(  7.8908  -5.0886   5.5944) ; c2  
    #(  6.9789  -6.3827   7.4823) ; c4  
    #(  5.8742  -6.7319   6.6202) ; c5  
    #(  5.8182  -6.2769   5.3570) ; c6  
    #(  7.1702  -6.7511   8.7402) ; n4  
    #(  8.7747  -4.3728   5.1568) ; o2  
    #(  6.4741  -7.3461   9.1662) ; h41 
    #(  7.9889  -6.4396   9.2429) ; h42 
    #(  5.0736  -7.3713   6.9922) ; h5  
    #(  4.9784  -6.5473   4.7170) ; h6  
  ))

(define rc04
  (make-constant-rc
    #( -0.5669  -0.8012   0.1918  ; dgf-base-tfo
       -0.8129   0.5817   0.0273
       -0.1334  -0.1404  -0.9811
       -0.3279   8.3874   0.3355)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2416  -8.2422   2.8181) ; c5* 
    #(  5.2050  -8.8128   1.8901) ; h5* 
    #(  5.5368  -8.7738   3.7227) ; h5**
    #(  6.3232  -7.2037   2.6002) ; c4* 
    #(  7.3048  -7.6757   2.5577) ; h4* 
    #(  6.0635  -6.5092   1.3456) ; o4* 
    #(  6.4697  -5.1547   1.4629) ; c1* 
    #(  7.2354  -5.0043   0.7018) ; h1* 
    #(  7.0856  -4.9610   2.8521) ; c2* 
    #(  6.7777  -3.9935   3.2487) ; h2**
    #(  8.4627  -5.1992   2.7423) ; o2* 
    #(  8.8693  -4.8638   1.9399) ; h2* 
    #(  6.3877  -6.0809   3.6362) ; c3* 
    #(  5.3770  -5.7562   3.8834) ; h3* 
    #(  7.1024  -6.4754   4.7985) ; o3* 
    #(  5.2764  -4.2883   1.2538) ; n1  
    #(  3.8961  -3.0896  -0.1893) ; n3  
    #(  5.0095  -3.8907  -0.0346) ; c2  
    #(  3.0480  -2.6632   0.8116) ; c4  
    #(  3.4093  -3.1310   2.1292) ; c5  
    #(  4.4878  -3.9124   2.3088) ; c6  
    #(  2.0216  -1.8941   0.4804) ; n4  
    #(  5.7005  -4.2164  -0.9842) ; o2  
    #(  1.4067  -1.5873   1.2205) ; h41 
    #(  1.8721  -1.6319  -0.4835) ; h42 
    #(  2.8048  -2.8507   2.9918) ; h5  
    #(  4.7491  -4.2593   3.3085) ; h6  
  ))

(define rc05
  (make-constant-rc
    #( -0.6298   0.0246   0.7763  ; dgf-base-tfo
       -0.5226  -0.7529  -0.4001
        0.5746  -0.6577   0.4870
       -0.0208  -3.4598  -9.6882)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  4.3825  -6.6585   4.0489) ; c5* 
    #(  4.6841  -7.2019   4.9443) ; h5* 
    #(  3.6189  -5.8889   4.1625) ; h5**
    #(  5.6255  -5.9175   3.5998) ; c4* 
    #(  5.8732  -5.1228   4.3034) ; h4* 
    #(  6.7337  -6.8605   3.5222) ; o4* 
    #(  7.5932  -6.4923   2.4548) ; c1* 
    #(  8.5661  -6.2983   2.9064) ; h1* 
    #(  7.0527  -5.2012   1.8322) ; c2* 
    #(  7.1627  -5.2525   0.7490) ; h2**
    #(  7.6666  -4.1249   2.4880) ; o2* 
    #(  8.5944  -4.2543   2.6981) ; h2* 
    #(  5.5661  -5.3029   2.2009) ; c3* 
    #(  5.0841  -6.0018   1.5172) ; h3* 
    #(  4.9062  -4.0452   2.2042) ; o3* 
    #(  7.6298  -7.6136   1.4752) ; n1  
    #(  8.5977  -9.5977   0.7329) ; n3  
    #(  8.5951  -8.5745   1.6594) ; c2  
    #(  7.7372  -9.7371  -0.3364) ; c4  
    #(  6.7596  -8.6801  -0.4476) ; c5  
    #(  6.7338  -7.6721   0.4408) ; c6  
    #(  7.8849 -10.7881  -1.1289) ; n4  
    #(  9.3993  -8.5377   2.5743) ; o2  
    #(  7.2499 -10.8809  -1.9088) ; h41 
    #(  8.6122 -11.4649  -0.9468) ; h42 
    #(  6.0317  -8.6941  -1.2588) ; h5  
    #(  5.9901  -6.8809   0.3459) ; h6  
  ))

(define rc06
  (make-constant-rc
    #( -0.9837   0.0476  -0.1733  ; dgf-base-tfo
       -0.1792  -0.3353   0.9249
       -0.0141   0.9409   0.3384
        5.7793  -5.2303   4.5997)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  3.9938  -6.7042   1.9023) ; c5* 
    #(  3.2332  -5.9343   2.0319) ; h5* 
    #(  3.9666  -7.2863   0.9812) ; h5**
    #(  5.3098  -5.9546   1.8564) ; c4* 
    #(  5.3863  -5.3702   0.9395) ; h4* 
    #(  5.3851  -5.0642   3.0076) ; o4* 
    #(  6.7315  -4.9724   3.4462) ; c1* 
    #(  7.0033  -3.9202   3.3619) ; h1* 
    #(  7.5997  -5.8018   2.4948) ; c2* 
    #(  8.3627  -6.3254   3.0707) ; h2**
    #(  8.0410  -4.9501   1.4724) ; o2* 
    #(  8.2781  -4.0644   1.7570) ; h2* 
    #(  6.5701  -6.8129   1.9714) ; c3* 
    #(  6.4186  -7.5809   2.7299) ; h3* 
    #(  6.9357  -7.3841   0.7235) ; o3* 
    #(  6.8024  -5.4718   4.8475) ; n1  
    #(  6.6920  -5.0495   7.1354) ; n3  
    #(  6.6201  -4.5500   5.8506) ; c2  
    #(  6.9254  -6.3614   7.4926) ; c4  
    #(  7.1046  -7.2543   6.3718) ; c5  
    #(  7.0391  -6.7951   5.1106) ; c6  
    #(  6.9614  -6.6648   8.7815) ; n4  
    #(  6.4083  -3.3696   5.6340) ; o2  
    #(  7.1329  -7.6280   9.0324) ; h41 
    #(  6.8204  -5.9469   9.4777) ; h42 
    #(  7.2954  -8.3135   6.5440) ; h5  
    #(  7.1753  -7.4798   4.2735) ; h6  
  ))

(define rc07
  (make-constant-rc
    #(  0.0033   0.2720  -0.9623  ; dgf-base-tfo
        0.3013  -0.9179  -0.2584
       -0.9535  -0.2891  -0.0850
       43.0403  13.7233  34.5710)
    #(  0.9187   0.2887   0.2694  ; p-o3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; p-o3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; p-o3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; p   
    #( 34.5130  10.2320  46.4660) ; o1p 
    #( 33.4130  12.3960  46.9340) ; o2p 
    #( 31.9810  10.3390  46.4820) ; o5* 
    #( 30.8152  11.1619  46.2003) ; c5* 
    #( 30.4519  10.9454  45.1957) ; h5* 
    #( 31.0379  12.2016  46.4400) ; h5**
    #( 29.7081  10.7448  47.1428) ; c4* 
    #( 28.8710  11.4416  47.0982) ; h4* 
    #( 29.2550   9.4394  46.8162) ; o4* 
    #( 29.3907   8.5625  47.9460) ; c1* 
    #( 28.4416   8.5669  48.4819) ; h1* 
    #( 30.4468   9.2031  48.7952) ; c2* 
    #( 31.4222   8.9651  48.3709) ; h2**
    #( 30.3701   8.9157  50.1624) ; o2* 
    #( 30.0652   8.0304  50.3740) ; h2* 
    #( 30.1622  10.6879  48.6120) ; c3* 
    #( 31.0952  11.2399  48.7254) ; h3* 
    #( 29.1076  11.1535  49.4702) ; o3* 
    #( 29.7883   7.2209  47.5235) ; n1  
    #( 29.1825   5.0438  46.8275) ; n3  
    #( 28.8008   6.2912  47.2263) ; c2  
    #( 30.4888   4.6890  46.7186) ; c4  
    #( 31.5034   5.6405  47.0249) ; c5  
    #( 31.1091   6.8691  47.4156) ; c6  
    #( 30.8109   3.4584  46.3336) ; n4  
    #( 27.6171   6.5989  47.3189) ; o2  
    #( 31.7923   3.2301  46.2638) ; h41 
    #( 30.0880   2.7857  46.1215) ; h42 
    #( 32.5542   5.3634  46.9395) ; h5  
    #( 31.8523   7.6279  47.6603) ; h6  
  ))

(define rc08
  (make-constant-rc
    #(  0.0797  -0.6026  -0.7941  ; dgf-base-tfo
        0.7939   0.5201  -0.3150
        0.6028  -0.6054   0.5198
      -36.8341  41.5293   1.6628)
    #(  0.9187   0.2887   0.2694  ; p-o3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; p-o3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; p-o3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; p   
    #( 34.5130  10.2320  46.4660) ; o1p 
    #( 33.4130  12.3960  46.9340) ; o2p 
    #( 31.9810  10.3390  46.4820) ; o5* 
    #( 31.8779   9.9369  47.8760) ; c5* 
    #( 31.3239  10.6931  48.4322) ; h5* 
    #( 32.8647   9.6624  48.2489) ; h5**
    #( 31.0429   8.6773  47.9401) ; c4* 
    #( 31.0779   8.2331  48.9349) ; h4* 
    #( 29.6956   8.9669  47.5983) ; o4* 
    #( 29.2784   8.1700  46.4782) ; c1* 
    #( 28.8006   7.2731  46.8722) ; h1* 
    #( 30.5544   7.7940  45.7875) ; c2* 
    #( 30.8837   8.6410  45.1856) ; h2**
    #( 30.5100   6.6007  45.0582) ; o2* 
    #( 29.6694   6.4168  44.6326) ; h2* 
    #( 31.5146   7.5954  46.9527) ; c3* 
    #( 32.5255   7.8261  46.6166) ; h3* 
    #( 31.3876   6.2951  47.5516) ; o3* 
    #( 28.3976   8.9302  45.5933) ; n1  
    #( 26.2155   9.6135  44.9910) ; n3  
    #( 27.0281   8.8961  45.8192) ; c2  
    #( 26.7044  10.3489  43.9595) ; c4  
    #( 28.1088  10.3837  43.7247) ; c5  
    #( 28.8978   9.6708  44.5535) ; c6  
    #( 25.8715  11.0249  43.1749) ; n4  
    #( 26.5733   8.2371  46.7484) ; o2  
    #( 26.2707  11.5609  42.4177) ; h41 
    #( 24.8760  10.9939  43.3427) ; h42 
    #( 28.5089  10.9722  42.8990) ; h5  
    #( 29.9782   9.6687  44.4097) ; h6  
  ))

(define rc09
  (make-constant-rc
    #(  0.8727   0.4760  -0.1091  ; dgf-base-tfo
       -0.4188   0.6148  -0.6682
       -0.2510   0.6289   0.7359
       -8.1687 -52.0761 -25.0726)
    #(  0.9187   0.2887   0.2694  ; p-o3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; p-o3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; p-o3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; p   
    #( 34.5130  10.2320  46.4660) ; o1p 
    #( 33.4130  12.3960  46.9340) ; o2p 
    #( 31.9810  10.3390  46.4820) ; o5* 
    #( 30.8152  11.1619  46.2003) ; c5* 
    #( 30.4519  10.9454  45.1957) ; h5* 
    #( 31.0379  12.2016  46.4400) ; h5**
    #( 29.7081  10.7448  47.1428) ; c4* 
    #( 29.4506   9.6945  47.0059) ; h4* 
    #( 30.1045  10.9634  48.4885) ; o4* 
    #( 29.1794  11.8418  49.1490) ; c1* 
    #( 28.4388  11.2210  49.6533) ; h1* 
    #( 28.5211  12.6008  48.0367) ; c2* 
    #( 29.1947  13.3949  47.7147) ; h2**
    #( 27.2316  13.0683  48.3134) ; o2* 
    #( 27.0851  13.3391  49.2227) ; h2* 
    #( 28.4131  11.5507  46.9391) ; c3* 
    #( 28.4451  12.0512  45.9713) ; h3* 
    #( 27.2707  10.6955  47.1097) ; o3* 
    #( 29.8751  12.7405  50.0682) ; n1  
    #( 30.7172  13.1841  52.2328) ; n3  
    #( 30.0617  12.3404  51.3847) ; c2  
    #( 31.1834  14.3941  51.8297) ; c4  
    #( 30.9913  14.8074  50.4803) ; c5  
    #( 30.3434  13.9610  49.6548) ; c6  
    #( 31.8090  15.1847  52.6957) ; n4  
    #( 29.6470  11.2494  51.7616) ; o2  
    #( 32.1422  16.0774  52.3606) ; h41 
    #( 31.9392  14.8893  53.6527) ; h42 
    #( 31.3632  15.7771  50.1491) ; h5  
    #( 30.1742  14.2374  48.6141) ; h6  
  ))

(define rc10
  (make-constant-rc
    #(  0.1549   0.8710  -0.4663  ; dgf-base-tfo
        0.6768  -0.4374  -0.5921
       -0.7197  -0.2239  -0.6572
       25.2447 -14.1920  50.3201)
    #(  0.9187   0.2887   0.2694  ; p-o3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; p-o3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; p-o3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; p   
    #( 34.5130  10.2320  46.4660) ; o1p 
    #( 33.4130  12.3960  46.9340) ; o2p 
    #( 31.9810  10.3390  46.4820) ; o5* 
    #( 31.8779   9.9369  47.8760) ; c5* 
    #( 31.3239  10.6931  48.4322) ; h5* 
    #( 32.8647   9.6624  48.2489) ; h5**
    #( 31.0429   8.6773  47.9401) ; c4* 
    #( 30.0440   8.8473  47.5383) ; h4* 
    #( 31.6749   7.6351  47.2119) ; o4* 
    #( 31.9159   6.5022  48.0616) ; c1* 
    #( 31.0691   5.8243  47.9544) ; h1* 
    #( 31.9300   7.0685  49.4493) ; c2* 
    #( 32.9024   7.5288  49.6245) ; h2**
    #( 31.5672   6.1750  50.4632) ; o2* 
    #( 31.8416   5.2663  50.3200) ; h2* 
    #( 30.8618   8.1514  49.3749) ; c3* 
    #( 31.1122   8.9396  50.0850) ; h3* 
    #( 29.5351   7.6245  49.5409) ; o3* 
    #( 33.1890   5.8629  47.7343) ; n1  
    #( 34.4004   4.2636  46.4828) ; n3  
    #( 33.2062   4.8497  46.7851) ; c2  
    #( 35.5600   4.6374  47.0822) ; c4  
    #( 35.5444   5.6751  48.0577) ; c5  
    #( 34.3565   6.2450  48.3432) ; c6  
    #( 36.6977   4.0305  46.7598) ; n4  
    #( 32.1661   4.5034  46.2348) ; o2  
    #( 37.5405   4.3347  47.2259) ; h41 
    #( 36.7033   3.2923  46.0706) ; h42 
    #( 36.4713   5.9811  48.5428) ; h5  
    #( 34.2986   7.0426  49.0839) ; h6  
  ))

(define rcs 
  (list rc01 rc02 rc03 rc04 rc05 rc06 rc07 rc08 rc09 rc10))

(define rg
  (make-constant-rg
    #( -0.0018  -0.8207   0.5714  ; dgf-base-tfo
        0.2679  -0.5509  -0.7904
        0.9634   0.1517   0.2209
        0.0073   8.4030   0.6232)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  5.4550   8.2120  -2.8810) ; c5* 
    #(  5.4546   8.8508  -1.9978) ; h5* 
    #(  5.7588   8.6625  -3.8259) ; h5**
    #(  6.4970   7.1480  -2.5980) ; c4* 
    #(  7.4896   7.5919  -2.5214) ; h4* 
    #(  6.1630   6.4860  -1.3440) ; o4* 
    #(  6.5400   5.1200  -1.4190) ; c1* 
    #(  7.2763   4.9681  -0.6297) ; h1* 
    #(  7.1940   4.8830  -2.7770) ; c2* 
    #(  6.8667   3.9183  -3.1647) ; h2**
    #(  8.5860   5.0910  -2.6140) ; o2* 
    #(  8.9510   4.7626  -1.7890) ; h2* 
    #(  6.5720   6.0040  -3.6090) ; c3* 
    #(  5.5636   5.7066  -3.8966) ; h3* 
    #(  7.3801   6.3562  -4.7350) ; o3* 
    #(  4.7150   0.4910  -0.1360) ; n1  
    #(  6.3490   2.1730  -0.6020) ; n3  
    #(  5.9530   0.9650  -0.2670) ; c2  
    #(  5.2900   2.9790  -0.8260) ; c4  
    #(  3.9720   2.6390  -0.7330) ; c5  
    #(  3.6770   1.3160  -0.3660) ; c6  
    #(  6.8426   0.0056  -0.0019) ; n2  
    #(  3.1660   3.7290  -1.0360) ; n7  
    #(  5.3170   4.2990  -1.1930) ; n9  
    #(  4.0100   4.6780  -1.2990) ; c8  
    #(  2.4280   0.8450  -0.2360) ; o6  
    #(  4.6151  -0.4677   0.1305) ; h1  
    #(  6.6463  -0.9463   0.2729) ; h21 
    #(  7.8170   0.2642  -0.0640) ; h22 
    #(  3.4421   5.5744  -1.5482) ; h8  
  ))

(define rg01
  (make-constant-rg
    #( -0.0043  -0.8175   0.5759  ; dgf-base-tfo
        0.2617  -0.5567  -0.7884
        0.9651   0.1473   0.2164
        0.0359   8.3929   0.5532)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  5.4352   8.2183  -2.7757) ; c5* 
    #(  5.3830   8.7883  -1.8481) ; h5* 
    #(  5.7729   8.7436  -3.6691) ; h5**
    #(  6.4830   7.1518  -2.5252) ; c4* 
    #(  7.4749   7.5972  -2.4482) ; h4* 
    #(  6.1626   6.4620  -1.2827) ; o4* 
    #(  6.5431   5.0992  -1.3905) ; c1* 
    #(  7.2871   4.9328  -0.6114) ; h1* 
    #(  7.1852   4.8935  -2.7592) ; c2* 
    #(  6.8573   3.9363  -3.1645) ; h2**
    #(  8.5780   5.1025  -2.6046) ; o2* 
    #(  8.9516   4.7577  -1.7902) ; h2* 
    #(  6.5522   6.0300  -3.5612) ; c3* 
    #(  5.5420   5.7356  -3.8459) ; h3* 
    #(  7.3487   6.4089  -4.6867) ; o3* 
    #(  4.7442   0.4514  -0.1390) ; n1  
    #(  6.3687   2.1459  -0.5926) ; n3  
    #(  5.9795   0.9335  -0.2657) ; c2  
    #(  5.3052   2.9471  -0.8125) ; c4  
    #(  3.9891   2.5987  -0.7230) ; c5  
    #(  3.7016   1.2717  -0.3647) ; c6  
    #(  6.8745  -0.0224  -0.0058) ; n2  
    #(  3.1770   3.6859  -1.0198) ; n7  
    #(  5.3247   4.2695  -1.1710) ; n9  
    #(  4.0156   4.6415  -1.2759) ; c8  
    #(  2.4553   0.7925  -0.2390) ; o6  
    #(  4.6497  -0.5095   0.1212) ; h1  
    #(  6.6836  -0.9771   0.2627) ; h21 
    #(  7.8474   0.2424  -0.0653) ; h22 
    #(  3.4426   5.5361  -1.5199) ; h8  
  ))

(define rg02
  (make-constant-rg
    #(  0.5566   0.0449   0.8296  ; dgf-base-tfo
        0.5125   0.7673  -0.3854
       -0.6538   0.6397   0.4041
       -9.1161  -3.7679  -2.9968)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.5778   6.6594  -4.0364) ; c5* 
    #(  4.9220   7.1963  -4.9204) ; h5* 
    #(  3.7996   5.9091  -4.1764) ; h5**
    #(  5.7873   5.8869  -3.5482) ; c4* 
    #(  6.0405   5.0875  -4.2446) ; h4* 
    #(  6.9135   6.8036  -3.4310) ; o4* 
    #(  7.7293   6.4084  -2.3392) ; c1* 
    #(  8.7078   6.1815  -2.7624) ; h1* 
    #(  7.1305   5.1418  -1.7347) ; c2* 
    #(  7.2040   5.1982  -0.6486) ; h2**
    #(  7.7417   4.0392  -2.3813) ; o2* 
    #(  8.6785   4.1443  -2.5630) ; h2* 
    #(  5.6666   5.2728  -2.1536) ; c3* 
    #(  5.1747   5.9805  -1.4863) ; h3* 
    #(  4.9997   4.0086  -2.1973) ; o3* 
    #( 10.3245   8.5459   1.5467) ; n1  
    #(  9.8051   6.9432  -0.1497) ; n3  
    #( 10.5175   7.4328   0.8408) ; c2  
    #(  8.7523   7.7422  -0.4228) ; c4  
    #(  8.4257   8.9060   0.2099) ; c5  
    #(  9.2665   9.3242   1.2540) ; c6  
    #( 11.6077   6.7966   1.2752) ; n2  
    #(  7.2750   9.4537  -0.3428) ; n7  
    #(  7.7962   7.5519  -1.3859) ; n9  
    #(  6.9479   8.6157  -1.2771) ; c8  
    #(  9.0664  10.4462   1.9610) ; o6  
    #( 10.9838   8.7524   2.2697) ; h1  
    #( 12.2274   7.0896   2.0170) ; h21 
    #( 11.8502   5.9398   0.7984) ; h22 
    #(  6.0430   8.9853  -1.7594) ; h8  
  ))

(define rg03
  (make-constant-rg
    #( -0.5021   0.0731   0.8617  ; dgf-base-tfo
       -0.8112   0.3054  -0.4986
       -0.2996  -0.9494  -0.0940
        6.4273  -5.1944  -3.7807)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.1214   6.7116  -1.9049) ; c5* 
    #(  3.3465   5.9610  -2.0607) ; h5* 
    #(  4.0789   7.2928  -0.9837) ; h5**
    #(  5.4170   5.9293  -1.8186) ; c4* 
    #(  5.4506   5.3400  -0.9023) ; h4* 
    #(  5.5067   5.0417  -2.9703) ; o4* 
    #(  6.8650   4.9152  -3.3612) ; c1* 
    #(  7.1090   3.8577  -3.2603) ; h1* 
    #(  7.7152   5.7282  -2.3894) ; c2* 
    #(  8.5029   6.2356  -2.9463) ; h2**
    #(  8.1036   4.8568  -1.3419) ; o2* 
    #(  8.3270   3.9651  -1.6184) ; h2* 
    #(  6.7003   6.7565  -1.8911) ; c3* 
    #(  6.5898   7.5329  -2.6482) ; h3* 
    #(  7.0505   7.2878  -0.6105) ; o3* 
    #(  9.6740   4.7656  -7.6614) ; n1  
    #(  9.0739   4.3013  -5.3941) ; n3  
    #(  9.8416   4.2192  -6.4581) ; c2  
    #(  7.9885   5.0632  -5.6446) ; c4  
    #(  7.6822   5.6856  -6.8194) ; c5  
    #(  8.5831   5.5215  -7.8840) ; c6  
    #( 10.9733   3.5117  -6.4286) ; n2  
    #(  6.4857   6.3816  -6.7035) ; n7  
    #(  6.9740   5.3703  -4.7760) ; n9  
    #(  6.1133   6.1613  -5.4808) ; c8  
    #(  8.4084   6.0747  -9.0933) ; o6  
    #( 10.3759   4.5855  -8.3504) ; h1  
    #( 11.6254   3.3761  -7.1879) ; h21 
    #( 11.1917   3.0460  -5.5593) ; h22 
    #(  5.1705   6.6830  -5.3167) ; h8  
  ))

(define rg04
  (make-constant-rg
    #( -0.5426  -0.8175   0.1929  ; dgf-base-tfo
        0.8304  -0.5567  -0.0237
        0.1267   0.1473   0.9809
       -0.5075   8.3929   0.2229)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  5.4352   8.2183  -2.7757) ; c5* 
    #(  5.3830   8.7883  -1.8481) ; h5* 
    #(  5.7729   8.7436  -3.6691) ; h5**
    #(  6.4830   7.1518  -2.5252) ; c4* 
    #(  7.4749   7.5972  -2.4482) ; h4* 
    #(  6.1626   6.4620  -1.2827) ; o4* 
    #(  6.5431   5.0992  -1.3905) ; c1* 
    #(  7.2871   4.9328  -0.6114) ; h1* 
    #(  7.1852   4.8935  -2.7592) ; c2* 
    #(  6.8573   3.9363  -3.1645) ; h2**
    #(  8.5780   5.1025  -2.6046) ; o2* 
    #(  8.9516   4.7577  -1.7902) ; h2* 
    #(  6.5522   6.0300  -3.5612) ; c3* 
    #(  5.5420   5.7356  -3.8459) ; h3* 
    #(  7.3487   6.4089  -4.6867) ; o3* 
    #(  3.6343   2.6680   2.0783) ; n1  
    #(  5.4505   3.9805   1.2446) ; n3  
    #(  4.7540   3.3816   2.1851) ; c2  
    #(  4.8805   3.7951   0.0354) ; c4  
    #(  3.7416   3.0925  -0.2305) ; c5  
    #(  3.0873   2.4980   0.8606) ; c6  
    #(  5.1433   3.4373   3.4609) ; n2  
    #(  3.4605   3.1184  -1.5906) ; n7  
    #(  5.3247   4.2695  -1.1710) ; n9  
    #(  4.4244   3.8244  -2.0953) ; c8  
    #(  1.9600   1.7805   0.7462) ; o6  
    #(  3.2489   2.2879   2.9191) ; h1  
    #(  4.6785   3.0243   4.2568) ; h21 
    #(  5.9823   3.9654   3.6539) ; h22 
    #(  4.2675   3.8876  -3.1721) ; h8  
  ))

(define rg05
  (make-constant-rg
    #( -0.5891   0.0449   0.8068  ; dgf-base-tfo
        0.5375   0.7673   0.3498
       -0.6034   0.6397  -0.4762
       -0.3019  -3.7679  -9.5913)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.5778   6.6594  -4.0364) ; c5* 
    #(  4.9220   7.1963  -4.9204) ; h5* 
    #(  3.7996   5.9091  -4.1764) ; h5**
    #(  5.7873   5.8869  -3.5482) ; c4* 
    #(  6.0405   5.0875  -4.2446) ; h4* 
    #(  6.9135   6.8036  -3.4310) ; o4* 
    #(  7.7293   6.4084  -2.3392) ; c1* 
    #(  8.7078   6.1815  -2.7624) ; h1* 
    #(  7.1305   5.1418  -1.7347) ; c2* 
    #(  7.2040   5.1982  -0.6486) ; h2**
    #(  7.7417   4.0392  -2.3813) ; o2* 
    #(  8.6785   4.1443  -2.5630) ; h2* 
    #(  5.6666   5.2728  -2.1536) ; c3* 
    #(  5.1747   5.9805  -1.4863) ; h3* 
    #(  4.9997   4.0086  -2.1973) ; o3* 
    #( 10.2594  10.6774  -1.0056) ; n1  
    #(  9.7528   8.7080  -2.2631) ; n3  
    #( 10.4471   9.7876  -1.9791) ; c2  
    #(  8.7271   8.5575  -1.3991) ; c4  
    #(  8.4100   9.3803  -0.3580) ; c5  
    #(  9.2294  10.5030  -0.1574) ; c6  
    #( 11.5110  10.1256  -2.7114) ; n2  
    #(  7.2891   8.9068   0.3121) ; n7  
    #(  7.7962   7.5519  -1.3859) ; n9  
    #(  6.9702   7.8292  -0.3353) ; c8  
    #(  9.0349  11.3951   0.8250) ; o6  
    #( 10.9013  11.4422  -0.9512) ; h1  
    #( 12.1031  10.9341  -2.5861) ; h21 
    #( 11.7369   9.5180  -3.4859) ; h22 
    #(  6.0888   7.3990   0.1403) ; h8  
  ))

(define rg06
  (make-constant-rg
    #( -0.9815   0.0731  -0.1772  ; dgf-base-tfo
        0.1912   0.3054  -0.9328
       -0.0141  -0.9494  -0.3137
        5.7506  -5.1944   4.7470)
    #( -0.8143  -0.5091  -0.2788  ; p-o3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; p-o3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; p-o3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; p   
    #(  1.6980   7.6960  -3.5570) ; o1p 
    #(  3.2260   9.5010  -4.4020) ; o2p 
    #(  4.1590   7.6040  -3.0340) ; o5* 
    #(  4.1214   6.7116  -1.9049) ; c5* 
    #(  3.3465   5.9610  -2.0607) ; h5* 
    #(  4.0789   7.2928  -0.9837) ; h5**
    #(  5.4170   5.9293  -1.8186) ; c4* 
    #(  5.4506   5.3400  -0.9023) ; h4* 
    #(  5.5067   5.0417  -2.9703) ; o4* 
    #(  6.8650   4.9152  -3.3612) ; c1* 
    #(  7.1090   3.8577  -3.2603) ; h1* 
    #(  7.7152   5.7282  -2.3894) ; c2* 
    #(  8.5029   6.2356  -2.9463) ; h2**
    #(  8.1036   4.8568  -1.3419) ; o2* 
    #(  8.3270   3.9651  -1.6184) ; h2* 
    #(  6.7003   6.7565  -1.8911) ; c3* 
    #(  6.5898   7.5329  -2.6482) ; h3* 
    #(  7.0505   7.2878  -0.6105) ; o3* 
    #(  6.6624   3.5061  -8.2986) ; n1  
    #(  6.5810   3.2570  -5.9221) ; n3  
    #(  6.5151   2.8263  -7.1625) ; c2  
    #(  6.8364   4.5817  -5.8882) ; c4  
    #(  7.0116   5.4064  -6.9609) ; c5  
    #(  6.9173   4.8260  -8.2361) ; c6  
    #(  6.2717   1.5402  -7.4250) ; n2  
    #(  7.2573   6.7070  -6.5394) ; n7  
    #(  6.9740   5.3703  -4.7760) ; n9  
    #(  7.2238   6.6275  -5.2453) ; c8  
    #(  7.0668   5.5163  -9.3763) ; o6  
    #(  6.5754   2.9964  -9.1545) ; h1  
    #(  6.1908   1.1105  -8.3354) ; h21 
    #(  6.1346   0.9352  -6.6280) ; h22 
    #(  7.4108   7.6227  -4.8418) ; h8  
  ))

(define rg07
  (make-constant-rg
    #(  0.0894  -0.6059   0.7905  ; dgf-base-tfo
       -0.6810   0.5420   0.4924
       -0.7268  -0.5824  -0.3642
       34.1424  45.9610 -11.8600)
    #( -0.8644  -0.4956  -0.0851  ; p-o3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; p-o3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; p-o3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; p   
    #( 29.9860   0.6630  47.6290) ; o1p 
    #( 31.7210  -0.6460  48.8090) ; o2p 
    #( 32.4940   1.2540  47.2740) ; o5* 
    #( 33.8709   0.7918  47.2113) ; c5* 
    #( 34.1386   0.5870  46.1747) ; h5* 
    #( 34.0186  -0.0095  47.9353) ; h5**
    #( 34.7297   1.9687  47.6685) ; c4* 
    #( 35.7723   1.6845  47.8113) ; h4* 
    #( 34.6455   2.9768  46.6660) ; o4* 
    #( 34.1690   4.1829  47.2627) ; c1* 
    #( 35.0437   4.7633  47.5560) ; h1* 
    #( 33.4145   3.7532  48.4954) ; c2* 
    #( 32.4340   3.3797  48.2001) ; h2**
    #( 33.3209   4.6953  49.5217) ; o2* 
    #( 33.2374   5.6059  49.2295) ; h2* 
    #( 34.2724   2.5970  48.9773) ; c3* 
    #( 33.6373   1.8935  49.5157) ; h3* 
    #( 35.3453   3.1884  49.7285) ; o3* 
    #( 34.0511   7.8930  43.7791) ; n1  
    #( 34.9937   6.3369  45.3199) ; n3  
    #( 35.0882   7.3126  44.4200) ; c2  
    #( 33.7190   5.9650  45.5374) ; c4  
    #( 32.5845   6.4770  44.9458) ; c5  
    #( 32.7430   7.5179  43.9914) ; c6  
    #( 36.3030   7.7827  44.1036) ; n2  
    #( 31.4499   5.8335  45.4368) ; n7  
    #( 33.2760   4.9817  46.4043) ; n9  
    #( 31.9235   4.9639  46.2934) ; c8  
    #( 31.8602   8.1000  43.3695) ; o6  
    #( 34.2623   8.6223  43.1283) ; h1  
    #( 36.5188   8.5081  43.4347) ; h21 
    #( 37.0888   7.3524  44.5699) ; h22 
    #( 31.0815   4.4201  46.7218) ; h8  
  ))

(define rg08
  (make-constant-rg
    #(  0.2224   0.6335   0.7411  ; dgf-base-tfo
       -0.3644  -0.6510   0.6659
        0.9043  -0.4181   0.0861
      -47.6824  -0.5823 -31.7554)
    #( -0.8644  -0.4956  -0.0851  ; p-o3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; p-o3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; p-o3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; p   
    #( 29.9860   0.6630  47.6290) ; o1p 
    #( 31.7210  -0.6460  48.8090) ; o2p 
    #( 32.4940   1.2540  47.2740) ; o5* 
    #( 32.5924   2.3488  48.2255) ; c5* 
    #( 33.3674   2.1246  48.9584) ; h5* 
    #( 31.5994   2.5917  48.6037) ; h5**
    #( 33.0722   3.5577  47.4258) ; c4* 
    #( 33.0310   4.4778  48.0089) ; h4* 
    #( 34.4173   3.3055  47.0316) ; o4* 
    #( 34.5056   3.3910  45.6094) ; c1* 
    #( 34.7881   4.4152  45.3663) ; h1* 
    #( 33.1122   3.1198  45.1010) ; c2* 
    #( 32.9230   2.0469  45.1369) ; h2**
    #( 32.7946   3.6590  43.8529) ; o2* 
    #( 33.5170   3.6707  43.2207) ; h2* 
    #( 32.2730   3.8173  46.1566) ; c3* 
    #( 31.3094   3.3123  46.2244) ; h3* 
    #( 32.2391   5.2039  45.7807) ; o3* 
    #( 39.3337   2.7157  44.1441) ; n1  
    #( 37.4430   3.8242  45.0824) ; n3  
    #( 38.7276   3.7646  44.7403) ; c2  
    #( 36.7791   2.6963  44.7704) ; c4  
    #( 37.2860   1.5653  44.1678) ; c5  
    #( 38.6647   1.5552  43.8235) ; c6  
    #( 39.5123   4.8216  44.9936) ; n2  
    #( 36.2829   0.6110  44.0078) ; n7  
    #( 35.4394   2.4314  44.9931) ; n9  
    #( 35.2180   1.1815  44.5128) ; c8  
    #( 39.2907   0.6514  43.2796) ; o6  
    #( 40.3076   2.8048  43.9352) ; h1  
    #( 40.4994   4.9066  44.7977) ; h21 
    #( 39.0738   5.6108  45.4464) ; h22 
    #( 34.3856   0.4842  44.4185) ; h8  
  ))

(define rg09
  (make-constant-rg
    #( -0.9699  -0.1688  -0.1753  ; dgf-base-tfo
       -0.1050  -0.3598   0.9271
       -0.2196   0.9176   0.3312
       45.6217 -38.9484 -12.3208)
    #( -0.8644  -0.4956  -0.0851  ; p-o3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; p-o3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; p-o3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; p   
    #( 29.9860   0.6630  47.6290) ; o1p 
    #( 31.7210  -0.6460  48.8090) ; o2p 
    #( 32.4940   1.2540  47.2740) ; o5* 
    #( 33.8709   0.7918  47.2113) ; c5* 
    #( 34.1386   0.5870  46.1747) ; h5* 
    #( 34.0186  -0.0095  47.9353) ; h5**
    #( 34.7297   1.9687  47.6685) ; c4* 
    #( 34.5880   2.8482  47.0404) ; h4* 
    #( 34.3575   2.2770  49.0081) ; o4* 
    #( 35.5157   2.1993  49.8389) ; c1* 
    #( 35.9424   3.2010  49.8893) ; h1* 
    #( 36.4701   1.2820  49.1169) ; c2* 
    #( 36.1545   0.2498  49.2683) ; h2**
    #( 37.8262   1.4547  49.4008) ; o2* 
    #( 38.0227   1.6945  50.3094) ; h2* 
    #( 36.2242   1.6797  47.6725) ; c3* 
    #( 36.4297   0.8197  47.0351) ; h3* 
    #( 37.0289   2.8480  47.4426) ; o3* 
    #( 34.3005   3.5042  54.6070) ; n1  
    #( 34.7693   3.7936  52.2874) ; n3  
    #( 34.4484   4.2541  53.4939) ; c2  
    #( 34.9354   2.4584  52.2785) ; c4  
    #( 34.8092   1.5915  53.3422) ; c5  
    #( 34.4646   2.1367  54.6085) ; c6  
    #( 34.2514   5.5708  53.6503) ; n2  
    #( 35.0641   0.2835  52.9337) ; n7  
    #( 35.2669   1.6690  51.1915) ; n9  
    #( 35.3288   0.3954  51.6563) ; c8  
    #( 34.3151   1.5317  55.6650) ; o6  
    #( 34.0623   3.9797  55.4539) ; h1  
    #( 33.9950   6.0502  54.5016) ; h21 
    #( 34.3512   6.1432  52.8242) ; h22 
    #( 35.5414  -0.6006  51.2679) ; h8  
  ))

(define rg10
  (make-constant-rg
    #( -0.0980  -0.9723   0.2122  ; dgf-base-tfo
       -0.9731   0.1383   0.1841
       -0.2083  -0.1885  -0.9597
       17.8469  38.8265  37.0475)
    #( -0.8644  -0.4956  -0.0851  ; p-o3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; p-o3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; p-o3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; p   
    #( 29.9860   0.6630  47.6290) ; o1p 
    #( 31.7210  -0.6460  48.8090) ; o2p 
    #( 32.4940   1.2540  47.2740) ; o5* 
    #( 32.5924   2.3488  48.2255) ; c5* 
    #( 33.3674   2.1246  48.9584) ; h5* 
    #( 31.5994   2.5917  48.6037) ; h5**
    #( 33.0722   3.5577  47.4258) ; c4* 
    #( 34.0333   3.3761  46.9447) ; h4* 
    #( 32.0890   3.8338  46.4332) ; o4* 
    #( 31.6377   5.1787  46.5914) ; c1* 
    #( 32.2499   5.8016  45.9392) ; h1* 
    #( 31.9167   5.5319  48.0305) ; c2* 
    #( 31.1507   5.0820  48.6621) ; h2**
    #( 32.0865   6.8890  48.3114) ; o2* 
    #( 31.5363   7.4819  47.7942) ; h2* 
    #( 33.2398   4.8224  48.2563) ; c3* 
    #( 33.3166   4.5570  49.3108) ; h3* 
    #( 34.2528   5.7056  47.7476) ; o3* 
    #( 28.2782   6.3049  42.9364) ; n1  
    #( 30.4001   5.8547  43.9258) ; n3  
    #( 29.6195   6.1568  42.8913) ; c2  
    #( 29.7005   5.7006  45.0649) ; c4  
    #( 28.3383   5.8221  45.2343) ; c5  
    #( 27.5519   6.1461  44.0958) ; c6  
    #( 30.1838   6.3385  41.6890) ; n2  
    #( 27.9936   5.5926  46.5651) ; n7  
    #( 30.2046   5.3825  46.3136) ; n9  
    #( 29.1371   5.3398  47.1506) ; c8  
    #( 26.3361   6.3024  44.0495) ; o6  
    #( 27.8122   6.5394  42.0833) ; h1  
    #( 29.7125   6.5595  40.8235) ; h21 
    #( 31.1859   6.2231  41.6389) ; h22 
    #( 28.9406   5.1504  48.2059) ; h8  
  ))

(define rgs
  (list rg01 rg02 rg03 rg04 rg05 rg06 rg07 rg08 rg09 rg10))

(define ru
  (make-constant-ru
    #( -0.0359  -0.8071   0.5894  ; dgf-base-tfo
       -0.2669   0.5761   0.7726
       -0.9631  -0.1296  -0.2361
        0.1584   8.3434   0.5434)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2430  -8.2420   2.8260) ; c5* 
    #(  5.1974  -8.8497   1.9223) ; h5* 
    #(  5.5548  -8.7348   3.7469) ; h5**
    #(  6.3140  -7.2060   2.5510) ; c4* 
    #(  7.2954  -7.6762   2.4898) ; h4* 
    #(  6.0140  -6.5420   1.2890) ; o4* 
    #(  6.4190  -5.1840   1.3620) ; c1* 
    #(  7.1608  -5.0495   0.5747) ; h1* 
    #(  7.0760  -4.9560   2.7270) ; c2* 
    #(  6.7770  -3.9803   3.1099) ; h2**
    #(  8.4500  -5.1930   2.5810) ; o2* 
    #(  8.8309  -4.8755   1.7590) ; h2* 
    #(  6.4060  -6.0590   3.5580) ; c3* 
    #(  5.4021  -5.7313   3.8281) ; h3* 
    #(  7.1570  -6.4240   4.7070) ; o3* 
    #(  5.2170  -4.3260   1.1690) ; n1  
    #(  4.2960  -2.2560   0.6290) ; n3  
    #(  5.4330  -3.0200   0.7990) ; c2  
    #(  2.9930  -2.6780   0.7940) ; c4  
    #(  2.8670  -4.0630   1.1830) ; c5  
    #(  3.9570  -4.8300   1.3550) ; c6  
    #(  6.5470  -2.5560   0.6290) ; o2  
    #(  2.0540  -1.9000   0.6130) ; o4  
    #(  4.4300  -1.3020   0.3600) ; h3  
    #(  1.9590  -4.4570   1.3250) ; h5  
    #(  3.8460  -5.7860   1.6240) ; h6  
  ))

(define ru01
  (make-constant-ru
    #( -0.0137  -0.8012   0.5983  ; dgf-base-tfo
       -0.2523   0.5817   0.7733
       -0.9675  -0.1404  -0.2101
        0.2031   8.3874   0.4228)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2416  -8.2422   2.8181) ; c5* 
    #(  5.2050  -8.8128   1.8901) ; h5* 
    #(  5.5368  -8.7738   3.7227) ; h5**
    #(  6.3232  -7.2037   2.6002) ; c4* 
    #(  7.3048  -7.6757   2.5577) ; h4* 
    #(  6.0635  -6.5092   1.3456) ; o4* 
    #(  6.4697  -5.1547   1.4629) ; c1* 
    #(  7.2354  -5.0043   0.7018) ; h1* 
    #(  7.0856  -4.9610   2.8521) ; c2* 
    #(  6.7777  -3.9935   3.2487) ; h2**
    #(  8.4627  -5.1992   2.7423) ; o2* 
    #(  8.8693  -4.8638   1.9399) ; h2* 
    #(  6.3877  -6.0809   3.6362) ; c3* 
    #(  5.3770  -5.7562   3.8834) ; h3* 
    #(  7.1024  -6.4754   4.7985) ; o3* 
    #(  5.2764  -4.2883   1.2538) ; n1  
    #(  4.3777  -2.2062   0.7229) ; n3  
    #(  5.5069  -2.9779   0.9088) ; c2  
    #(  3.0693  -2.6246   0.8500) ; c4  
    #(  2.9279  -4.0146   1.2149) ; c5  
    #(  4.0101  -4.7892   1.4017) ; c6  
    #(  6.6267  -2.5166   0.7728) ; o2  
    #(  2.1383  -1.8396   0.6581) ; o4  
    #(  4.5223  -1.2489   0.4716) ; h3  
    #(  2.0151  -4.4065   1.3290) ; h5  
    #(  3.8886  -5.7486   1.6535) ; h6  
  ))

(define ru02
  (make-constant-ru
    #(  0.5141   0.0246   0.8574  ; dgf-base-tfo
       -0.5547  -0.7529   0.3542
        0.6542  -0.6577  -0.3734
       -9.1111  -3.4598  -3.2939)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  4.3825  -6.6585   4.0489) ; c5* 
    #(  4.6841  -7.2019   4.9443) ; h5* 
    #(  3.6189  -5.8889   4.1625) ; h5**
    #(  5.6255  -5.9175   3.5998) ; c4* 
    #(  5.8732  -5.1228   4.3034) ; h4* 
    #(  6.7337  -6.8605   3.5222) ; o4* 
    #(  7.5932  -6.4923   2.4548) ; c1* 
    #(  8.5661  -6.2983   2.9064) ; h1* 
    #(  7.0527  -5.2012   1.8322) ; c2* 
    #(  7.1627  -5.2525   0.7490) ; h2**
    #(  7.6666  -4.1249   2.4880) ; o2* 
    #(  8.5944  -4.2543   2.6981) ; h2* 
    #(  5.5661  -5.3029   2.2009) ; c3* 
    #(  5.0841  -6.0018   1.5172) ; h3* 
    #(  4.9062  -4.0452   2.2042) ; o3* 
    #(  7.6298  -7.6136   1.4752) ; n1  
    #(  8.6945  -8.7046  -0.2857) ; n3  
    #(  8.6943  -7.6514   0.6066) ; c2  
    #(  7.7426  -9.6987  -0.3801) ; c4  
    #(  6.6642  -9.5742   0.5722) ; c5  
    #(  6.6391  -8.5592   1.4526) ; c6  
    #(  9.5840  -6.8186   0.6136) ; o2  
    #(  7.8505 -10.5925  -1.2223) ; o4  
    #(  9.4601  -8.7514  -0.9277) ; h3  
    #(  5.9281 -10.2509   0.5782) ; h5  
    #(  5.8831  -8.4931   2.1028) ; h6  
  ))

(define ru03
  (make-constant-ru
    #( -0.4993   0.0476   0.8651  ; dgf-base-tfo
        0.8078  -0.3353   0.4847
        0.3132   0.9409   0.1290
        6.2989  -5.2303  -3.8577)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  3.9938  -6.7042   1.9023) ; c5* 
    #(  3.2332  -5.9343   2.0319) ; h5* 
    #(  3.9666  -7.2863   0.9812) ; h5**
    #(  5.3098  -5.9546   1.8564) ; c4* 
    #(  5.3863  -5.3702   0.9395) ; h4* 
    #(  5.3851  -5.0642   3.0076) ; o4* 
    #(  6.7315  -4.9724   3.4462) ; c1* 
    #(  7.0033  -3.9202   3.3619) ; h1* 
    #(  7.5997  -5.8018   2.4948) ; c2* 
    #(  8.3627  -6.3254   3.0707) ; h2**
    #(  8.0410  -4.9501   1.4724) ; o2* 
    #(  8.2781  -4.0644   1.7570) ; h2* 
    #(  6.5701  -6.8129   1.9714) ; c3* 
    #(  6.4186  -7.5809   2.7299) ; h3* 
    #(  6.9357  -7.3841   0.7235) ; o3* 
    #(  6.8024  -5.4718   4.8475) ; n1  
    #(  7.9218  -5.5700   6.8877) ; n3  
    #(  7.8908  -5.0886   5.5944) ; c2  
    #(  6.9789  -6.3827   7.4823) ; c4  
    #(  5.8742  -6.7319   6.6202) ; c5  
    #(  5.8182  -6.2769   5.3570) ; c6  
    #(  8.7747  -4.3728   5.1568) ; o2  
    #(  7.1154  -6.7509   8.6509) ; o4  
    #(  8.7055  -5.3037   7.4491) ; h3  
    #(  5.1416  -7.3178   6.9665) ; h5  
    #(  5.0441  -6.5310   4.7784) ; h6  
  ))

(define ru04
  (make-constant-ru
    #( -0.5669  -0.8012   0.1918  ; dgf-base-tfo
       -0.8129   0.5817   0.0273
       -0.1334  -0.1404  -0.9811
       -0.3279   8.3874   0.3355)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2416  -8.2422   2.8181) ; c5* 
    #(  5.2050  -8.8128   1.8901) ; h5* 
    #(  5.5368  -8.7738   3.7227) ; h5**
    #(  6.3232  -7.2037   2.6002) ; c4* 
    #(  7.3048  -7.6757   2.5577) ; h4* 
    #(  6.0635  -6.5092   1.3456) ; o4* 
    #(  6.4697  -5.1547   1.4629) ; c1* 
    #(  7.2354  -5.0043   0.7018) ; h1* 
    #(  7.0856  -4.9610   2.8521) ; c2* 
    #(  6.7777  -3.9935   3.2487) ; h2**
    #(  8.4627  -5.1992   2.7423) ; o2* 
    #(  8.8693  -4.8638   1.9399) ; h2* 
    #(  6.3877  -6.0809   3.6362) ; c3* 
    #(  5.3770  -5.7562   3.8834) ; h3* 
    #(  7.1024  -6.4754   4.7985) ; o3* 
    #(  5.2764  -4.2883   1.2538) ; n1  
    #(  3.8961  -3.0896  -0.1893) ; n3  
    #(  5.0095  -3.8907  -0.0346) ; c2  
    #(  3.0480  -2.6632   0.8116) ; c4  
    #(  3.4093  -3.1310   2.1292) ; c5  
    #(  4.4878  -3.9124   2.3088) ; c6  
    #(  5.7005  -4.2164  -0.9842) ; o2  
    #(  2.0800  -1.9458   0.5503) ; o4  
    #(  3.6834  -2.7882  -1.1190) ; h3  
    #(  2.8508  -2.8721   2.9172) ; h5  
    #(  4.7188  -4.2247   3.2295) ; h6  
  ))

(define ru05
  (make-constant-ru
    #( -0.6298   0.0246   0.7763  ; dgf-base-tfo
       -0.5226  -0.7529  -0.4001
        0.5746  -0.6577   0.4870
       -0.0208  -3.4598  -9.6882)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  4.3825  -6.6585   4.0489) ; c5* 
    #(  4.6841  -7.2019   4.9443) ; h5* 
    #(  3.6189  -5.8889   4.1625) ; h5**
    #(  5.6255  -5.9175   3.5998) ; c4* 
    #(  5.8732  -5.1228   4.3034) ; h4* 
    #(  6.7337  -6.8605   3.5222) ; o4* 
    #(  7.5932  -6.4923   2.4548) ; c1* 
    #(  8.5661  -6.2983   2.9064) ; h1* 
    #(  7.0527  -5.2012   1.8322) ; c2* 
    #(  7.1627  -5.2525   0.7490) ; h2**
    #(  7.6666  -4.1249   2.4880) ; o2* 
    #(  8.5944  -4.2543   2.6981) ; h2* 
    #(  5.5661  -5.3029   2.2009) ; c3* 
    #(  5.0841  -6.0018   1.5172) ; h3* 
    #(  4.9062  -4.0452   2.2042) ; o3* 
    #(  7.6298  -7.6136   1.4752) ; n1  
    #(  8.5977  -9.5977   0.7329) ; n3  
    #(  8.5951  -8.5745   1.6594) ; c2  
    #(  7.7372  -9.7371  -0.3364) ; c4  
    #(  6.7596  -8.6801  -0.4476) ; c5  
    #(  6.7338  -7.6721   0.4408) ; c6  
    #(  9.3993  -8.5377   2.5743) ; o2  
    #(  7.8374 -10.6990  -1.1008) ; o4  
    #(  9.2924 -10.3081   0.8477) ; h3  
    #(  6.0932  -8.6982  -1.1929) ; h5  
    #(  6.0481  -6.9515   0.3446) ; h6  
  ))

(define ru06
  (make-constant-ru
    #( -0.9837   0.0476  -0.1733  ; dgf-base-tfo
       -0.1792  -0.3353   0.9249
       -0.0141   0.9409   0.3384
        5.7793  -5.2303   4.5997)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  3.9938  -6.7042   1.9023) ; c5* 
    #(  3.2332  -5.9343   2.0319) ; h5* 
    #(  3.9666  -7.2863   0.9812) ; h5**
    #(  5.3098  -5.9546   1.8564) ; c4* 
    #(  5.3863  -5.3702   0.9395) ; h4* 
    #(  5.3851  -5.0642   3.0076) ; o4* 
    #(  6.7315  -4.9724   3.4462) ; c1* 
    #(  7.0033  -3.9202   3.3619) ; h1* 
    #(  7.5997  -5.8018   2.4948) ; c2* 
    #(  8.3627  -6.3254   3.0707) ; h2**
    #(  8.0410  -4.9501   1.4724) ; o2* 
    #(  8.2781  -4.0644   1.7570) ; h2* 
    #(  6.5701  -6.8129   1.9714) ; c3* 
    #(  6.4186  -7.5809   2.7299) ; h3* 
    #(  6.9357  -7.3841   0.7235) ; o3* 
    #(  6.8024  -5.4718   4.8475) ; n1  
    #(  6.6920  -5.0495   7.1354) ; n3  
    #(  6.6201  -4.5500   5.8506) ; c2  
    #(  6.9254  -6.3614   7.4926) ; c4  
    #(  7.1046  -7.2543   6.3718) ; c5  
    #(  7.0391  -6.7951   5.1106) ; c6  
    #(  6.4083  -3.3696   5.6340) ; o2  
    #(  6.9679  -6.6901   8.6800) ; o4  
    #(  6.5626  -4.3957   7.8812) ; h3  
    #(  7.2781  -8.2254   6.5350) ; h5  
    #(  7.1657  -7.4312   4.3503) ; h6  
  ))

(define ru07
  (make-constant-ru
    #( -0.9434   0.3172   0.0971  ; dgf-base-tfo
        0.2294   0.4125   0.8816
        0.2396   0.8539  -0.4619
        8.3625 -52.7147   1.3745)
    #(  0.2765  -0.1121  -0.9545  ; p-o3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; p-o3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; p-o3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; p   
    #( 21.9980  14.5500  46.8210) ; o1p 
    #( 21.1450  14.0270  44.5420) ; o2p 
    #( 22.1250  16.3600  44.9460) ; o5* 
    #( 21.5037  16.8594  43.7323) ; c5* 
    #( 20.8147  17.6663  43.9823) ; h5* 
    #( 21.1086  16.0230  43.1557) ; h5**
    #( 22.5654  17.4874  42.8616) ; c4* 
    #( 22.1584  17.7243  41.8785) ; h4* 
    #( 23.0557  18.6826  43.4751) ; o4* 
    #( 24.4788  18.6151  43.6455) ; c1* 
    #( 24.9355  19.0840  42.7739) ; h1* 
    #( 24.7958  17.1427  43.6474) ; c2* 
    #( 24.5652  16.7400  44.6336) ; h2**
    #( 26.1041  16.8773  43.2455) ; o2* 
    #( 26.7516  17.5328  43.5149) ; h2* 
    #( 23.8109  16.5979  42.6377) ; c3* 
    #( 23.5756  15.5686  42.9084) ; h3* 
    #( 24.2890  16.7447  41.2729) ; o3* 
    #( 24.9420  19.2174  44.8923) ; n1  
    #( 25.2655  20.5636  44.8883) ; n3  
    #( 25.1663  21.2219  43.8561) ; c2  
    #( 25.6911  21.1219  46.0494) ; c4  
    #( 25.8051  20.4068  47.2048) ; c5  
    #( 26.2093  20.9962  48.2534) ; c6  
    #( 25.4692  19.0221  47.2053) ; o2  
    #( 25.0502  18.4827  46.0370) ; o4  
    #( 25.9599  22.1772  46.0966) ; h3  
    #( 25.5545  18.4409  48.1234) ; h5  
    #( 24.7854  17.4265  45.9883) ; h6  
  ))

(define ru08
  (make-constant-ru
    #( -0.0080  -0.7928   0.6094  ; dgf-base-tfo
       -0.7512   0.4071   0.5197
       -0.6601  -0.4536  -0.5988
       44.1482  30.7036   2.1088)
    #(  0.2765  -0.1121  -0.9545  ; p-o3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; p-o3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; p-o3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; p   
    #( 21.9980  14.5500  46.8210) ; o1p 
    #( 21.1450  14.0270  44.5420) ; o2p 
    #( 22.1250  16.3600  44.9460) ; o5* 
    #( 23.5096  16.1227  44.5783) ; c5* 
    #( 23.5649  15.8588  43.5222) ; h5* 
    #( 23.9621  15.4341  45.2919) ; h5**
    #( 24.2805  17.4138  44.7151) ; c4* 
    #( 25.3492  17.2309  44.6030) ; h4* 
    #( 23.8497  18.3471  43.7208) ; o4* 
    #( 23.4090  19.5681  44.3321) ; c1* 
    #( 24.2595  20.2496  44.3524) ; h1* 
    #( 23.0418  19.1813  45.7407) ; c2* 
    #( 22.0532  18.7224  45.7273) ; h2**
    #( 23.1307  20.2521  46.6291) ; o2* 
    #( 22.8888  21.1051  46.2611) ; h2* 
    #( 24.0799  18.1326  46.0700) ; c3* 
    #( 23.6490  17.4370  46.7900) ; h3* 
    #( 25.3329  18.7227  46.5109) ; o3* 
    #( 22.2515  20.1624  43.6698) ; n1  
    #( 22.4760  21.0609  42.6406) ; n3  
    #( 23.6229  21.3462  42.3061) ; c2  
    #( 21.3986  21.6081  42.0236) ; c4  
    #( 20.1189  21.3012  42.3804) ; c5  
    #( 19.1599  21.8516  41.7578) ; c6  
    #( 19.8919  20.3745  43.4387) ; o2  
    #( 20.9790  19.8423  44.0440) ; o4  
    #( 21.5235  22.3222  41.2097) ; h3  
    #( 18.8732  20.1200  43.7312) ; h5  
    #( 20.8545  19.1313  44.8608) ; h6  
  ))

(define ru09
  (make-constant-ru
    #( -0.0317   0.1374   0.9900  ; dgf-base-tfo
       -0.3422  -0.9321   0.1184
        0.9391  -0.3351   0.0765
      -32.1929  25.8198 -28.5088)
    #(  0.2765  -0.1121  -0.9545  ; p-o3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; p-o3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; p-o3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; p   
    #( 21.9980  14.5500  46.8210) ; o1p 
    #( 21.1450  14.0270  44.5420) ; o2p 
    #( 22.1250  16.3600  44.9460) ; o5* 
    #( 21.5037  16.8594  43.7323) ; c5* 
    #( 20.8147  17.6663  43.9823) ; h5* 
    #( 21.1086  16.0230  43.1557) ; h5**
    #( 22.5654  17.4874  42.8616) ; c4* 
    #( 23.0565  18.3036  43.3915) ; h4* 
    #( 23.5375  16.5054  42.4925) ; o4* 
    #( 23.6574  16.4257  41.0649) ; c1* 
    #( 24.4701  17.0882  40.7671) ; h1* 
    #( 22.3525  16.9643  40.5396) ; c2* 
    #( 21.5993  16.1799  40.6133) ; h2**
    #( 22.4693  17.4849  39.2515) ; o2* 
    #( 23.0899  17.0235  38.6827) ; h2* 
    #( 22.0341  18.0633  41.5279) ; c3* 
    #( 20.9509  18.1709  41.5846) ; h3* 
    #( 22.7249  19.3020  41.2100) ; o3* 
    #( 23.8580  15.0648  40.5757) ; n1  
    #( 25.1556  14.5982  40.4523) ; n3  
    #( 26.1047  15.3210  40.7448) ; c2  
    #( 25.3391  13.3315  40.0020) ; c4  
    #( 24.2974  12.5148  39.6749) ; c5  
    #( 24.5450  11.3410  39.2610) ; c6  
    #( 22.9633  12.9979  39.8053) ; o2  
    #( 22.8009  14.2648  40.2524) ; o4  
    #( 26.3414  12.9194  39.8855) ; h3  
    #( 22.1227  12.3533  39.5486) ; h5  
    #( 21.7989  14.6788  40.3650) ; h6  
  ))

(define ru10
  (make-constant-ru
    #( -0.9674   0.1021  -0.2318  ; dgf-base-tfo
       -0.2514  -0.2766   0.9275
        0.0306   0.9555   0.2933
       27.8571 -42.1305 -24.4563)
    #(  0.2765  -0.1121  -0.9545  ; p-o3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; p-o3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; p-o3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; p   
    #( 21.9980  14.5500  46.8210) ; o1p 
    #( 21.1450  14.0270  44.5420) ; o2p 
    #( 22.1250  16.3600  44.9460) ; o5* 
    #( 23.5096  16.1227  44.5783) ; c5* 
    #( 23.5649  15.8588  43.5222) ; h5* 
    #( 23.9621  15.4341  45.2919) ; h5**
    #( 24.2805  17.4138  44.7151) ; c4* 
    #( 23.8509  18.1819  44.0720) ; h4* 
    #( 24.2506  17.8583  46.0741) ; o4* 
    #( 25.5830  18.0320  46.5775) ; c1* 
    #( 25.8569  19.0761  46.4256) ; h1* 
    #( 26.4410  17.1555  45.7033) ; c2* 
    #( 26.3459  16.1253  46.0462) ; h2**
    #( 27.7649  17.5888  45.6478) ; o2* 
    #( 28.1004  17.9719  46.4616) ; h2* 
    #( 25.7796  17.2997  44.3513) ; c3* 
    #( 25.9478  16.3824  43.7871) ; h3* 
    #( 26.2154  18.4984  43.6541) ; o3* 
    #( 25.7321  17.6281  47.9726) ; n1  
    #( 25.5136  18.5779  48.9560) ; n3  
    #( 25.2079  19.7276  48.6503) ; c2  
    #( 25.6482  18.1987  50.2518) ; c4  
    #( 25.9847  16.9266  50.6092) ; c5  
    #( 26.0918  16.6439  51.8416) ; c6  
    #( 26.2067  15.9515  49.5943) ; o2  
    #( 26.0713  16.3497  48.3080) ; o4  
    #( 25.4890  18.9105  51.0618) ; h3  
    #( 26.4742  14.9310  49.8682) ; h5  
    #( 26.2346  15.6394  47.4975) ; h6  
  ))

(define rus
  (list ru01 ru02 ru03 ru04 ru05 ru06 ru07 ru08 ru09 ru10))

(define rg*
  (make-constant-rg
    #( -0.2067  -0.0264   0.9780  ; dgf-base-tfo
        0.9770  -0.0586   0.2049
        0.0519   0.9979   0.0379
        1.0331 -46.8078 -36.4742)
    #( -0.8644  -0.4956  -0.0851  ; p-o3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; p-o3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; p-o3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; p   
    #( 29.9860   0.6630  47.6290) ; o1p 
    #( 31.7210  -0.6460  48.8090) ; o2p 
    #( 32.4940   1.2540  47.2740) ; o5* 
    #( 32.1610   2.2370  46.2560) ; c5* 
    #( 31.2986   2.8190  46.5812) ; h5* 
    #( 32.0980   1.7468  45.2845) ; h5**
    #( 33.3476   3.1959  46.1947) ; c4* 
    #( 33.2668   3.8958  45.3630) ; h4* 
    #( 33.3799   3.9183  47.4216) ; o4* 
    #( 34.6515   3.7222  48.0398) ; c1* 
    #( 35.2947   4.5412  47.7180) ; h1* 
    #( 35.1756   2.4228  47.4827) ; c2* 
    #( 34.6778   1.5937  47.9856) ; h2**
    #( 36.5631   2.2672  47.4798) ; o2* 
    #( 37.0163   2.6579  48.2305) ; h2* 
    #( 34.6953   2.5043  46.0448) ; c3* 
    #( 34.5444   1.4917  45.6706) ; h3* 
    #( 35.6679   3.3009  45.3487) ; o3* 
    #( 37.4804   4.0914  52.2559) ; n1  
    #( 36.9670   4.1312  49.9281) ; n3  
    #( 37.8045   4.2519  50.9550) ; c2  
    #( 35.7171   3.8264  50.3222) ; c4  
    #( 35.2668   3.6420  51.6115) ; c5  
    #( 36.2037   3.7829  52.6706) ; c6  
    #( 39.0869   4.5552  50.7092) ; n2  
    #( 33.9075   3.3338  51.6102) ; n7  
    #( 34.6126   3.6358  49.5108) ; n9  
    #( 33.5805   3.3442  50.3425) ; c8  
    #( 35.9958   3.6512  53.8724) ; o6  
    #( 38.2106   4.2053  52.9295) ; h1  
    #( 39.8218   4.6863  51.3896) ; h21 
    #( 39.3420   4.6857  49.7407) ; h22 
    #( 32.5194   3.1070  50.2664) ; h8  
  ))

(define ru*
  (make-constant-ru
    #( -0.0109   0.5907   0.8068  ; dgf-base-tfo
        0.2217  -0.7853   0.5780
        0.9751   0.1852  -0.1224
       -1.4225 -11.0956  -2.5217)
    #( -0.8313  -0.4738  -0.2906  ; p-o3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; p-o3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; p-o3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; p   
    #(  1.4950  -7.6230   3.4770) ; o1p 
    #(  2.9490  -9.4640   4.3740) ; o2p 
    #(  3.9730  -7.5950   3.0340) ; o5* 
    #(  5.2430  -8.2420   2.8260) ; c5* 
    #(  5.1974  -8.8497   1.9223) ; h5* 
    #(  5.5548  -8.7348   3.7469) ; h5**
    #(  6.3140  -7.2060   2.5510) ; c4* 
    #(  5.8744  -6.2116   2.4731) ; h4* 
    #(  7.2798  -7.2260   3.6420) ; o4* 
    #(  8.5733  -6.9410   3.1329) ; c1* 
    #(  8.9047  -6.0374   3.6446) ; h1* 
    #(  8.4429  -6.6596   1.6327) ; c2* 
    #(  9.2880  -7.1071   1.1096) ; h2**
    #(  8.2502  -5.2799   1.4754) ; o2* 
    #(  8.7676  -4.7284   2.0667) ; h2* 
    #(  7.1642  -7.4416   1.3021) ; c3* 
    #(  7.4125  -8.5002   1.2260) ; h3* 
    #(  6.5160  -6.9772   0.1267) ; o3* 
    #(  9.4531  -8.1107   3.4087) ; n1  
    #( 11.5931  -9.0015   3.6357) ; n3  
    #( 10.8101  -7.8950   3.3748) ; c2  
    #( 11.1439 -10.2744   3.9206) ; c4  
    #(  9.7056 -10.4026   3.9332) ; c5  
    #(  8.9192  -9.3419   3.6833) ; c6  
    #( 11.3013  -6.8063   3.1326) ; o2  
    #( 11.9431 -11.1876   4.1375) ; o4  
    #( 12.5840  -8.8673   3.6158) ; h3  
    #(  9.2891 -11.2898   4.1313) ; h5  
    #(  7.9263  -9.4537   3.6977) ; h6  
  ))



; -- partial instantiations ---------------------------------------------------

(def-struct #f var id tfo nuc)

; add a single-quote at the start of this line if you want lazy computation
(begin

(defmacro (mk-var i tfo nuc)
  `(make-var ,i ,tfo ,nuc))

(defmacro (absolute-pos var p)
  `(tfo-apply (var-tfo ,var) ,p))

(defmacro (lazy-computation-of expr)
  expr)
)

'; remove the single-quote from this line if you want lazy computation
(begin

(defmacro (mk-var i tfo nuc)
  `(make-var ,i ,tfo (make-relative-nuc ,tfo ,nuc)))

(defmacro (absolute-pos var p)
  `(force ,p))

(defmacro (lazy-computation-of expr)
  `(delay ,expr))
)

(defmacro (atom-pos atom var)
  `(let ((v ,var))
     (absolute-pos v (,atom (var-nuc v)))))

(define (get-var id lst)
  (let ((v (car lst)))
    (if (=fx id (var-id v))
      v
      (get-var id (cdr lst)))))

(define (make-relative-nuc tfo n)
  (cond ((ra? n)
         (make-ra
           (nuc-dgf-base-tfo  n)
           (nuc-p-o3*-275-tfo n)
           (nuc-p-o3*-180-tfo n)
           (nuc-p-o3*-60-tfo  n)
           (lazy-computation-of (tfo-apply tfo (nuc-p    n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o1p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n1   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n3   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c6   n)))
           (lazy-computation-of (tfo-apply tfo (ra-n6    n)))
           (lazy-computation-of (tfo-apply tfo (ra-n7    n)))
           (lazy-computation-of (tfo-apply tfo (ra-n9    n)))
           (lazy-computation-of (tfo-apply tfo (ra-c8    n)))
           (lazy-computation-of (tfo-apply tfo (ra-h2    n)))
           (lazy-computation-of (tfo-apply tfo (ra-h61   n)))
           (lazy-computation-of (tfo-apply tfo (ra-h62   n)))
           (lazy-computation-of (tfo-apply tfo (ra-h8    n)))))
        ((rc? n)
         (make-rc
           (nuc-dgf-base-tfo  n)
           (nuc-p-o3*-275-tfo n)
           (nuc-p-o3*-180-tfo n)
           (nuc-p-o3*-60-tfo  n)
           (lazy-computation-of (tfo-apply tfo (nuc-p    n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o1p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n1   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n3   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c6   n)))
           (lazy-computation-of (tfo-apply tfo (rc-n4    n)))
           (lazy-computation-of (tfo-apply tfo (rc-o2    n)))
           (lazy-computation-of (tfo-apply tfo (rc-h41   n)))
           (lazy-computation-of (tfo-apply tfo (rc-h42   n)))
           (lazy-computation-of (tfo-apply tfo (rc-h5    n)))
           (lazy-computation-of (tfo-apply tfo (rc-h6    n)))))
        ((rg? n)
         (make-rg
           (nuc-dgf-base-tfo  n)
           (nuc-p-o3*-275-tfo n)
           (nuc-p-o3*-180-tfo n)
           (nuc-p-o3*-60-tfo  n)
           (lazy-computation-of (tfo-apply tfo (nuc-p    n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o1p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n1   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n3   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c6   n)))
           (lazy-computation-of (tfo-apply tfo (rg-n2    n)))
           (lazy-computation-of (tfo-apply tfo (rg-n7    n)))
           (lazy-computation-of (tfo-apply tfo (rg-n9    n)))
           (lazy-computation-of (tfo-apply tfo (rg-c8    n)))
           (lazy-computation-of (tfo-apply tfo (rg-o6    n)))
           (lazy-computation-of (tfo-apply tfo (rg-h1    n)))
           (lazy-computation-of (tfo-apply tfo (rg-h21   n)))
           (lazy-computation-of (tfo-apply tfo (rg-h22   n)))
           (lazy-computation-of (tfo-apply tfo (rg-h8    n)))))
        (else
         (make-ru
           (nuc-dgf-base-tfo  n)
           (nuc-p-o3*-275-tfo n)
           (nuc-p-o3*-180-tfo n)
           (nuc-p-o3*-60-tfo  n)
           (lazy-computation-of (tfo-apply tfo (nuc-p    n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o1p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2p  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h5** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o4*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h1*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2** n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h2*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-h3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-o3*  n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n1   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-n3   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c2   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c4   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c5   n)))
           (lazy-computation-of (tfo-apply tfo (nuc-c6   n)))
           (lazy-computation-of (tfo-apply tfo (ru-o2    n)))
           (lazy-computation-of (tfo-apply tfo (ru-o4    n)))
           (lazy-computation-of (tfo-apply tfo (ru-h3    n)))
           (lazy-computation-of (tfo-apply tfo (ru-h5    n)))
           (lazy-computation-of (tfo-apply tfo (ru-h6    n)))))))

; -- search -------------------------------------------------------------------

; sequential backtracking algorithm

(define (search partial-inst domains constraint?)
  (if (null? domains)
    (list partial-inst)
    (let ((remaining-domains (cdr domains)))

      (define (try-assignments lst)
        (if (null? lst)
          '()
          (let ((var (car lst)))
            (if (constraint? var partial-inst)
              (let* ((subsols1
                       (search
                         (cons var partial-inst)
                         remaining-domains
                         constraint?))
                     (subsols2
                       (try-assignments (cdr lst))))
                (append subsols1 subsols2))
              (try-assignments (cdr lst))))))

      (try-assignments ((car domains) partial-inst)))))

; -- domains ------------------------------------------------------------------

; primary structure:   strand a cugccacgucug, strand b cagacguggcag
;
; secondary structure: strand a cugccacgucug
;                               ||||||||||||
;                               gacggugcagac strand b
;
; tertiary structure:
;
;    5' end of strand a c1----g12 3' end of strand b
;                     u2-------a11
;                    g3-------c10
;                    c4-----g9
;                     c5---g8
;                        a6
;                      g6-c7
;                     c5----g8
;                    a4-------u9
;                    g3--------c10
;                     a2-------u11
;   5' end of strand b c1----g12 3' end of strand a
;
; "helix", "stacked" and "connected" describe the spatial relationship
; between two consecutive nucleotides. e.g. the nucleotides c1 and u2
; from the strand a.
;
; "wc" (stands for watson-crick and is a type of base-pairing),
; and "wc-dumas" describe the spatial relationship between 
; nucleotides from two chains that are growing in opposite directions.
; e.g. the nucleotides c1 from strand a and g12 from strand b.

; dynamic domains

; given,
;   "ref" a nucleotide which is already positioned,
;   "nuc" the nucleotide to be placed,
;   and "tfo" a transformation matrix which expresses the desired
;   relationship between "ref" and "nuc",
; the function "dgf-base" computes the transformation matrix that
; places the nucleotide "nuc" in the given relationship to "ref".

(define (dgf-base tfo ref nuc)
  (let* ((ref-nuc (var-nuc ref))
         (align
          (tfo-inv-ortho
            (cond ((ra? ref-nuc)
                   (tfo-align (atom-pos nuc-c1* ref)
                              (atom-pos ra-n9   ref)
                              (atom-pos nuc-c4  ref)))
                  ((rc? ref-nuc)
                   (tfo-align (atom-pos nuc-c1* ref)
                              (atom-pos nuc-n1  ref)
                              (atom-pos nuc-c2  ref)))
                  ((rg? ref-nuc)
                   (tfo-align (atom-pos nuc-c1* ref)
                              (atom-pos rg-n9   ref)
                              (atom-pos nuc-c4  ref)))
                  (else
                   (tfo-align (atom-pos nuc-c1* ref)
                              (atom-pos nuc-n1  ref)
                              (atom-pos nuc-c2  ref)))))))
    (tfo-combine (nuc-dgf-base-tfo nuc)
                 (tfo-combine tfo align))))

; placement of first nucleotide.

(define (reference nuc i)
  (lambda (partial-inst)
    (list (mk-var i tfo-id nuc))))

; the transformation matrix for wc is from:
;
; chandrasekaran r. et al (1989) a re-examination of the crystal
; structure of a-dna using fiber diffraction data. j. biomol.
; struct. & dynamics 6(6):1189-1202.

(define wc-tfo
  '#(-1.0000  0.0028 -0.0019
      0.0028  0.3468 -0.9379
     -0.0019 -0.9379 -0.3468
     -0.0080  6.0730  8.7208))

(define (wc nuc i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (tfo (dgf-base wc-tfo ref nuc)))
      (list (mk-var i tfo nuc)))))

(define wc-dumas-tfo
  '#(-0.9737 -0.1834  0.1352
     -0.1779  0.2417 -0.9539
      0.1422 -0.9529 -0.2679
      0.4837  6.2649  8.0285))
         
(define (wc-dumas nuc i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (tfo (dgf-base wc-dumas-tfo ref nuc)))
      (list (mk-var i tfo nuc)))))

(define helix5*-tfo
  '#( 0.9886 -0.0961  0.1156
      0.1424  0.8452 -0.5152
     -0.0482  0.5258  0.8492
     -3.8737  0.5480  3.8024))

(define (helix5* nuc i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (tfo (dgf-base helix5*-tfo ref nuc)))
      (list (mk-var i tfo nuc)))))

(define helix3*-tfo
  '#( 0.9886  0.1424 -0.0482
     -0.0961  0.8452  0.5258
      0.1156 -0.5152  0.8492
      3.4426  2.0474 -3.7042))

(define (helix3* nuc i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (tfo (dgf-base helix3*-tfo ref nuc)))
      (list (mk-var i tfo nuc)))))

(define g37-a38-tfo
  '#( 0.9991  0.0164 -0.0387
     -0.0375  0.7616 -0.6470
      0.0189  0.6478  0.7615
     -3.3018  0.9975  2.5585))

(define (g37-a38 nuc i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (tfo (dgf-base g37-a38-tfo ref nuc)))
      (mk-var i tfo nuc))))

(define (stacked5* nuc i j)
  (lambda (partial-inst)
    (cons ((g37-a38 nuc i j) partial-inst)
          ((helix5* nuc i j) partial-inst))))

(define a38-g37-tfo
  '#( 0.9991 -0.0375  0.0189
      0.0164  0.7616  0.6478 
     -0.0387 -0.6470  0.7615
      3.3819  0.7718 -2.5321))

(define (a38-g37 nuc i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (tfo (dgf-base a38-g37-tfo ref nuc)))
      (mk-var i tfo nuc))))
   
(define (stacked3* nuc i j)
  (lambda (partial-inst)
    (cons ((a38-g37 nuc i j) partial-inst)
          ((helix3* nuc i j) partial-inst))))

(define (p-o3* nucs i j)
  (lambda (partial-inst)
    (let* ((ref (get-var j partial-inst))
           (align
             (tfo-inv-ortho
               (tfo-align (atom-pos nuc-o3* ref)
                          (atom-pos nuc-c3* ref)
                          (atom-pos nuc-c4* ref)))))
      (let loop ((lst nucs) (domains '()))
        (if (null? lst)
          domains
          (let ((nuc (car lst)))
            (let ((tfo-60 (tfo-combine (nuc-p-o3*-60-tfo nuc) align))
                  (tfo-180 (tfo-combine (nuc-p-o3*-180-tfo nuc) align))
                  (tfo-275 (tfo-combine (nuc-p-o3*-275-tfo nuc) align)))
              (loop (cdr lst)
                    (cons (mk-var i tfo-60 nuc)
                          (cons (mk-var i tfo-180 nuc)
                                (cons (mk-var i tfo-275 nuc) domains)))))))))))

; -- problem statement --------------------------------------------------------

; define anticodon problem -- science 253:1255 figure 3a, 3b and 3c

(define anticodon-domains
  (list 
   (reference rc  27   )
   (helix5*   rc  28 27)
   (helix5*   ra  29 28)
   (helix5*   rg  30 29)
   (helix5*   ra  31 30)
   (wc        ru  39 31)
   (helix5*   rc  40 39)
   (helix5*   ru  41 40)
   (helix5*   rg  42 41)
   (helix5*   rg  43 42)
   (stacked3* ra  38 39)
   (stacked3* rg  37 38)
   (stacked3* ra  36 37)
   (stacked3* ra  35 36)
   (stacked3* rg  34 35);<-. distance
   (p-o3*     rcs 32 31);  | constraint
   (p-o3*     rus 33 32);<-' 3.0 angstroms
   ))

; anticodon constraint

(define (anticodon-constraint? v partial-inst)
  (if (= (var-id v) 33)
    (let ((p   (atom-pos nuc-p (get-var 34 partial-inst))) ; p in nucleotide 34
          (o3* (atom-pos nuc-o3* v)))                      ; o3' in nucl. 33
      (float<= (pt-dist p o3*) 3.0))                       ; check distance
    #t))

(define (anticodon)
  (search '() anticodon-domains anticodon-constraint?))

; define pseudoknot problem -- science 253:1255 figure 4a and 4b

(define pseudoknot-domains
  (list
   (reference ra  23   )
   (wc-dumas  ru   8 23)
   (helix3*   rg  22 23)
   (wc-dumas  rc   9 22)
   (helix3*   rg  21 22)
   (wc-dumas  rc  10 21)
   (helix3*   rc  20 21)
   (wc-dumas  rg  11 20)
   (helix3*   ru* 19 20);<-.
   (wc-dumas  ra  12 19);  | distance
;                       ;  | constraint
; helix 1               ;  | 4.0 angstroms
   (helix3*   rc   3 19);  |
   (wc-dumas  rg  13  3);  |
   (helix3*   rc   2  3);  |
   (wc-dumas  rg  14  2);  |
   (helix3*   rc   1  2);  |
   (wc-dumas  rg* 15  1);  |
;                       ;  |
; l2 loop               ;  |
   (p-o3*     rus 16 15);  |
   (p-o3*     rcs 17 16);  |
   (p-o3*     ras 18 17);<-'
;
; l1 loop
   (helix3*   ru   7  8);<-.
   (p-o3*     rcs  4  3);  | constraint
   (stacked5* ru   5  4);  | 4.5 angstroms
   (stacked5* rc   6  5);<-'
   ))
  
; pseudoknot constraint

(define (pseudoknot-constraint? v partial-inst)
  (case (var-id v)
    ((18)
     (let ((p   (atom-pos nuc-p (get-var 19 partial-inst)))
           (o3* (atom-pos nuc-o3* v)))
       (float<= (pt-dist p o3*) 4.0)))
    ((6)
     (let ((p   (atom-pos nuc-p (get-var 7 partial-inst)))
           (o3* (atom-pos nuc-o3* v)))
       (float<= (pt-dist p o3*) 4.5)))
    (else
     #t)))

(define (pseudoknot)
  (search '() pseudoknot-domains pseudoknot-constraint?))

; -- testing -----------------------------------------------------------------

(define (list-of-atoms n)
  (cond ((ra? n)
         (vector
    (nuc-p    n)
    (nuc-o1p  n)
    (nuc-o2p  n)
    (nuc-o5*  n)
    (nuc-c5*  n)
    (nuc-h5*  n)
    (nuc-h5** n)
    (nuc-c4*  n)
    (nuc-h4*  n)
    (nuc-o4*  n)
    (nuc-c1*  n)
    (nuc-h1*  n)
    (nuc-c2*  n)
    (nuc-h2** n)
    (nuc-o2*  n)
    (nuc-h2*  n)
    (nuc-c3*  n)
    (nuc-h3*  n)
    (nuc-o3*  n)
    (nuc-n1   n)
    (nuc-n3   n)
    (nuc-c2   n)
    (nuc-c4   n)
    (nuc-c5   n)
    (nuc-c6   n)
           (ra-n6   n)
           (ra-n7   n)
           (ra-n9   n)
           (ra-c8   n)
           (ra-h2   n)
           (ra-h61  n)
           (ra-h62  n)
           (ra-h8   n)))
        ((rc? n)
         (vector
    (nuc-p    n)
    (nuc-o1p  n)
    (nuc-o2p  n)
    (nuc-o5*  n)
    (nuc-c5*  n)
    (nuc-h5*  n)
    (nuc-h5** n)
    (nuc-c4*  n)
    (nuc-h4*  n)
    (nuc-o4*  n)
    (nuc-c1*  n)
    (nuc-h1*  n)
    (nuc-c2*  n)
    (nuc-h2** n)
    (nuc-o2*  n)
    (nuc-h2*  n)
    (nuc-c3*  n)
    (nuc-h3*  n)
    (nuc-o3*  n)
    (nuc-n1   n)
    (nuc-n3   n)
    (nuc-c2   n)
    (nuc-c4   n)
    (nuc-c5   n)
    (nuc-c6   n)
           (rc-n4   n)
           (rc-o2   n)
           (rc-h41  n)
           (rc-h42  n)
           (rc-h5   n)
           (rc-h6   n)))
        ((rg? n)
         (vector
    (nuc-p    n)
    (nuc-o1p  n)
    (nuc-o2p  n)
    (nuc-o5*  n)
    (nuc-c5*  n)
    (nuc-h5*  n)
    (nuc-h5** n)
    (nuc-c4*  n)
    (nuc-h4*  n)
    (nuc-o4*  n)
    (nuc-c1*  n)
    (nuc-h1*  n)
    (nuc-c2*  n)
    (nuc-h2** n)
    (nuc-o2*  n)
    (nuc-h2*  n)
    (nuc-c3*  n)
    (nuc-h3*  n)
    (nuc-o3*  n)
    (nuc-n1   n)
    (nuc-n3   n)
    (nuc-c2   n)
    (nuc-c4   n)
    (nuc-c5   n)
    (nuc-c6   n)
           (rg-n2   n)
           (rg-n7   n)
           (rg-n9   n)
           (rg-c8   n)
           (rg-o6   n)
           (rg-h1   n)
           (rg-h21  n)
           (rg-h22  n)
           (rg-h8   n)))
        (else
         (vector
    (nuc-p    n)
    (nuc-o1p  n)
    (nuc-o2p  n)
    (nuc-o5*  n)
    (nuc-c5*  n)
    (nuc-h5*  n)
    (nuc-h5** n)
    (nuc-c4*  n)
    (nuc-h4*  n)
    (nuc-o4*  n)
    (nuc-c1*  n)
    (nuc-h1*  n)
    (nuc-c2*  n)
    (nuc-h2** n)
    (nuc-o2*  n)
    (nuc-h2*  n)
    (nuc-c3*  n)
    (nuc-h3*  n)
    (nuc-o3*  n)
    (nuc-n1   n)
    (nuc-n3   n)
    (nuc-c2   n)
    (nuc-c4   n)
    (nuc-c5   n)
    (nuc-c6   n)
           (ru-o2   n)
           (ru-o4   n)
           (ru-h3   n)
           (ru-h5   n)
           (ru-h6   n)))))

(define (var-most-distant-atom v)
  (let ((atoms (list-of-atoms (var-nuc v))))
    (let loop ((i (-fx (vector-length atoms) 1)) (result 0.0))
      (if (<fx i 0)
        result
        (let* ((pos (vector-ref atoms i))
               (abs-pos (absolute-pos v pos))
               (x (pt-x abs-pos))
               (y (pt-y abs-pos))
               (z (pt-z abs-pos))
               (d (floatsqrt (float+ (float* x x) (float* y y) (float* z z)))))
          (if (float> d result)
            (loop (-fx i 1) d)
            (loop (-fx i 1) result)))))))

(define (sol-most-distant-atom s)
  (maximum (map var-most-distant-atom s)))

(define (most-distant-atom sols)
  (maximum (map sol-most-distant-atom sols)))

(define (maximum lst)
  (let loop ((m (car lst)) (l (cdr lst)))
    (if (null? l)
      m
      (let ((x (car l)))
        (loop (if (float> x m) x m) (cdr l))))))

(define (check)
  (length (pseudoknot)))

(define (run)
   (most-distant-atom (pseudoknot)))

(define (doit num thunk)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (thunk)))
	  res))
   (loop num 0))

(define (test-exit n v)
   (let ((x (/ v n)))
      (exit (if (and (> x 33.7) (< x 33.9)) 0 1))))

;; main
(define (main argv)
   (let ((n (if (null? (cdr argv)) 50 (string->integer (cadr argv)))))
      (if (=fx n 1)
	  (let ((v (run)))
	     (print v)
	     (test-exit n v))
	  (test-exit n (doit n run)))))
