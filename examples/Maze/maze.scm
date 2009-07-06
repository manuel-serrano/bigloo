;;; Copyright (c) 1993 by Olin Shivers.

;;; Syntax for defining record types.
;;; This implementation works with the Scheme48 system --
;;; or any Scheme that uses Clinger's "explicit renaming"
;;; macro system.
;;;
;;; (define-record name . field-specs)
;;;
;;; A field-spec is one of the following:
;;;     field		; Initialised field
;;;     (field [default])	; Defaulted field.
;;; An initialised field has its initial value passed as an argument to
;;; the the record maker procedure. A defaulted field takes its value from
;;; the the DEFAULT expression. If a DEFAULT expression is not given, then
;;; the defaulted field's initial value is undefined.
;;; 
;;; Example:
;;; (define-record employee
;;;     name
;;;     id
;;;     (salary 10000)
;;;     (department)	; Initial value undefined.
;;;     sex
;;;     married?)
;;; 
;;; Defines the following:
;;; - A maker procedure:
;;;   (make-employee "John Smith" 742931 'male #f)
;;;   MAKE-EMPLOYEE takes one argument for each initialised field.
;;; 
;;; - Accessor procedures:
;;;   (employee:name emp)
;;;   (employee:id-number emp)
;;;   (employee:salary emp)
;;;   (employee:department emp)
;;;   (employee:sex emp)
;;;   (employee:married? emp)
;;; 
;;; - Setter procedures:
;;;   (set-employee:name emp)
;;;   (set-employee:id-number emp)
;;;   (set-employee:salary emp 20000)
;;;   (set-employee:department emp "Vaporware")
;;;   (set-employee:sex emp 'female)
;;;   (set-employee:married? emp #t)
;;; 
;;; - A type predicate:
;;;   (employee? x)
;;; 
;;; - The record type descriptor:
;;;     type/employee

(module maze (main main))

(define (bitwise-and x y)
   (bit-and x y))
(define (bitwise-or x y)
   (bit-or x y))
(define (bitwise-not x)
   (bit-not x))

(define-macro (receive variables producer . body)
   (define gensym (let ((n 0))
		     (lambda ()
			(set! n (+ 1 n))
			(string->symbol (string-append "g" (number->string n))))))
   (let ((values (gensym)))
      `(let ((,values ,producer))
          ,(let loop ((variables variables)
                      (off       0)
                      (bindings  '()))
              (if (null? variables)
                  `(let ,bindings ,@body)
                  (loop (cdr variables)
                        (+ off 1)
                        (cons `(,(car variables) (vector-ref ,values ,off))
                              bindings)))))))

(define-macro (values . values)
   `(vector ,@values))


(define-macro (define-record name . field)
   (define (symbol-append . s)
      (string->symbol (apply string-append (map symbol->string s))))
   (define gensym (let ((n 0))
		     (lambda ()
			(set! n (+ 1 n))
			(string->symbol (string-append "g" (number->string n))))))
   (let* ((alloc-name (symbol-append 'alloc- name))
	  (pred-name  (symbol-append name '?))
	  (make-name  (symbol-append 'make- name))
	  (init-field (let loop ((field  field)
				 (ifield '()))
			 (cond
			    ((null? field)
			     (reverse ifield))
			    ((pair? (car field))
			     (loop (cdr field) ifield))
			    (else
			     (loop (cdr field)
				   (cons (car field) ifield))))))
	  (default-field (let loop ((field  field)
				    (dfield '()))
			    (cond
			       ((null? field)
				(reverse dfield))
			       ((not (pair? (car field)))
				(loop (cdr field) dfield))
			       (else
				(loop (cdr field)
				      (cons (car field) dfield))))))
	  (len        (length field))
	  (alloc      `(define (,alloc-name)
			  (make-vector ,(+ len 1) 'unspecified)))
	  (pred       `(define (,pred-name x)
			  (and (vector? x)
			       (=fx (vector-length x) ,(+ len 1))
			       (eq? (vector-ref x 0) ',name))))
	  (make       (let ((v (gensym)))
			 `(define (,make-name ,@init-field)
			     (let ((,v (,alloc-name)))
				(vector-set! ,v 0 ',name)
				,@(let loop ((field  field)
					     (ifield init-field)
					     (init   '())
					     (off    1))
				     (cond
					((null? field)
					 (reverse (cons v init)))
					((pair? (car field))
					 (loop (cdr field)
					       ifield
					       (cons `(vector-set!
						       ,v
						       ,off
						       ,(cadr (car field)))
						     init)
					       (+ off 1)))
					(else
					 (loop (cdr field)
					       (cdr ifield)
					       (cons `(vector-set!
						       ,v
						       ,off
						       ,(car field))
						     init)
					       (+ off 1)))))))))
	  (set-get   (let loop ((field field)
				(off   1)
				(set   '()))
			(if (null? field)
			    (reverse set)
			    (let* ((fname (if (pair? (car field))
					      (car (car field))
					      (car field)))
				   (set-name (symbol-append 'set-
							    name
							    ':
							    fname))
				   (ref-name (symbol-append name
							    ':
							    fname))
				   (a-set `(define-macro (,set-name o v)
					    `(vector-set! ,o ,,off ,v)))
				   (a-ref `(define-macro (,ref-name o)
					    `(vector-ref ,o ,,off))))
			       (loop (cdr field)
				     (+ off 1)
				     (cons a-set (cons a-ref set))))))))
      `(begin ,alloc ,pred ,make ,@set-get)))
   
(define-record harr
  nrows
  ncols
  elts)

(define-record wall
  owner		; Box that owns this wall.
  neighbor	; The other box bordering this wall.
  bit)		; Integer -- a bit identifying this wall in OWNER's box.

(define-record box
  reachable	; Union/find set -- all reachable boxs.
  id		; Identifying info (e.g., the coords of the box).
  (walls -1)	; A bitset telling which walls are still standing.
  (parent #f)	; For DFS spanning tree construction.
  (mark #f))    ; For marking the solution path.

;*---------------------------------------------------------------------*/
;*    harr.scm                                                         */
;*---------------------------------------------------------------------*/
;;; Hex arrays
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - define-record

;;;        ___       ___       ___
;;;       /   \     /   \     /   \
;;;   ___/  A  \___/  A  \___/  A  \___
;;;  /   \     /   \     /   \     /   \
;;; /  A  \___/  A  \___/  A  \___/  A  \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/

;;; Hex arrays are indexed by the (x,y) coord of the center of the hexagonal
;;; element. Hexes are three wide and two high; e.g., to get from the center
;;; of an elt to its {NW, N, NE} neighbors, add {(-3,1), (0,2), (3,1)}
;;; respectively.
;;;
;;; Hex arrays are represented with a matrix, essentially made by shoving the
;;; odd columns down a half-box so things line up. The mapping is as follows:
;;;     Center coord      row/column
;;;     ------------      ----------
;;;     (x,  y)        -> (y/2, x/3)
;;;     (3c, 2r + c&1) <- (r,   c)




(define (harr r c)
  (make-harr r c (make-vector (*fx r c))))



(define (href ha x y)
  (let ((r (/fx y 2))
	(c (/fx x 3)))
    (vector-ref (harr:elts ha)
		(+fx (*fx (harr:ncols ha) r) c))))

(define (hset! ha x y val)
  (let ((r (/fx y 2))
	(c (/fx x 3)))
    (vector-set! (harr:elts ha)
		 (+fx (*fx (harr:ncols ha) r) c)
		 val)))

(define (href/rc ha r c)
    (vector-ref (harr:elts ha)
		(+fx (*fx (harr:ncols ha) r) c)))

;;; Create a nrows x ncols hex array. The elt centered on coord (x, y)
;;; is the value returned by (PROC x y).

(define (harr-tabulate nrows ncols proc)
  (let ((v (make-vector (*fx nrows ncols))))

    (do ((r (-fx nrows 1) (-fx r 1)))
	((<fx r 0))
      (do ((c 0 (+fx c 1))
	   (i (*fx r ncols) (+fx i 1)))
	  ((=fx c ncols))
	(vector-set! v i (proc (*fx 3 c) (+fx (*fx 2 r) (bitwise-and c 1))))))

    (make-harr nrows ncols v)))


(define (harr-for-each proc harr)
  (vector-for-each proc (harr:elts harr)))

;*---------------------------------------------------------------------*/
;*    hex.scm                                                          */
;*---------------------------------------------------------------------*/
;;; Hexagonal hackery for maze generation.
;;; Copyright (c) 1995 by Olin Shivers.

;;; Every elt of the hex array manages his SW, S, and SE wall.
;;; Terminology: - An even column is one whose column index is even. That
;;;                means the first, third, ... columns (indices 0, 2, ...).
;;;              - An odd column is one whose column index is odd. That
;;;                means the second, fourth... columns (indices 1, 3, ...).
;;;              The even/odd flip-flop is confusing; be careful to keep it
;;;              straight. The *even* columns are the low ones. The *odd*
;;;              columns are the high ones.
;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/
;;;  0 1 2 3


(define south-west 1)
(define south      2)
(define south-east 4)

(define (gen-maze-array r c)
  (harr-tabulate r c (lambda (x y) (my-make-box (base-set 1) (cons x y)))))

;;; This could be made more efficient.
(define (make-wall-vec harr)
  (let* ((nrows (harr:nrows harr))
	 (ncols (harr:ncols harr))
	 (xmax (*fx 3 (-fx ncols 1)))

	 ;; Accumulate walls.
	 (walls '())
	 (add-wall (lambda (o n b) ; owner neighbor bit
		     (set! walls (cons (make-wall o n b) walls)))))
	
    ;; Do everything but the bottom row.
    (do ((x (*fx (-fx ncols 1) 3) (-fx x 3)))
	((<fx x 0))
      (do ((y (+fx (*fx (-fx nrows 1) 2) (bitwise-and x 1))
	      (-fx y 2)))
	  ((<=fx y 1))	; Don't do bottom row.
	  (let ((hex (href harr x y)))
	    (if (not (zerofx? x))
		(add-wall hex (href harr (-fx x 3) (-fx y 1)) south-west))
	    (add-wall hex (href harr x (-fx y 2)) south)
	    (if (<fx x xmax)
		(add-wall hex (href harr (+fx x 3) (-fx y 1)) south-east)))))

    ;; Do the SE and SW walls of the odd columns on the bottom row.
    ;; If the rightmost bottom hex lies in an odd column, however,
    ;; don't add it's SE wall -- it's a corner hex, and has no SE neighbor.
    (if (>fx ncols 1)
	(let ((rmoc-x (+fx 3 (*fx 6 (/fx (-fx ncols 2) 2)))))
	  ;; Do rightmost odd col.
	  (let ((rmoc-hex (href harr rmoc-x 1)))
	    (if (<fx rmoc-x xmax) ; Not  a corner -- do E wall.
		(add-wall rmoc-hex (href harr xmax 0) south-east))
	    (add-wall rmoc-hex (href harr (-fx rmoc-x 3) 0) south-west))

	  (do ((x (-fx rmoc-x 6) ; Do the rest of the bottom row's odd cols.
		  (-fx x 6)))
	      ((<fx x 3))	; 3 is X coord of leftmost odd column.
	    (add-wall (href harr x 1) (href harr (-fx x 3) 0) south-west)
	    (add-wall (href harr x 1) (href harr (+fx x 3) 0) south-east))))

    (list->vector walls)))


;;; Find the box ctop from the top row, and the box cbot from the bottom
;;; row such that cbot is furthest from ctop. 
;;; Return [ctop-x, ctop-y, cbot-x, cbot-y].

(define (pick-entrances harr)
  (dfs-maze harr (href/rc harr 0 0) for-each-hex-child)
  (let ((nrows (harr:nrows harr))
	(ncols (harr:ncols harr)))
    (let tp-lp ((max-len -1)
		(entrance #f)
		(exit #f)
		(tcol (-fx ncols 1)))
      (if (<fx tcol 0) (values entrance exit)
	  (let ((top-box (href/rc harr (-fx nrows 1) tcol)))
	    (reroot-maze top-box)
	    (receive (max-len entrance exit)
		(let bt-lp ((max-len max-len)
			    (entrance entrance)
			    (exit exit)
			    (bcol (-fx ncols 1)))
		  (if (<fx bcol 0) (values max-len entrance exit)
		      (let ((this-len (path-length (href/rc harr 0 bcol))))
			(if (>fx this-len max-len)
			    (bt-lp this-len tcol bcol (-fx bcol 1))
			    (bt-lp max-len  entrance exit (-fx bcol 1))))))
	      (tp-lp max-len entrance exit (-fx tcol 1))))))))
		


;;; Apply PROC to each node reachable from BOX.
(define (for-each-hex-child proc harr box)
  (let* ((walls (box:walls box))
	 (id (box:id box))
	 (x (car id))
	 (y (cdr id))
	 (nr (harr:nrows harr))
	 (nc (harr:ncols harr))
	 (maxy (*fx 2 (-fx nr 1)))
	 (maxx (*fx 3 (-fx nc 1))))
    (if (not (bit-test walls south-west)) (proc (href harr (-fx x 3) (-fx y 1))))
    (if (not (bit-test walls south))      (proc (href harr x       (-fx y 2))))
    (if (not (bit-test walls south-east)) (proc (href harr (+fx x 3) (-fx y 1))))

    ;; NW neighbor, if there is one (we may be in col 1, or top row/odd col)
    (if (and (>fx x 0)	; Not in first column.
	     (or (<=fx y maxy)		; Not on top row or
		 (zerofx? (modulofx x 6))))	; not in an odd column.
	(let ((nw (href harr (-fx x 3) (+fx y 1))))
	  (if (not (bit-test (box:walls nw) south-east)) (proc nw))))

    ;; N neighbor, if there is one (we may be on top row).
    (if (<fx y maxy)		; Not on top row
	(let ((n (href harr x (+fx y 2))))
	  (if (not (bit-test (box:walls n) south)) (proc n))))

    ;; NE neighbor, if there is one (we may be in last col, or top row/odd col)
    (if (and (<fx x maxx)	; Not in last column.
	     (or (<=fx y maxy)		; Not on top row or
		 (zerofx? (modulofx x 6))))	; not in an odd column.
	(let ((ne (href harr (+fx x 3) (+fx y 1))))
	  (if (not (bit-test (box:walls ne) south-west)) (proc ne))))))



;;; The top-level
(define (make-maze nrows ncols)
   (let* ((boxs (gen-maze-array nrows ncols))
	  (walls (permute-vec! (make-wall-vec boxs) (random-state 20))))
      (dig-maze walls (*fx nrows ncols))
      (receive (entrance exit) (pick-entrances boxs)
	       (let* ((exit-box (href/rc boxs 0 exit))
		      (walls (box:walls exit-box)))
		  (reroot-maze (href/rc boxs (-fx nrows 1) entrance))
		  (mark-path exit-box)
		  (set-box:walls exit-box (bitwise-and walls (bitwise-not south)))
		  (values boxs entrance exit)))))


(define (pmaze nrows ncols)
  (receive (boxs entrance exit) (make-maze nrows ncols)
    (print-hexmaze boxs entrance)))


;*---------------------------------------------------------------------*/
;*    hexprint.scm                                                     */
;*---------------------------------------------------------------------*/
;;; Print out a hex array with characters.
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - hex array code
;;; - hex box code

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ 

;;; Top part of top row looks like this:
;;;    _   _  _   _
;;;  _/ \_/ \/ \_/ \
;;; /        

(define (print-hexmaze harr entrance)
  (let* ((nrows  (harr:nrows harr))
	 (ncols  (harr:ncols harr))
	 (ncols2 (*fx 2 (/fx ncols 2))))

    ;; Print out the flat tops for the top row's odd cols.
    (do ((c 1 (+fx c 2)))
	((>=fx c ncols))
      (display "   ")
      (write-char (if (=fx c entrance) #\space #\_)))
    (newline)

    ;; Print out the slanted tops for the top row's odd cols
    ;; and the flat tops for the top row's even cols.
    (write-char #\space)
    (do ((c 0 (+fx c 2)))
	((>=fx c ncols2))
      (display* (if (=fx c entrance) #\space #\_)
	      "/"
	      (dot/space harr (-fx nrows 1) (+fx c 1))
	      "\\"))
    (if (oddfx? ncols)
	(write-char (if (=fx entrance (-fx ncols 1)) #\space #\_)))
    (newline)

    (do ((r (-fx nrows 1) (-fx r 1)))
	((<fx r 0))

      ;; Do the bottoms for row r's odd cols.
      (write-char #\/)
      (do ((c 1 (+fx c 2)))
	  ((>=fx c ncols2))
	;; The dot/space for the even col just behind c.
	(write-char (dot/space harr r (-fx c 1)))
	(display-hexbottom (box:walls (href/rc harr r c))))	

      (cond ((oddfx? ncols)
	     (write-char (dot/space harr r (-fx ncols 1)))
	     (write-char #\\)))
      (newline)

      ;; Do the bottoms for row r's even cols.
      (do ((c 0 (+fx c 2)))
	  ((>=fx c ncols2))
	(display-hexbottom (box:walls (href/rc harr r c)))
	;; The dot/space is for the odd col just after c, on row below.
	(write-char (dot/space harr (-fx r 1) (+fx c 1))))
      
      (cond ((oddfx? ncols)
	     (display-hexbottom (box:walls (href/rc harr r (-fx ncols 1)))))
	    ((not (zerofx? r)) (write-char #\\)))
      (newline))))

(define (bit-test j bit)
  (not (zerofx? (bitwise-and j bit))))

;;; Return a . if harr[r,c] is marked, otherwise a space.
;;; We use the dot to mark the solution path.
(define (dot/space harr r c)
  (if (and (>=fx r 0) (box:mark (href/rc harr r c))) #\. #\space))

;;; Print a \_/ hex bottom.
(define (display-hexbottom hexwalls)
  (write-char (if (bit-test hexwalls south-west) #\\ #\space))
  (write-char (if (bit-test hexwalls south     ) #\_ #\space))
  (write-char (if (bit-test hexwalls south-east) #\/ #\space)))

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/


;*---------------------------------------------------------------------*/
;*    maze.scm                                                         */
;*---------------------------------------------------------------------*/
;;; Building mazes with union/find disjoint sets.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This is the algorithmic core of the maze constructor.
;;; External dependencies:
;;; - RANDOM-INT
;;; - Union/find code
;;; - bitwise logical functions

(define (my-make-box r i )
   (let ((x (make-box r i)))
      (if (not (eq? (box:parent x) #f))
	  (error "my-make-box" "Not #f parent" x)
	  x)))

;;; Iterates in reverse order.

(define (vector-for-each proc v)
  (let lp ((i (-fx (vector-length v) 1)))
    (cond ((>=fx i 0)
	   (proc (vector-ref v i))
	   (lp (-fx i 1))))))


;;; Randomly permute a vector.

(define (permute-vec! v random-state)
  (let lp ((i (-fx (vector-length v) 1)))
    (cond ((>fx i 1)
	   (let ((elt-i (vector-ref v i))
		 (j (random-int i random-state)))	; j in [0,i)
	     (vector-set! v i (vector-ref v j))
	     (vector-set! v j elt-i))
	   (lp (-fx i 1)))))
  v)


;;; This is the core of the algorithm.

(define (dig-maze walls nboxs)
  (bind-exit (quit)
    (begin
      (vector-for-each
       (lambda (wall)			; For each wall,
	 (let* ((c1   (wall:owner wall)) ; find the boxs on
		(set1 (box:reachable c1))

		(c2   (wall:neighbor wall)) ; each side of the wall
		(set2 (box:reachable c2)))

	   ;; If there is no path from c1 to c2, knock down the
	   ;; wall and union the two sets of reachable boxs.
	   ;; If the new set of reachable boxs is the whole set
	   ;; of boxs, quit.
	   (if (not (set-equal? set1 set2))
	       (let ((walls (box:walls c1))	
		     (wall-mask (bitwise-not (wall:bit wall))))
		 (union! set1 set2)
		 (set-box:walls c1 (bitwise-and walls wall-mask))
		 (if (=fx (set-size set1) nboxs) (quit #f))))))
       walls))))


;;; Some simple DFS routines useful for determining path length 
;;; through the maze.

;;; Build a DFS tree from ROOT. 
;;; (DO-CHILDREN proc maze node) applies PROC to each of NODE's children.
;;; We assume there are no loops in the maze; if this is incorrect, the
;;; algorithm will diverge.

(define (dfs-maze maze root do-children)
   (let search ((node root) (parent #f))
      (set-box:parent node parent)
      (do-children (lambda (child)
		      (if (not (eq? child parent))
			  (search child node)))
		   maze node)))

;;; Move the root to NEW-ROOT.

(define (reroot-maze new-root)
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   (let lp ((node new-root) (new-parent #f))
      (let ((old-parent (box:parent node)))
	 (set-box:parent node new-parent)
	 (if old-parent (lp old-parent node)))))

;;; How far from BOX to the root?

(define (path-length box)
  (do ((len 0 (+fx len 1))
       (node (box:parent box) (box:parent node)))
      ((not node) len)))

;;; Mark the nodes from NODE back to root. Used to mark the winning path.

(define (mark-path node)
  (let lp ((node node))
    (set-box:mark node #t)
    (cond ((box:parent node) => lp))))

;*---------------------------------------------------------------------*/
;*    rand.scm                                                         */
;*---------------------------------------------------------------------*/
; Minimal Standard Random Number Generator
; Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
; better constants, as proposed by Park.
; By Ozan Yigit

;;; Rehacked by Olin 4/1995.

(define (random-state n)
  (cons n #f))

(define (rand state)
  (let ((seed (car state))
	(A 48271)
	(M 268435455) ;; 2147483647
	(Q 44488)
	(R 3399))
    (let* ((hi (/fx seed Q))
	   (lo (modulofx seed Q))
	   (test (-fx (*fx A lo) (*fx R hi)))
	   (val (if (>fx test 0) test (+fx test M))))
      (set-car! state val)
      val)))

(define (random-int n state)
  (modulo (rand state) n))

; poker test
; seed 1
; cards 0-9 inclusive (random 10)
; five cards per hand
; 10000 hands
;
; Poker Hand     Example    Probability  Calculated
; 5 of a kind    (aaaaa)      0.0001      0
; 4 of a kind    (aaaab)      0.0045      0.0053
; Full house     (aaabb)      0.009       0.0093
; 3 of a kind    (aaabc)      0.072       0.0682
; two pairs      (aabbc)      0.108       0.1104
; Pair           (aabcd)      0.504       0.501
; Bust           (abcde)      0.3024      0.3058

; (define (random n)
;   (let* ((M 2147483647)
; 	 (slop (modulofx M n)))
;     (let loop ((r (rand)))
;       (if (>fx r slop)
; 	  (modulofx r n)	
; 	  (loop (rand))))))
; 
; (define (rngtest)
;   (display "implementation ")
;   (srand 1)
;   (let loop ((n 0))
;     (if (<fx n 10000)
;         (begin
;          (rand)
;          (loop (1+ n)))))
;   (if (=fx *seed* 399268537)
;       (display "looks correct.")
;       (begin
;        (display "failed.")
;        (newline)
;        (display "   current seed ") (display *seed*)
;        (newline)
;        (display "   correct seed 399268537")))
;   (newline))

;*---------------------------------------------------------------------*/
;*    uf.scm                                                           */
;*---------------------------------------------------------------------*/
;;; Tarjan's amortised union-find data structure.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This data structure implements disjoint sets of elements.
;;; Four operations are supported. The implementation is extremely
;;; fast -- any sequence of N operations can be performed in time
;;; so close to linear it's laughable how close it is. See your
;;; intro data structures book for more. The operations are:
;;;
;;; - (base-set nelts) -> set
;;;   Returns a new set, of size NELTS.
;;;
;;; - (set-size s) -> integer
;;;   Returns the number of elements in set S.
;;;
;;; - (union! set1 set2)
;;;   Unions the two sets -- SET1 and SET2 are now considered the same set
;;;   by SET-EQUAL?.
;;;
;;; - (set-equal? set1 set2)
;;;   Returns true <==> the two sets are the same.

;;; Representation: a set is a cons box. Every set has a "representative"
;;; cons box, reached by chasing cdr links until we find the cons with
;;; cdr = (). Set equality is determined by comparing representatives using
;;; EQ?. A representative's car contains the number of elements in the set.

;;; The speed of the algorithm comes because when we chase links to find 
;;; representatives, we collapse links by changing all the boxs in the path
;;; we followed to point directly to the representative, so that next time
;;; we walk the cdr-chain, we'll go directly to the representative in one hop.


(define (base-set nelts) (cons nelts '()))

;;; Sets are chained together through cdr links. Last guy in the chain
;;; is the root of the set.

(define (get-set-root s)
  (let lp ((r s))			; Find the last pair
    (let ((next (cdr r)))		; in the list. That's
      (cond ((pair? next) (lp next))	; the root r.

	    (else
	     (if (not (eq? r s))	; Now zip down the list again,
		 (let lp ((x s))	; changing everyone's cdr to r.
		   (let ((next (cdr x)))	
		     (cond ((not (eq? r next))
			    (set-cdr! x r)
			    (lp next))))))
	     r)))))			; Then return r.

(define (set-equal? s1 s2) (eq? (get-set-root s1) (get-set-root s2)))

(define (set-size s) (car (get-set-root s)))

(define (union! s1 s2)
  (let* ((r1 (get-set-root s1))
	 (r2 (get-set-root s2))
	 (n1 (set-size r1))
	 (n2 (set-size r2))
	 (n  (+fx n1 n2)))

    (cond ((>fx n1 n2)
	   (set-cdr! r2 r1)
	   (set-car! r1 n))
	  (else
	   (set-cdr! r1 r2)
	   (set-car! r2 n)))))

(define (main argv)
   (let ((num (if (pair? (cdr argv))
		  (string->integer (cadr argv))
		  50)))
      (pmaze num 35)))

