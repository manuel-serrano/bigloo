;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-debug.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 09:30:19 2023                          */
;*    Last change :  Thu Jun 27 17:48:54 2024 (serrano)                */
;*    Copyright   :  2023-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    bbv debugging tools                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-debug
   
   (include "Tools/trace.sch"
	    "Tools/location.sch"
	    "SawMill/regset.sch"
	    "SawMill/bbset.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    type_typeof
	    tools_shape
	    tools_speek
	    backend_backend
	    type_cache
	    saw_lib
	    saw_defs
	    saw_regset
	    saw_regutils
	    saw_bbv-config
	    saw_bbv-types
	    saw_bbv-specialize
	    saw_bbv-cache
	    saw_bbv-range)

   (export  (gendebugid)
	    (dump-cfg ::global ::pair-nil ::pair-nil ::bstring)
	    (dump-json-cfg ::global ::pair-nil ::obj ::pair-nil ::bstring)
	    (log-blocks ::global ::pair-nil ::pair-nil)
	    (log-blocks-history::bstring ::global ::pair-nil ::pair-nil)
	    (assert-block ::blockS ::obj)
	    (assert-blocks ::blockS ::obj)
	    (assert-context! b::blockS)
	    (rtl-assert-reg-type ::rtl_reg ::type ::bool ::bbv-ctx ::obj ::bstring)
	    (rtl-assert-expr-type reg::rtl_reg type::type polarity ctx loc tag)
	    (rtl-assert-fxcmp ::symbol x y ::bool ::bbv-ctx ::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    dump-blocks-error ...                                            */
;*---------------------------------------------------------------------*/
(define dump-blocks-error #f)

;*---------------------------------------------------------------------*/
;*    debugcnt ...                                                     */
;*---------------------------------------------------------------------*/
(define debugcnt 0)

;*---------------------------------------------------------------------*/
;*    gendebugid ...                                                   */
;*---------------------------------------------------------------------*/
(define (gendebugid)
   (set! debugcnt (+fx debugcnt 1))
   debugcnt)

;*---------------------------------------------------------------------*/
;*    assert-block ...                                                 */
;*    -------------------------------------------------------------    */
;*    A debug fonction that tests the consistency of the preds,        */
;*    succs, and branch instructions.                                  */
;*---------------------------------------------------------------------*/
(define (assert-block b::blockS stage)
   (with-access::blockS b (preds succs first label parent gccnt creator mblock asleep)
      (unless asleep
	 ;; check that cnt and preds are in sync
	 (unless (or (=fx gccnt (length preds)) (eq? creator 'root))
	    (tprint (shape b))
	    (error stage 
	       (format "predecessors and cnt not in sync #~a" label)
	       (format "preds.len=~a gccnt=~a" (length preds) gccnt)))
	 ;; check that b in the preds.succs
	 (let ((l (filter (lambda (p)
			     (with-access::blockS p (succs asleep)
				(and (not asleep) (not (memq b succs)))))
		     preds)))
	    (when (pair? l)
	       (tprint (shape b))
	       (for-each (lambda (b) (tprint (shape b))) l)
	       (error stage 
		  (format "predecessors not pointing to #~a" label)
		  (map (lambda (b) (format "#~a" (block-label b))) l))))
	 ;; check that b in the succs.preds
	 (let ((l (filter (lambda (p)
			     (with-access::blockS p (preds asleep)
				(and (not asleep) (not (memq b preds)))))
		     succs)))
	    (when (pair? l)
	       (tprint (shape b))
	       (for-each (lambda (b) (tprint (shape b))) l)
	       (error stage
		  (format "successors not pointing to #~a" label)
		  (map (lambda (b) (format "#~a" (block-label b))) l))))
	 ;; check that the instructions are in the succs
	 (let ((l '()))
	    (when (block-live? b)
	       (for-each (lambda (ins)
			    (cond
			       ((rtl_ins-ifeq? ins)
				(set! l (cons ins l)))
			       ((rtl_ins-ifne? ins)
				(with-access::rtl_ins ins (fun)
				   (with-access::rtl_ifne fun (then)
				      (unless (memq then succs)
					 (set! l (cons ins l))))))
			       ((rtl_ins-go? ins)
				(with-access::rtl_ins ins (fun)
				   (with-access::rtl_go fun (to)
				      (unless (memq to succs)
					 (set! l (cons ins l))))))
			       ((rtl_ins-switch? ins)
				(with-access::rtl_ins ins (fun)
				   (with-access::rtl_switch fun (labels)
				      (for-each (lambda (lbl)
						   (unless (memq lbl succs)
						      (set! l (cons ins l))))
					 labels))))))
		  first)
	       (when (pair? l)
		  (tprint "wrong block: " (shape b))
		  (tprint "parent block: " (shape parent))
		  (error stage
		     (format "instruction target not in succs of #~a" label)
		     (map shape l))))))))

;*---------------------------------------------------------------------*/
;*    dump-cfg ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-cfg global params blocks suffix)

   (define oname
      (if (string? *dest*)
	  *dest*
	  (if (and (pair? *src-files*) (string? (car *src-files*)))
	      (prefix (car *src-files*))
	      "./a.out")))
   
   (define filename
      (string-replace (format "~a-~a~a" oname (global-id global) suffix)
	 #\/ #\_))
   
   (define name (prefix filename))
      
   (define (dump-cfg port)
      (let* ((id (global-id global)))
	 (fprint port ";; -*- mode: bee -*-")
	 (fprint port ";; *** " id ":")
	 (fprintf port ";; BIGLOOBBVVLENGTH=\"~a\" BIGLOOBBVVERSIONLIMIT=\"~a\" BIGLOOBBVSTRATEGY=\"~a\" ~( )\n"
	    (or (getenv "BIGLOOBBVVLENGTH") "false")
	    (or (getenv "BIGLOOBBVVERSIONLIMIT") "4")
	    (or (getenv "BIGLOOBBVSTRATEGY") "size")
	    (command-line))
	 (fprintf port ";; bglcfg '~a' > '~a.dot' && dot '~a.dot' -Tpdf > '~a.pdf'\n"
	    filename name name name)
	 (for-each (lambda (b)
		      (dump b port 0)
		      (newline port))
	    blocks)
	 id))
   
   (if oname
       (call-with-output-file filename dump-cfg)
       (dump-cfg (current-error-port))))

;*---------------------------------------------------------------------*/
;*    shape-ctx-entry ...                                              */
;*---------------------------------------------------------------------*/
(define (shape-ctx-entry e)
   (with-access::bbv-ctxentry e (reg types polarity value aliases count)
      (if (and (=fx (length types) 1)
	       (eq? (car types) *obj*)
	       (eq? value '_)
	       (null? aliases))
	  (shape reg)
	  (cons (shape reg)
	     (cond
		((not (eq? value '_))
		 (list (format "~a:~a" (shape value) count)))
		(else
		 (if polarity
		     (map (lambda (t) (format "~a:~a" (shape t) count)) types)
		     (map (lambda (t) (format "!~a" (shape t))) types))))))))

;*---------------------------------------------------------------------*/
;*    dump-json-cfg ...                                                */
;*---------------------------------------------------------------------*/
(define (dump-json-cfg global params history blocks suffix)
   
   (define oname
      (if (string? *dest*)
	  *dest*
	  (if (and (pair? *src-files*) (string? (car *src-files*)))
	      (prefix (car *src-files*))
	      "./a.out")))
   
   (define filename
      (string-replace (format "~a-~a~a" oname (global-id global) suffix)
	 #\/ #\_))
   
   (define name (prefix filename))

   (define (loc first)
      (let ((i (find (lambda (i)
			  (with-access::rtl_ins i (loc) loc))
		    first)))
	 (when i
	    (with-access::rtl_ins i (loc)
	       (when (location? loc)
		  (format "~a:~a" (basename (location-fname loc)) (location-lnum loc)))))))

   (define (dump-ins o::rtl_ins/bbv p)
      (with-access::rtl_ins/bbv o (fun dest args)
	 (when dest
	    (display "(" p)
	    (dump dest p 0)
	    (display " <- " p))
	 (dump-ins-rhs o p 0)
	 (when dest
	    (display ")" p))
	 (newline p)))

   (define (dump-blockS o::blockS p m sep)
      
      (define (lbl n)
	 (if (isa? n block) (block-label n) (typeof n)))
      
      (with-access::blockS o (label collapsed first parent ctx preds succs)
	 (dump-margin p m)
	 (fprint p "{")
	 (dump-margin p (+fx m 2))
	 (fprint p "\"id\": " label ",")
	 (dump-margin p (+fx m 2))
	 (fprint p "\"origin\": " (block-label parent) ",")
	 (dump-margin p (+fx m 2))
	 (fprintf p "\"bbs\": \"~a\",\n" (global-id global))
	 (dump-margin p (+fx m 2))
	 (fprintf p "\"loc\": \"~a\",\n" (loc first))
	 (dump-margin p (+fx m 2))
	 (fprint p "\"usage\": " 0 ",")
	 (dump-margin p (+fx m 2))
	 (fprintf p "\"context\": ~s,\n"
	    (call-with-output-string
	       (lambda (op)
		  (for-each (lambda (e)
			       (display (shape-ctx-entry e) op)
			       (display " " op))
		     (with-access::bbv-ctx ctx (entries)
			entries)))))
	 (dump-margin p (+fx m 2))
	 (fprint p "\"predecessors\": [" (format "~(, )" (map lbl preds)) "],")
	 (dump-margin p (+fx m 2))
	 (fprint p "\"successors\": [" (format "~(, )" (map lbl succs)) "],")
	 (dump-margin p (+fx m 2))
	 (fprintf p "\"details\": ~s\n"
	    (call-with-output-string
	       (lambda (op)
		  (for-each (lambda (i) (dump-ins i op)) first))))
	 (dump-margin p m)
	 (fprint p "}" sep)))
   
   (define (dump-cfg port)
      (let* ((id (global-id global)))
	 (fprint port "{")
	 (fprint port "  \"compiler\": \"bigloo\",")
	 (fprintf port "  \"version-limit\": ~a,\n"
	    (or (getenv "BIGLOOBBVVERSIONLIMIT") "4"))
	 (fprintf port "  \"vector-length\": \"~a\",\n"
	    (or (getenv "BIGLOOBBVVLENGTH") "false"))
	 (fprintf port "  \"merge-strategy\": \"~a\",\n"
	    (or (getenv "BIGLOOBBVSTRATEGY") "size"))
	 (fprint port "  \"specializedCFG\": [")

	 (format "~(, )"
	    (map (lambda (b) (dump-blockS b port 4 ","))
	       (sort-blocks blocks)))

	 (display "    null" port)
	 (if history
	     (begin
		(fprint port "  ],")
		(fprint port "  \"history\": " history))
	     (fprint port "  ]"))
		
	 (fprint port "}")
	 id))
   
   (if oname
       (call-with-output-file filename dump-cfg)
       (dump-cfg (current-error-port))))

   
;*---------------------------------------------------------------------*/
;*    make-margins ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (make-margins n)
   `',(apply vector (map (lambda (n) (make-string n #\space)) (iota n))))

;*---------------------------------------------------------------------*/
;*    sort-blocks ...                                                  */
;*---------------------------------------------------------------------*/
(define (sort-blocks b)
   (sort (lambda (x y) (<=fx (block-label x) (block-label y))) b))

;*---------------------------------------------------------------------*/
;*    log-blocks ...                                                   */
;*---------------------------------------------------------------------*/
(define (log-blocks global::global params::pair-nil bv::pair-nil)

   

   (define margins
      (make-margins 40))

   (define (log-label block #!optional prefix)
      (if prefix
	  (string-append prefix (integer->string (block-label block)))
	  (integer->string (block-label block))))
   
   (define (paddingl n str)
      (if str
	  (let ((len (string-length str)))
	     (if (<fx len n)
		 (string-append (vector-ref margins (-fx n len)) str)
		 str))
	  (vector-ref margins n)))

   (define (paddingr n str)
      (if str
	  (let ((len (string-length str)))
	     (if (<fx len n)
		 (string-append str (vector-ref margins (-fx n len)))
		 str))
	  (vector-ref margins n)))
      
   (define (log-entry e)
      (with-access::bbv-ctxentry e (reg types polarity value aliases count)
	 (if (pair? aliases)
	     (format "~a:[~( )~a ~( )]"
		(shape reg)
		(if polarity
		    (map (lambda (t) (format "~a:~a" (shape t) count)) types)
		    (map (lambda (t) (format "!~a" (shape t))) types))
		(if (eq? value '_) "" (format " ~a" (shape value)))
		(map shape aliases))
	     (format "~a:[~( )~a]"
		(shape reg)
		(if polarity
		    (map (lambda (t) (format "~a:~a" (shape t) count)) types)
		    (map (lambda (t) (format "!~a" (shape t))) types))
		(if (eq? value '_) "" (format " ~a" (shape value)))))))
   
   (define (log-ctx ctx)
      (with-access::bbv-ctx ctx (id entries)
	 ;;(format "~4d ~( )" id (map log-entry entries))
	 (format "~( )" (map log-entry entries))))
   
   (define (log-creator mblock creator)
      (cond
	 ((pair? creator)
	  (format "<- ~a+~a"
	     (block-label (car creator))
	     (block-label (cdr creator))))
	 ((isa? creator block)
	  (format "<- ~a" (block-label creator)))
	 (else
	  (format "<- ~a" creator))))
   
   (define (log-merges mblock merges)
      (format "~(, )"
	 (map (lambda (m)
		 (format "~a+~a" (block-label (car m)) (block-label (cdr m))))
	    merges)))

   (define (print-bs b::blockS)
      (with-access::blockS b (creator merges ctx mblock)
	 (print " "
	    (paddingl 4 (log-label b))
	    (paddingr 6 (if mblock (log-label mblock "~>")))
	    (paddingr 12 (log-creator mblock creator))
	    " " (paddingl 30 (log-merges mblock merges))
	    " " (log-ctx ctx))))
      
   (define (print-bv b::blockV)
      (with-access::blockV b (label versions)
	 (print "-----------------------------------------------------------")
	 (print "{" label "}")
	 (for-each print-bs (sort-blocks versions))))
      
   ;; sort the blocks according to the parent label
   (with-output-to-port (current-error-port)
      (lambda ()
	 (print "==== " (shape global) " " (map shape params))
	 (for-each print-bv (sort-blocks bv))
	 (newline))))

;*---------------------------------------------------------------------*/
;*    log-blocks-history ...                                           */
;*---------------------------------------------------------------------*/
(define (log-blocks-history global params blocks)

   (define (sort-blocks b)
      (sort (lambda (x y) (<= (block-label x) (block-label y))) b))

   (define (log-blockS-creator b::blockS p m)
      (with-access::blockS b (label versions creator ctx)
	 (dump-margin p m)
	 (fprint p "{")
	 (dump-margin p (+fx m 2))
	 (fprint p "\"id\": " label ",")
	 (dump-margin p (+fx m 2))
	 (fprint p "\"origin\": " (blockV-label (blockS-parent b)) ",")
	 (dump-margin p (+fx m 2))
	 (fprintf p "\"bbs\": \"~a\",\n" (global-id global))
	 (dump-margin p (+fx m 2))
	 (fprintf p "\"context\": ~s,\n"
	    (call-with-output-string
	       (lambda (op)
		  (for-each (lambda (e)
			       (display (shape-ctx-entry e) op)
			       (display " " op))
		     (with-access::bbv-ctx ctx (entries)
			entries)))))
	 (dump-margin p (+fx m 2))
	 (cond
	    ((isa? creator block)
	     (fprint p "\"event\": \"create\",")
	     (dump-margin p (+fx m 2))
	     (fprintf p "\"from\": ~a\n" (block-label creator)))
	    ((symbol? creator)
	     (fprint p "\"event\": \"create\",")
	     (dump-margin p (+fx m 2))
	     (fprintf p "\"from\": \"~a\"\n" creator))
	    ((pair? creator)
	     (fprint p "\"event\": \"mergeCreate\",")
	     (dump-margin p (+fx m 2))
	     (fprintf p "\"merged\": [~a, ~a]\n"
		(block-label (car creator))
		(block-label (cdr creator))))
	    (else
	     (error "log-blocks-history"
		(format "missing block creator (~a)" label) creator)))
	 (dump-margin p m)
	 (fprint p "},")))

   (define (log-blockS-merges b::blockS p m)
      (with-access::blockS b (label versions merges mblock)
	 (for-each (lambda (e)
		      (dump-margin p m)
		      (fprint p "{")
		      (dump-margin p (+fx m 2))
		      (fprint p "\"id\": " label ",")
		      (dump-margin p (+fx m 2))
		      (fprint p "\"event\": \"merge\",")
		      (dump-margin p (+fx m 2))
		      (fprintf p "\"merged\": [~a, ~a]\n"
			 (block-label (car e))
			 (block-label (cdr e)))
		      (dump-margin p m)
		      (fprint p "},"))
	    merges)))
   
   (define (log-blockV-creator b::blockV p m)
      (with-access::blockV b (label versions)
	 (for-each (lambda (b) (log-blockS-creator b p m))
	    (sort-blocks versions))))
   
   (define (log-blockV-merges b::blockV p m)
      (with-access::blockV b (label versions)
	 (for-each (lambda (b) (log-blockS-merges b p m))
	    (sort-blocks versions))))

   (call-with-output-string
      (lambda (port)
	 (fprint port "[")
	 (for-each (lambda (b) (log-blockV-creator b port 4))
	    (sort-blocks blocks))
	 (for-each (lambda (b) (log-blockV-merges b port 4))
	    (sort-blocks blocks))
	 (display "    null\n" port)
	 (fprint port "  ]"))))

;*---------------------------------------------------------------------*/
;*    assert-blocks ...                                                */
;*    -------------------------------------------------------------    */
;*    Coalesce requires dangling blocks to be removed from             */
;*    parent.versions. This pass implements a simple mark&sweep        */
;*    collectors for basic blocks.                                     */
;*---------------------------------------------------------------------*/
(define (assert-blocks b::blockS lbl)
   (when *bbv-debug*
      (let loop ((bs (list b))
		 (set (make-empty-bbset)))
	 (cond
	    ((null? bs)
	     set)
	    ((bbset-in? (car bs) set)
	     (loop (cdr bs) set))
	    (else
	     (with-access::blockS (car bs) (succs)
		(assert-block (car bs) lbl)
		(loop (append succs (cdr bs))
		   (bbset-cons (car bs) set))))))))

;*---------------------------------------------------------------------*/
;*    reg-debugname ...                                                */
;*---------------------------------------------------------------------*/
(define (reg-debugname reg::rtl_reg)
   (with-access::rtl_reg reg (var debugname key)
      (cond
	 (debugname
	  debugname)
	 ((global? var)
	  (with-access::global var (id module alias)
	     (set! debugname
		(bigloo-module-mangle (symbol->string (or alias id))
		   (symbol->string module)))
	     debugname))
	 ((local? var)
	  (with-access::local var (id)
	     (set! debugname
		(bigloo-mangle
		   (string-append
		      (symbol->string id) "_" (symbol->string key))))
	     debugname))
	 (else
	  (set! debugname (symbol->string (gensym 'bbv)))
	  debugname))))

;*---------------------------------------------------------------------*/
;*    type-predicate ...                                               */
;*---------------------------------------------------------------------*/
(define (type-predicate type)
   (cond
      ((eq? type *long*) "INTEGERP")
      ((eq? type *bint*) "INTEGERP")
      ((eq? type *real*) "FLONUMP")
      ((eq? type *breal*) "FLONUMP")
      ((eq? type *vector*) "VECTORP")
      ((eq? type *pair*) "PAIRP")
      ((eq? type *string*) "STRINGP")
      ((eq? type *bstring*) "STRINGP")
      ((eq? type *char*) "CHARP")
      ((eq? type *bchar*) "CHARP")
      ((eq? type *bnil*) "NULLP")
      ((eq? type *procedure*) "PROCEDUREP")
      ((eq? type *output-port*) "OUTPUT_PORTP")
      ((eq? type *bool*) "BOOLEANP")
      ((eq? type *bbool*) "BOOLEANP")
      ((eq? type *symbol*) "SYMBOLP")
      (else #f)))

;*---------------------------------------------------------------------*/
;*    assert-cnt ...                                                   */
;*---------------------------------------------------------------------*/
(define assert-cnt 0)

;*---------------------------------------------------------------------*/
;*    assert-failure ...                                               */
;*---------------------------------------------------------------------*/
(define (assert-failure pred reg polarity loc #!optional (tag "BBV-ASSERT-FAILURE"))
   (let ((debugname (reg-debugname reg)))
      (set! assert-cnt (+fx assert-cnt 1))
      (if (location? loc)
	  (format "(fprintf(stderr, \"*** ~a[~a]:%s:%d\\n%s: %s::~a%s (%s:%d)\\n\", __FILE__, __LINE__, \"~a\", \"~a\", BSTRING_TO_STRING(bgl_typeof(~a)), \"~a\", ~a), exit(127), 0)"
	     tag assert-cnt (if polarity "" "!") pred debugname debugname (location-fname loc) (location-pos loc))
	  (format "(fprintf(stderr, \"*** ~a[~a]:%s:%d\\n%s: %s::~a%s\\n\", __FILE__, __LINE__, \"~a\", \"~a\", BSTRING_TO_STRING(bgl_typeof(~a))), exit(127), 0)"
	     tag assert-cnt (if polarity "" "!") pred debugname debugname))))

;*---------------------------------------------------------------------*/
;*    assert-context! ...                                              */
;*    -------------------------------------------------------------    */
;*    Insert assertion to verify dynamically the correction of the     */
;*    specialized CFG.                                                 */
;*---------------------------------------------------------------------*/
(define (assert-context! b::blockS)
   
   (define (pragma-ins cexpr::bstring ctx loc)
      (let ((pr (instantiate::rtl_pragma
		   (srfi0 'bigloo-c)
		   (format cexpr)))
	    (es (instantiate::regset (msize 0) (regv '#()) (regl '()))))
	 (instantiate::rtl_ins/bbv
	    (loc loc)
	    (fun pr)
	    (ctx ctx)
	    (in es)
	    (out es)
	    (def es)
	    (args '()))))
   
   (define (type-predicates types)
      (when (pair? types)
	 (filter (lambda (x) x) (map type-predicate types))))
   
   (define (ctx-assert ctx::bbv-ctx loc)
      (with-access::bbv-ctx ctx (entries)
	 (let ((checks (filter-map (lambda (e)
				      (with-access::bbv-ctxentry e (reg types polarity)
					 (with-access::rtl_reg reg (debugname name var type)
					    (when (and (bigloo-type? type)
						       (not (eq? type *procedure-el*))
						       (not (eq? type *procedure*)))
					       (let ((preds (type-predicates types)))
						  (when (pair? preds)
						     (format "~( && )"
							(map (lambda (p)
								(format "(~a~a(~a) || ~a)"
								   (if polarity "" "!")
								   p
								   (reg-debugname reg)
								   (assert-failure p reg polarity loc)))
							   preds))))))))
			  entries)))
	    (when (pair? checks)
	       (format "~( \n         && )" checks)))))
   
   
   (define (assert-ins i::rtl_ins/bbv first)
      (when (or first (>=fx *bbv-assert* 2))
	 (with-access::rtl_ins/bbv i (loc ctx)
	    (let ((assert (ctx-assert ctx loc)))
	       (when (string? assert)
		  (with-access::bbv-ctx ctx (entries)
		     (pragma-ins assert ctx loc)))))))
   
   (assert-blocks b "before assert!")
   (if (>=fx *bbv-assert* 1)
       (let loop ((bs (list b))
		  (acc (make-empty-bbset)))
	  (cond
	     ((null? bs)
	      b)
	     ((bbset-in? (car bs) acc)
	      (loop (cdr bs) acc))
	     (else
	      (with-access::blockS (car bs) (first succs preds label ctx)
		 (when *bbv-debug* (assert-block (car bs) "assert-context!"))
		 (let loop ((first first)
			    (acc '()))
		    (if (null? first)
			(block-first-set! (car bs) (reverse! acc))
			(let* ((ins (car first))
			       (assert (assert-ins ins (null? acc))))
			   (with-access::rtl_ins/bbv ins (ctx)
			      (loop (cdr first)
				 (if assert
				     (cons* ins assert acc)
				     (cons ins acc)))))))
		 (loop (append succs (cdr bs)) (bbset-cons (car bs) acc))))))
       b))

;*---------------------------------------------------------------------*/
;*    rtl-assert-reg-type ...                                          */
;*---------------------------------------------------------------------*/
(define (rtl-assert-reg-type reg::rtl_reg type::type polarity ctx loc tag)
   (let ((pred (type-predicate type)))
      (if pred
	  (let ((debugname (reg-debugname reg)))
	     (let ((cexpr (format "/* rtl-assert-reg-type */ (~a~a(~a) || ~a)"
			     (if polarity "" "!")
			     pred
			     debugname
			     (assert-failure pred reg loc tag))))
		(instantiate::rtl_pragma
		   (srfi0 'bigloo-c)
		   (format cexpr))))
	  (begin
	     (tprint "PAS DE TYPE PRED " (shape type) " " (eq? type *bnil*))
	     #f))))

;*---------------------------------------------------------------------*/
;*    rtl-assert-expr-type ...                                         */
;*---------------------------------------------------------------------*/
(define (rtl-assert-expr-type reg::rtl_reg type::type polarity ctx loc tag)
   (let ((debugname (reg-debugname reg)))
      (let ((cexpr (format "/* rtl-assert-expr-type */ ($1 || ~a)"
		      (assert-failure (or (type-predicate type) "???") reg loc tag))))
	 (instantiate::rtl_pragma
	    (srfi0 'bigloo-c)
	    (format cexpr)))))

;*---------------------------------------------------------------------*/
;*    rtl-assert-fxcmp ...                                             */
;*---------------------------------------------------------------------*/
(define (rtl-assert-fxcmp op x y polarity ctx loc tag)
   
   (define (assert-fxcmp-failure loc tag)
      (set! assert-cnt (+fx assert-cnt 1))
      (if (location? loc)
	  (format "(fprintf(stderr, \"*** ~a[~a]:%s:%d\\n%s: %s:%d\\n\", __FILE__, __LINE__, \"~a\", \"~a\", ~a), exit(127), 0)"
	     tag assert-cnt op (location-fname loc) (location-pos loc))
	  (format "(fprintf(stderr, \"*** ~a[~a]:%s:%d\\n%s\\n\", __FILE__, __LINE__, \"~a\"), exit(127), 0)"
	     tag assert-cnt op)))

   (define (compile-simple x)
      (cond
	 ((isa? x rtl_ins)
	  (with-access::rtl_ins x (fun args)
	     (cond
		((isa? fun rtl_loadi)
		 (with-access::rtl_loadi fun (constant)
		    (with-access::atom constant (value)
		       (format "CINT(~a)" value))))
		((isa? fun rtl_call)
		 (with-access::rtl_call fun (var)
		    (with-access::variable var (value type)
		       (if (eq? var *bint->long*)
			   (if (rtl_reg? (car args))
			       (format "CINT(~a)" (compile-simple (car args)))
			       (tprint "x=" (shape x)))
			   (tprint "y=" (shape x)))))
		 #f)
		(else
		 (tprint "z=" (shape x) " " (typeof fun))
		 #f))))
	 ((isa? x rtl_reg)
	  (reg-debugname x))
	 (else
	  (tprint "w=" (shape x) " " (typeof fun))
	  #f)))
   
   (let ((cx (compile-simple x))
	 (cy (compile-simple y)))
      (when (and cx cy)
	 (let ((cexpr (format "/* rtl-assert-fxcmp */ (~a((~a) ~a (~a)) || ~a)"
			 (if polarity "" "!")
			 cx (if (eq? op '=) "==" op) cy (assert-fxcmp-failure loc tag))))
	    (instantiate::rtl_pragma
	       (srfi0 'bigloo-c)
	       (format cexpr))))))


