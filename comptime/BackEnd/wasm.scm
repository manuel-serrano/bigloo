
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_wasm
    (include "Engine/pass.sch"
      "Tools/location.sch")
    (import engine_param
        engine_configure
        tools_license
        tools_error
        tools_shape
        tools_location
        backend_backend
        backend_cvm
        cnst_alloc
        
        backend_cplib
        module_module
        type_type
        type_cache
        type_tools
        type_env
        object_class
        object_slots
        ast_var
        ast_node
        ast_env
        ast_ident
        ast_occur
        
        ast_type-occur
        ast_pragma
        cgen_cop
        saw_wasm_compile
        saw_wasm_code
        (emit-bdb-loc cgen_emit-cop)
        type_tools
        cnst_node)
        
    (export (build-wasm-backend)
        *wasm-port*))

;*---------------------------------------------------------------------*/
;*    The backend                                                      */
;*---------------------------------------------------------------------*/
(register-backend! 'wasm build-wasm-backend)

;*---------------------------------------------------------------------*/
;*    build-jvm-backend ...                                            */
;*---------------------------------------------------------------------*/
(define (build-wasm-backend)
   (instantiate::wasm
      (language 'wasm)
      (heap-compatible 'c)
      (trace-support #f)
      (srfi0 'bigloo-wasm)
      (foreign-clause-support '(wasm extern))
      (strict-type-cast #t)
      (pragma-support #t)
      (require-tailc #t)
      ; TODO: maybe remove these two checks
      (bound-check #f)
      (type-check #f)
      (force-register-gc-roots #f)
      (string-literal-support #f)))

(define-method (backend-compile me::wasm)
    (wasm-walk me))

(define-method (backend-link-objects me::wasm sources)
    (tprint sources))

(define (require-prototype? global)
   (and (or (eq? (global-module global) *module*)
        (not (eq? (global-import global) 'static)))
    (or (and (eq? (global-module global) *module*)
         (eq? (global-import global) 'export))
        (>fx (global-occurrence global) 0)
        (eq? (global-removable global) 'never))))

(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read-line p) (cons line result)))))))

(define (wasm-walk me::wasm)
  (pass-prelude "WebAssembly generation"
    (lambda () (start-emission! ".wat")))

  (for-each-type! (lambda (t) (type-occurrence-set! t 0)))
  (for-each-global!
    (lambda (global)
      (cond
        ((and (eq? (global-module global) *module*)
              (>fx (global-occurrence global) 0))
          (type-increment-global! global))
        ((require-prototype? global)
          (type-increment-global! global)
          (type-occurrence-increment! (global-type global))
          (when (sfun? (global-value global))
            (for-each (lambda (a)
              (cond
                ((type? a)
                  (type-occurrence-increment! a))
                ((local? a)
                  (type-occurrence-increment! (local-type a)))))
            (sfun-args (global-value global))))))))

  (let ((fixpoint #f))
    (let loop ()
      (unless fixpoint
        (set! fixpoint #t)
        (for-each (lambda (t::tclass)
          (when (>fx (type-occurrence t) 0)
            (with-access::tclass t (its-super slots)
              (when (and its-super (=fx (type-occurrence its-super) 0))
                (type-occurrence-increment! its-super)
                (set! fixpoint #f))
              (for-each (lambda (t::slot)
                (with-access::slot t (type)
                  (when (=fx (type-occurrence type) 0)
                    (type-occurrence-increment! type)
                    (set! fixpoint #f))))
              slots))))
        (get-class-list)))))

  ;; Registers functions defined in this module to avoid importing them.
  ;; This is required as some files in the standard library redefine in Scheme
  ;; some C functions used by the C backend.
  (for-each (lambda (fun) 
    (set-variable-name! fun)
    (hashtable-put! *defined-ids* (global-name fun) #t)) 
    (cvm-functions me))

  (let ((compiled-funcs (backend-compile-functions me))
        (classes (filter (lambda (t) (>fx (type-occurrence t) 0)) (get-class-list))))

    (hashtable-put! *defined-ids* "BFALSE" #t)
    (hashtable-put! *defined-ids* "BTRUE" #t)
    (hashtable-put! *defined-ids* "BUNSPEC" #t)
    (hashtable-put! *defined-ids* "BOPTIONAL" #t)
    (hashtable-put! *defined-ids* "BKEY" #t)
    (hashtable-put! *defined-ids* "BREST" #t)
    (hashtable-put! *defined-ids* "BEOA" #t)

    (with-output-to-port *wasm-port* (lambda ()
      (wasm-pp
        `(module ,(wasm-sym (symbol->string *module*)) 
          (comment "Imports"
            (import "__runtime" "generic_va_call" (func $generic_va_call (param (ref $procedure)) (param (ref $vector)) (result eqref)))
            (import "__runtime" "BFALSE" (global $BFALSE i31ref))
            (import "__runtime" "BTRUE" (global $BTRUE i31ref))
            (import "__runtime" "BUNSPEC" (global $BUNSPEC i31ref))
            (import "__runtime" "BOPTIONAL" (global $BOPTIONAL i31ref))
            (import "__runtime" "BKEY" (global $BKEY i31ref))
            (import "__runtime" "BREST" (global $BREST i31ref))
            (import "__runtime" "BEOA" (global $BEOA i31ref))
            (import "__js" "trace" (func $__trace (param i32)))
            ,@(emit-imports))
          (comment "Memory" ,@(emit-memory))
          (comment "Primitive types" ,@(call-with-input-file "Wlib/runtime.types" port->sexp-list))
          (comment "Class types" ,@(emit-class-types classes))
          (comment "Globals" ,@(emit-prototypes))
          (comment "Constants" ,@(emit-cnsts))
          (comment "String data" ,@(emit-strings))
          (comment "Functions" ,@compiled-funcs))))))
  
  (stop-emission!))

(define *wasm-port* #f)

(define (wasm-pp l)
  (define (ppindent depth)
    (unless (=fx depth 0)
        (display "  ")
        (ppindent (-fx depth 1))))

  (letrec ((aux (lambda (l depth)
    (define (pp-args l)
      (for-each (lambda (n)
        (newline)
        (aux n (+fx depth 1))) l))

    (define (dump-string s)
      (define (visible? c)
        (and 
          (char>=? c #\x20) 
          (char<? c #\x7F) ;; exclude the DEL character (illegal in WASM text format)
          (not (char=? c #\")) 
          (not (char=? c #\\))))

      (display "\"")
      (let iter ((i 0))
        (when (<fx i (string-length s))
          (let ((c (string-ref s i))
                (hex "0123456789abcdef"))
            (cond 
              ((visible? c) (display c))
              ((char=? c #\") (display "\\\""))
              ((char=? c #\\) (display "\\\\"))
              (else (display* "\\" 
                (string-ref hex (bit-rsh (char->integer (char-and c #\xF0)) 4))
                (string-ref hex (char->integer (char-and c #\x0F)))
                ))))
          (iter (+fx i 1))))
      (display "\""))

    (define (pp-arg a)
      (cond
        ((elong? a) (display a))
        ((llong? a) (display a))
        ((bignum? a) (display a))
        ((string? a) (dump-string a))
        (else (write a))))

    (define (pp-0 l)
      (display "(")
      (write (car l))
      (pp-args (cdr l))
      (display ")"))

    (define (pp-1 l)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (pp-args (cddr l))
      (display ")"))

    (define (pp-2 l)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (display " ")
      (pp-arg (car (cddr l)))
      (pp-args (cdr (cddr l)))
      (display ")"))

    (define (pp-oneline l)
      (display "(")
      (write (car l))
      (map (lambda (a) (display " ") (pp-arg a)) (cdr l))
      (display ")"))

    (define (pp-comment l)
      (match-case l
        ((comment ?comment . ?nodes)
          (display* ";; " comment)
          (for-each (lambda (node) (newline) (aux node depth)) nodes))
        (else
          (error "wasm-pp" "Illegal form for comment node" l))))

    (define (pp-location l)
      (match-case l
        ((@ ?loc ?node)
          (display ";;@ ")
          (display* (location-fname loc) ":" (location-lnum loc) ":1\n")
          (aux node depth))
        (else
          (error "wasm-pp" "Illegal form for location node" l))))

    (ppindent depth)
    (cond 
      ((pair? l)
        (case (car l)
          ('comment (pp-comment l))
          ('@ (pp-location l))
          ('module (pp-1 l))
          ('import (pp-2 l))
          ('func (pp-1 l))
          ('type (pp-1 l))
          ('sub (pp-1 l))
          ('global (pp-1 l))
          ('memory (pp-oneline l))
          ('data (pp-1 l))
          ('elem (pp-oneline l))
          ('export (pp-oneline l))
          ('param (pp-oneline l))
          ('result (pp-oneline l))
          ('local (pp-oneline l))
          ('field (pp-oneline l))
          ('mut (pp-oneline l))
          ('i32.const (pp-oneline l))
          ('i64.const (pp-oneline l))
          ('f32.const (pp-oneline l))
          ('f64.const (pp-oneline l))
          ('ref (pp-oneline l))
          ('ref.null (pp-oneline l))
          ('local.get (pp-oneline l))
          ('global.get (pp-oneline l))
          ('local.set (pp-1 l))
          ('global.set (pp-1 l))
          ('br (pp-oneline l))
          ('unreachable (pp-oneline l))
          ('block (if (symbol? (cadr l)) (pp-1 l) (pp-0 l)))
          ('loop (if (symbol? (cadr l)) (pp-1 l) (pp-0 l)))
          ('call (pp-1 l))
          ('return_call (pp-1 l))
          ('call_ref (pp-1 l))
          ('return_call_ref (pp-1 l))
          ('struct.new (pp-1 l))
          ('struct.get (pp-2 l))
          ('array.get (pp-1 l))
          ('array.new (pp-1 l))
          ('array.new_elem (pp-2 l))
          (else (pp-0 l))))
      ((not l) 'nothing)
      (else (pp-arg l))))))
    (aux l 0))
  (newline))

;*---------------------------------------------------------------------*/
;*    start-emission! ...                                              */
;*---------------------------------------------------------------------*/
(define (start-emission! suffix)
   (let ((prefix 
        (when (string? *dest*) 
            (prefix *dest*))))
      (if (or (eq? *dest* '--to-stdout) (not (string? prefix)))
        (set! *wasm-port* (current-output-port))
        (let ((f-name (string-append prefix suffix)))
            (set! *wasm-port* (open-output-file f-name))
            (if (not (output-port? *wasm-port*))
            (error *bigloo-name* "Can't open file for output" f-name)
            #unspecified)))))

;*---------------------------------------------------------------------*/
;*    stop-emission! ...                                               */
;*---------------------------------------------------------------------*/
(define (stop-emission!)
   (cond
      ((not (output-port? *wasm-port*))
       #f)
      ((eq? *wasm-port* (current-output-port))
       #f)
      (else
       (flush-output-port *wasm-port*)
       (close-output-port *wasm-port*)
       (set! *wasm-port* #f))))

(define (class-refs class)
  (filter-map (lambda (slot)
    (with-access::slot slot (type)
      (isa? type tclass))) 
  (tclass-slots class)))

(define (scc-classes class-list)
  (define (splitf-at list f)
    (let helper ((left '())
                (right list))
      (if (and (not (null? right)) (f (car right)))
        (helper (cons (car right) left) (cdr right))
        (values (reverse left) right))))

  (let ((indexes '())
        (index 0)
        (low-links '())
        (on? '())
        (stack '())
        (sccs '()))

  (define (dfs class)
    (set! indexes (cons (cons class index) indexes))
    (set! low-links (cons (cons class index) low-links))
    (set! on? (cons (cons class #t) on?))
    (set! index (+fx index 1))
    (set! stack (cons class stack))

    (for-each (lambda (w) 
      (let ((found-index (assq w indexes))
            (found-low-link (assq w low-links))
            (on-stack? (assq w on?)))
        (cond
          ((not found-index)
            (dfs w)
            (set! low-links (cons (cons class (min (cdr found-low-link))) low-links)))
          ((and on-stack? (cdr on-stack?)) 
            (set! low-links (cons (cons class (min (cdr found-index))) low-links)))))
      ) 
    (class-refs class))

    (let ((found-index (assq class indexes))
          (found-low-link (assq class low-links)))
      (if (and found-index found-low-link (=fx (cdr found-index) (cdr found-low-link)))
        (multiple-value-bind (scc* stack*) (splitf-at stack (lambda (w) (not (eq? w class))))
          (set! stack (cdr stack*))
          (let ((scc (cons (car stack*) scc*)))
            (for-each (lambda (n) (set! on? (cons (cons n #f) on?))) scc)
            (set! sccs (cons scc sccs)))))
    ))
  
  (for-each (lambda (n) (when (not (assq n indexes)) (dfs n))) class-list)
  sccs))

;*---------------------------------------------------------------------*/
;*    emit-class-types ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-class-types class-list)
  ; Sorts classes such that all classes appear after their super class (if any).
  ; This is required by WASM: struct types must be defined after their supertype.

  ; TODO: emit classes that are mutually dependent in rec groups.
  ;       for that, compute the elementary cycles in the dependencies graph using the Jordan
  ;       algorithm, and generate a rec for each cycle.
  ; (let ((sccs (scc-classes class-list)))
  ;  (tprint (map (lambda (scc) (map (lambda (class) (type-name class)) scc)) sccs)))

  (let ((orders (make-hashtable))
        (current-order 0))
    (define (dfs class)
      (unless (hashtable-contains? orders class)
        (let ((super (tclass-its-super class)))
          (when super (dfs super)))
        (hashtable-put! orders class current-order)
        (set! current-order (+fx current-order 1))))
    (for-each dfs class-list)
    
    (filter-map emit-class-type 
      (sort (lambda (x y) (<fx (hashtable-get orders x) (hashtable-get orders y))) class-list))))

;*---------------------------------------------------------------------*/
;*    emit-class-type ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-class-type class)
  (define (emit-slot slot)
    (with-access::slot slot (type virtual-num name)
    (let ((cname (slot-type slot)))
      (if (>=fx virtual-num 0)
        #f ; TODO: what to do with virtual-num >= 0
        `(field 
          ,(wasm-sym (slot-name slot)) 
          ; TODO: consider removing WASM mut qualifier for read-only slots
          (mut ,(wasm-type cname)))))))

  (let ((super (tclass-its-super class))
        (name (type-class-name class))
        (struct `(struct
          ; MUST BE the same fields as defined in runtime.types.
          (field $header i64)
          (field $widening eqref)
          ,@(filter-map emit-slot (tclass-slots class)))))
    (if super
      `(type
        ,(wasm-sym name) 
        (sub ,@(if (tclass-final? class) '(final) '()) ,(wasm-sym (type-class-name super)) ,struct))
      #f)))

;*---------------------------------------------------------------------*/
;*    should-import? ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (should-import? variable::variable))

(define-method (should-import? variable::global)
  (case (global-import variable)
    ((import) #t)
    (else #f)))

(define-method (should-import? variable::local) #f)

;*---------------------------------------------------------------------*/
;*    emit-prototypes ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-prototypes)
  ;; set the proper name for bigloo-initialized! that is used
  ;; when a main is produced
  (let ((init (find-global 'bigloo-initialized! '__param)))
    (when init (set-variable-name! init)))
    
  (let ((globals '()))
    (let ((cnst-init (get-cnst-table)))
      ;; Use a more suitable name for WASM.
      (global-name-set! cnst-init "__wasm_cnsts")
      (set! globals (cons 
        `(global $__cnsts_table 
            (ref $cnst-table)
            (array.new_default $cnst-table (i32.const ,(get-cnst-offset)))) globals)))

    (for-each-global!
      (lambda (global)
        (if (and (require-prototype? global)
                (not (scnst? (global-value global)))
                (not (require-import? global)))
            (let ((prototype (emit-prototype (global-value global) global)))
              (when prototype (set! globals (cons prototype globals)))))))
    globals))

(define-generic (emit-prototype value::value variable::variable) #f)

(define-method (emit-prototype value::svar variable)
   (emit-prototype/svar/scnst value variable))

; TODO: maybe remove this function as scnst should never be emitted as prototypes 
;       (see conditions in emit-prototypes).
(define-method (emit-prototype value::scnst variable)
   (emit-prototype/svar/scnst value variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype/svar/scnst ...                                    */
;*---------------------------------------------------------------------*/
(define (emit-prototype/svar/scnst value variable)
  (with-access::variable variable (type id name pragma)
    (set-variable-name! variable)
    ; TODO: for now, all global variables are mutable...
    `(global 
      ,(wasm-sym name)
      ,@(if (eq? (global-import variable) 'export)
          `((export ,(global-name variable)))
          '())
      (mut ,(wasm-type type)) ,(emit-default-value type))))

;*---------------------------------------------------------------------*/
;*    emit-default-value ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-default-value type)
  (case (type-id type)
    ; TODO: implement types
    ('bool '(i32.const 0))
    ('char '(i32.const 0))
    ('byte '(i32.const 0))
    ('ubyte '(i32.const 0))
    ('int8 '(i32.const 0))
    ('uint8 '(i32.const 0))
    ('int16 '(i32.const 0))
    ('uint16 '(i32.const 0))
    ('int32 '(i32.const 0))
    ('uint32 '(i32.const 0))
    ('int64 '(i64.const 0))
    ('uint64 '(i64.const 0))
    ('int '(i32.const 0))
    ('uint '(i32.const 0))
    ('long '(i64.const 0))
    ('elong '(i64.const 0))
    ('llong '(i64.const 0))
    ('float '(f32.const 0))
    ('double '(f64.const 0))
    (else '(ref.null none))))

;*---------------------------------------------------------------------*/
;*    emit-cnsts ...                                                   */
;*---------------------------------------------------------------------*/
(define (emit-cnsts)
  (let ((cnsts '()))
    (for-each-global!
      (lambda (global)
        (if (and (require-prototype? global)
                 (scnst? (global-value global)))
          (let ((cnst (emit-cnst (global-value global) global)))
            (when cnst (set! cnsts (cons cnst cnsts)))))))
    (apply append cnsts)))

;*---------------------------------------------------------------------*/
;*    emit-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (emit-cnst value::scnst variable::global)
  (with-trace 'wasm "Emit CNST"
    (with-access::scnst value (class node)
      (trace-item "class=" class)
      (case class
      ((sreal)
        (emit-cnst-real node variable))
      ((selong)
        (emit-cnst-i64 node variable))
      ((sllong)
        (emit-cnst-i64 node variable))
      ((sint32)
        (emit-cnst-i32 node variable))
      ((suint32)
        (emit-cnst-i32 node variable))
      ((sint64)
        (emit-cnst-i64 node variable))
      ((suint64)
        (emit-cnst-i64 node variable))
      ((sstring) 
        (emit-cnst-string node variable))
      ((sfun) 
        (emit-cnst-sfun node variable))
      ((sgfun)
        (emit-cnst-sgfun node variable))
      ; ((selfun)
      ;  (emit-cnst-selfun node variable))
      ; ((slfun)
      ;  (emit-cnst-slfun node variable))
      ; ((stvector)
      ;  (emit-cnst-stvector node variable))
        (else
        (internal-error "backend:emit-cnst"
                (format "Unknown cnst class \"~a\"" class)
                (shape node)))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-string ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-cnst-string ostr global)
  (set-variable-name! global)
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      (mut (ref null $bstring)) 
      (ref.null $bstring))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-real ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-real real global)
  (set-variable-name! global)
  (let ((value 
          (cond
            ((nanfl? real) 'nan)
            ((and (infinitefl? real) (>fl real 0.0)) 'inf)
            ((infinitefl? real) '-inf)
            (else real))))
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      ; FIXME: remove the mut and null qualifiers
      (mut (ref null $real)) 
      (struct.new $real (f64.const ,value))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i32 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i32 value global)
  (set-variable-name! global)
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      i32 (i32.const ,value))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i64 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i64 value global)
  (set-variable-name! global)
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      i64 (i64.const ,value))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun sfun global)
  (emit-cnst-sfun/sgfun sfun global 'procedure))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sgfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sgfun sgfun global)
  (emit-cnst-sfun/sgfun sgfun global 'generic))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun/sgfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun/sgfun fun global kind)
  (with-trace 'wasm "Emit SFUN"
    (trace-item "name=" (global-name global) " import=" (global-import global))
    ; TODO: implement SFUN cnst
    (unless (eq? (global-import global) 'import)
      (let* ((actuals (app-args fun))
              (entry (car actuals))
              (arity (get-node-atom-value (cadr actuals)))
              (vname (set-variable-name! global))
              (name (set-variable-name! (var-variable entry))))
        `((global 
            ,(wasm-sym vname)
            ,@(if (eq? (global-import global) 'export)
                `((export ,vname))
                '())
            ; FIXME: should not be mutable and remove the null qualifier
            (mut (ref null $procedure)) 
            (struct.new $procedure 
              (ref.func ,(wasm-sym name))
              ,(wasm-cnst-unspec)
              (i32.const ,arity)
              ,(if (eq? kind 'generic)
                `(array.new_fixed $vector 3
                  ,(wasm-cnst-false)
                  ,(wasm-cnst-false)
                  ,(wasm-cnst-unspec))
                '(ref.null none)))))))))

;*---------------------------------------------------------------------*/
;*    emit-string-data ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-string-data str info)
  (define (split-long-data data block-size result)
    (if (=fx (string-length data) 0)
      (reverse result)
      (let ((len (min (string-length data) block-size)))
        (split-long-data 
          (substring data len) 
          block-size 
          (cons (substring data 0 len) result)))))
  (let ((section (car info))
        (offset (cdr info)))
    `(data ,section ,@(split-long-data str 100 '()))))

;*---------------------------------------------------------------------*/
;*    emit-strings ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-strings)
  (hashtable-map *allocated-strings* emit-string-data))
  
; We maintain a hashtable of imported globals and functions in WASM as
; we can't import two times the same item (this occurs in the standard
; library).
(define *defined-ids* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    emit-imports ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-imports)
  (let ((imports '()))
    (for-each-global! (lambda (global)
      ;; TODO: remove this log code
      ;; (tprint "GLOBAL " (global-id global) " " (global-name global) " " (global-import global) " " (global-occurrence global))
      (when (and (require-import? global) (not (scnst? global)))
        (let ((import (emit-import (global-value global) global)))
          (when (and import (not (hashtable-contains? *defined-ids* (global-name global))))
            (hashtable-put! *defined-ids* (global-name global) #t)
            (set! imports (cons import imports)))))))
    imports))

;*---------------------------------------------------------------------*/
;*    require-import? ...                                              */
;*---------------------------------------------------------------------*/
(define (require-import? global)
  (let ((value (global-value global))
        (import (global-import global)))
    ;; TODO: remove this log code
    ; (if (and (eq? import 'foreign))
    ;  (tprint "FOREIGN " (global-id global) " " (global-name global) " " (global-jvm-type-name global) " " (global-occurrence global)))
    (and
      (or 
        (eq? import 'import)
        (and 
          (eq? import 'foreign)
          (not (and (isa? value cfun) (not (string-null? (global-jvm-type-name global)))))))
      (>fx (global-occurrence global) 0))))

;*---------------------------------------------------------------------*/
;*    wasm-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (wasm-module variable)
  (let ((name (global-name variable))
        (library (global-library variable))
        (module (global-module variable)))
    ; (multiple-value-bind (id module) (bigloo-demangle name)
      ;; TODO: remove log
      ; (tprint "MODULE " name ": " library " " (global-module variable) " " module " " id)
      (let ((is-macro (isa? variable cfun)))
        (cond
          ; (is-macro "__runtime")
          (library (symbol->string library))
          ((not (eq? module 'foreign)) (symbol->string module))
          ; ((and module (not (eq? module #unspecified))) module)
          (else "__runtime")))))

(define-generic (emit-import value::value variable::variable)
  (set-variable-name! variable)
  (let ((name (global-name variable)))
    `(import ,(wasm-module variable) ,name (global ,(wasm-sym name) (mut ,(wasm-type (global-type variable)))))))

(define-method (emit-import value::cvar variable)
  (set-variable-name! variable)
  `(import 
    ,(wasm-module variable) 
    ,(global-name variable) 
    (global ,(wasm-sym (global-name variable)) ,(wasm-type (global-type variable)))))

(define-method (emit-import value::sfun variable)
  (emit-import/sfun/cfun (map (lambda (arg) (if (local? arg) (local-type arg) arg)) (sfun-args value)) variable))

(define-method (emit-import value::cfun variable)
  (emit-import/sfun/cfun (cfun-args-type value) variable))

;*---------------------------------------------------------------------*/
;*    emit-import/sfun/cfun ...                                        */
;*---------------------------------------------------------------------*/
(define (emit-import/sfun/cfun args-type variable)
  (set-variable-name! variable)
  `(import 
    ,(wasm-module variable)
    ,(global-name variable)
    ,(emit-func-signature args-type variable)))

;*---------------------------------------------------------------------*/
;*    emit-result ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-result t)
  ; TODO: merge this function with the one defined in code.scm (gen-result)
  (if (eq? (type-id t) 'void)
    '()
    `((result ,(wasm-type t)))))

;*---------------------------------------------------------------------*/
;*    emit-func-signature ...                                          */
;*---------------------------------------------------------------------*/
(define (emit-func-signature args-type variable)
  `(func ,(wasm-sym (global-name variable))
    ,@(map (lambda (type)
        `(param ,(wasm-type type)))
      args-type)
    ,@(emit-result (variable-type variable))))

;*---------------------------------------------------------------------*/
;*    emit-memory ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-memory)
  `((memory 1)
    (export "memory" (memory 0))))
