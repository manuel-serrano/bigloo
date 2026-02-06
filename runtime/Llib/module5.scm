;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/module5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 07:29:51 2025                          */
;*    Last change :  Fri Feb  6 10:05:51 2026 (serrano)                */
;*    Copyright   :  2025-26 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    module5 parser                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __module5

   (include "Llib/class.sch")
   
   (import  __error
	    __object
	    __configure
	    __reader
	    __hash
	    __library
	    __binary
	    __macro
	    __eval
	    __expander_srfi0
	    __expand
	    __class)

   (use     __type
	    __tvector
	    __bexit
	    __thread
	    __bit
	    __bignum
	    __bigloo
	    __os
	    __structure
	    __rgc
	    __evenv
	    __evobject
	    
	    __param
	    __trace
	    __intext

	    __r4_input_6_10_2
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_output_6_10_3
	    __r4_ports_6_10_1

	    __r5_control_features_6_4)
   
   (export (class Module
	      (id::symbol read-only)
	      (path::bstring read-only)
	      (expr::pair read-only)
	      (version::long read-only (default 5))
	      (checksum::long (default -1))
	      (decls read-only (default (create-hashtable :size 32 :weak 'open-string)))
	      (exports read-only (default (create-hashtable :size 32 :weak 'open-string)))
	      (imports read-only (default (create-hashtable :size 32 :weak 'open-string)))
	      (defs read-only (default (create-hashtable :size 32 :weak 'open-string)))
	      (classes read-only (default (create-hashtable :size 16 :weak 'open-string)))
	      (main (default #f))
	      (inits::pair-nil (default '()))
	      (libraries::pair-nil (default '()))
	      (body::obj (default '()))
	      (resolved::bool (default #f))
	      (cache-dir (default #f))
	      (heap (default #f)))
	   
	   (class Decl
	      (id::symbol read-only)
	      (xid read-only (default #f))
	      (alias::symbol read-only)
	      mod::Module
	      (modinfo (default #unspecified))
	      (def (default #unspecified))
	      (expr (default #unspecified))
	      (ronly (default #unspecified))
	      scope::symbol
	      (attributes::pair-nil (default '())))

	   (class Def
	      (id::symbol read-only)
	      (type::obj (default #unspecified))
	      kind::symbol 
	      (expr (default #unspecified))
	      (ronly (default #unspecified))
	      (decl (default #unspecified)))

	   (class KDef::Def
	      (depth::long read-only)
	      (registration read-only)
	      (super read-only)
	      (ctor read-only)
	      (kkind::symbol read-only)
	      (ci::struct read-only)
	      (properties::pair-nil read-only))

	   (module5-register-plugin! ::symbol ::procedure)
	   (module4-register-plugin! ::symbol ::procedure)
	   (module5-register-extern-plugin! ::bstring ::procedure)
	   (module5-resolve-path ::bstring ::bstring)
	   (module5-resolve-library ::symbol ::pair-nil)
	   (module5-preload-cache! ::pair-nil)
	   (module5-read::Module ::bstring #!key (lib-path '()) cache-dir expand)
	   (module5-read-library::Module ::bstring ::obj ::Module)
	   (module5-read-heap::Module ::bstring ::obj ::Module)
	   (module5-write-heap ::bstring ::Module)
	   (module5-parse::Module ::pair-nil ::bstring #!key (lib-path '()) cache-dir expand)
	   (module5-import-all! ::Module ::Module)
	   (module5-expand-and-resolve!::Module ::Module ::procedure #!key (heap-modules '()))
	   (module5-checksum!::Module ::Module)
	   (module5-get-decl::Decl ::Module ::symbol ::obj)
	   (module5-get-def::Def ::Module ::symbol ::obj)
	   (module5-get-export-def ::Module ::symbol)
	   (module5-get-class ::Module ::symbol)
	   ))

;*---------------------------------------------------------------------*/
;*    object-write ::Module ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-write m::Module . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Module id=~a path=~s resolved=~a>" (-> m id) (-> m path) (-> m resolved)))

;*---------------------------------------------------------------------*/
;*    object-display ::Module ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-display m::Module . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Module id=~a path=~s resolved=~a>" (-> m id) (-> m path) (-> m resolved)))

;*---------------------------------------------------------------------*/
;*    object-print ::Module ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-print m::Module port ds)
   (object-write m port))
   
;*---------------------------------------------------------------------*/
;*    object-write ::Decl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (object-write d::Decl . port)
   (let ((m::Module (-> d mod)))
      (fprintf (if (pair? port) (car port) (current-output-port))
	 "#<Decl ~a/~a mod=~a scope=~a ronly=~a def=~a>"
	 (-> d id) (-> d alias) (-> m id) (-> d scope) (-> d ronly) (typeof (-> d def)))))

;*---------------------------------------------------------------------*/
;*    object-display ::Decl ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display d::Decl . port)
   (let ((m::Module (-> d mod)))
      (fprintf (if (pair? port) (car port) (current-output-port))
	 "#<Decl ~a/~a mod=~a scope=~a ronly=~a def=~a>"
	 (-> d id) (-> d alias) (-> m id) (-> d scope) (-> d ronly) (typeof (-> d def)))))

;*---------------------------------------------------------------------*/
;*    object-print ::Decl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (object-print d::Decl port ds)
   (object-write d port))

;*---------------------------------------------------------------------*/
;*    object-write ::Def ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-write d::Def . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<~a ~a kind=~a ronly=~a>"
      (class-name (object-class d))
      (-> d id) (-> d kind) (-> d ronly)))

;*---------------------------------------------------------------------*/
;*    object-display ::Def ...                                         */
;*---------------------------------------------------------------------*/
(define-method (object-display d::Def . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<~a ~a kind=~a ronly=~a>"
      (class-name (object-class d))
      (-> d id) (-> d kind) (-> d ronly)))

;*---------------------------------------------------------------------*/
;*    object-print ::Def ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-print d::Def port ds)
   (object-write d port))

;*---------------------------------------------------------------------*/
;*    object-copy ::Decl ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::Decl)
   (duplicate::Decl d))

;*---------------------------------------------------------------------*/
;*    object-copy ::Def ...                                            */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::Def)
   (duplicate::Def d))

;*---------------------------------------------------------------------*/
;*    object-copy ::KDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::KDef)
   (duplicate::KDef d))

;*---------------------------------------------------------------------*/
;*    *modules-by-path* ...                                            */
;*---------------------------------------------------------------------*/
(define module-mutex (make-mutex "modules"))
(define heap-mutex (make-mutex "heaps"))
(define *modules-by-path* (create-hashtable :weak 'open-string))
(define *modules-by-id* (create-hashtable :weak 'open-string))
(define *heaps-by-path* (create-hashtable :weak 'open-string))
(define *plugins* '())
(define *plugins4* '())
(define *extern-plugins* '())

;*---------------------------------------------------------------------*/
;*    module5-qualified-name ...                                       */
;*---------------------------------------------------------------------*/
(define (module5-qualified-name::symbol alias::symbol id::symbol)
   ;; (string->symbol (format "~a@~a" alias id))
   (string->symbol (format "~a.~a" id alias)))
   
;*---------------------------------------------------------------------*/
;*    module5-preload-cache! ...                                       */
;*---------------------------------------------------------------------*/
(define (module5-preload-cache! modules::pair-nil)
   (synchronize module-mutex
      (for-each (lambda (m)
		   (with-access::Module m (path)
		      (hashtable-put! *modules-by-path* path m)))
	 modules)))

;*---------------------------------------------------------------------*/
;*    module5-register-plugin! ...                                     */
;*---------------------------------------------------------------------*/
(define (module5-register-plugin! id::symbol plugin::procedure)
   (synchronize module-mutex
      (if (assq id *plugins*)
	  (error "module5-register-plugin!" "Plugin already registered" id)
	  (set! *plugins* (cons (cons id plugin) *plugins*)))))

;*---------------------------------------------------------------------*/
;*    module4-register-plugin! ...                                     */
;*---------------------------------------------------------------------*/
(define (module4-register-plugin! id::symbol plugin::procedure)
   (synchronize module-mutex
      (if (assq id *plugins4*)
	  (error "module4-register-plugin!" "Plugin already registered" id)
	  (set! *plugins4* (cons (cons id plugin) *plugins4*)))))

;*---------------------------------------------------------------------*/
;*    module5-register-extern-plugin! ...                              */
;*---------------------------------------------------------------------*/
(define (module5-register-extern-plugin! name::bstring plugin::procedure)
   (synchronize module-mutex
      (if (assoc name *extern-plugins*)
	  (error "module5-register-extern-plugin!" "Plugin already registered" name)
	  (set! *extern-plugins* (cons (cons name plugin) *extern-plugins*)))))

;*---------------------------------------------------------------------*/
;*    absolute-file-name? ...                                          */
;*---------------------------------------------------------------------*/
(define (absolute-file-name? path)
   (when (>fx (string-length path) 0)
      (char=? (string-ref path 0) #\/)))

;*---------------------------------------------------------------------*/
;*    module5-resolve-path ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-resolve-path rel::bstring base::bstring)
   (with-trace 'module5 "module5-resolve-path"
      (trace-item "rel=" rel)
      (trace-item "base=" base)
      (let ((path (if (absolute-file-name? rel)
		      rel
		      (make-file-name (dirname base) rel))))
	 (synchronize module-mutex
	    (let ((m (hashtable-get *modules-by-path* path)))
	       (cond
		  ((isa? m Module)
		   (with-access::Module m (path)
		      path))
		  ((file-exists? path)
		   (file-name-unix-canonicalize! path))))))))

;*---------------------------------------------------------------------*/
;*    module5-resolve-library ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-resolve-library id::symbol search-path)
   (let* ((init (library-init-file id))
	  (path (find-file/path init search-path)))
      (when (and (string? path) (file-exists? path))
	 (file-name-unix-canonicalize! path))))

;*---------------------------------------------------------------------*/
;*    absolute-file-name ...                                           */
;*---------------------------------------------------------------------*/
(define (absolute-file-name path)
   (file-name-canonicalize (make-file-name (pwd) path)))

;*---------------------------------------------------------------------*/
;*    filecache-name ...                                               */
;*---------------------------------------------------------------------*/
(define (filecache-name path)
   (string-replace path #\/ #\_))

;*---------------------------------------------------------------------*/
;*    filecache-dirs ...                                               */
;*---------------------------------------------------------------------*/
(define (filecache-dirs path cache-dir)
   (let* ((apath (absolute-file-name path))
	  (cname (filecache-name apath))
	  (cpath (make-file-name cache-dir cname)))
      (values apath cname cpath)))

;*---------------------------------------------------------------------*/
;*    serialize ...                                                    */
;*---------------------------------------------------------------------*/
(define (serialize mod::Module)

   (define (decls->list decls)
      (hashtable-map decls
	 (lambda (k d::Decl)
	    (with-access::Decl d (mod)
	       (with-access::Module mod (path version id heap)
		  (let ((decl::Decl (duplicate::Decl d
				       (mod (class-nil Module))
				       (modinfo (cons (or heap version) path)))))
		     (when (isa? (-> d def) Def)
			(let ((def::Def (object-copy (-> d def))))
			   (set! (-> decl def) def)
			   (set! (-> def decl) decl)))
		     (cons k decl)))))))

   (define (classes->list classes exports)
      (hashtable-filter-map (-> mod classes)
	 (lambda (k ci)
	    (let* ((id (class-info-id ci))
		   (d (hashtable-get exports (symbol->string! id))))
	       (when d
		  (cons k ci))))))

   (with-trace 'module5-serialize "serialize"
      (trace-item "mod=" (-> mod id))
      (trace-item "path=" (-> mod path))
      (if (eq? (-> mod decls) #unspecified)
	  mod
	  (begin
	     (module5-checksum! mod)
	     (duplicate::Module mod
		(inits '())
		(body '())
		(decls #unspecified)
		(defs #unspecified)
		(imports (decls->list (-> mod imports)))
		(exports (decls->list (-> mod exports)))
		(classes (classes->list (-> mod classes) (-> mod exports))))))))

;*---------------------------------------------------------------------*/
;*    unserialize ...                                                  */
;*---------------------------------------------------------------------*/
(define (unserialize mod::Module lib-path cache-dir expand)

   (define (cannot-find d::Decl path)
      (error (-> mod id)
	 (format "Cannot find \"~a\" module source file" (-> d id))
	 path))

   (define (unserialize-resolve-path d path)
      (if (or (file-exists? path) (absolute-file-name? path))
	  path
	  (cannot-find d path)))
   
   (define (unserialize-decl d::Decl)
      (with-trace 'module5-serialize "unserialize-decl"
	 (trace-item "id="(-> d id))
	 (trace-item "modinfo=" (-> d modinfo))
	 (match-case (-> d modinfo)
	    ((5 . ?path)
	     (set! (-> d mod)
		(module5-read (unserialize-resolve-path d path)
		   lib-path: lib-path :cache-dir cache-dir :expand expand)))
	    ((4 . ?path)
	     (set! (-> d mod)
		(module4-read (unserialize-resolve-path d path)
		   lib-path: lib-path :cache-dir cache-dir :expand expand)))
	    ((?heap . ?path)
	     (let* ((hmod::Module (module5-read-heap heap
				     (-> d id) mod))
		    (def::Def (module5-get-export-def hmod (-> d id)))
		    (decl::Decl (-> def decl)))
		(set! (-> d mod) (-> decl mod))))
	    (else
	     (error "unserialize" "Illegal serialized declaration" (-> mod id))))
	 d))
   
   (define (list->decls! env l)
      (for-each (lambda (e)
		   (hashtable-put! env (car e) (unserialize-decl (cdr e))))
	 l)
      env)
   
   (define (list->defs! env l)
      (for-each (lambda (e)
		   (with-access::Decl (cdr e) (id def)
		      (when (isa? def Def)
			 (with-access::Def def (decl)
			    (set! decl (cdr e))))
		      (hashtable-put! env (symbol->string! id) def)))
	 l)
      env)
   
   (define (list->classes! env l)
      (for-each (lambda (e)
		   (hashtable-put! env (car e) (cdr e)))
	 l)
      env)

   (with-trace 'module5-serialize "unserialize"
      (trace-item "mod=" (-> mod id))
      (trace-item "path=" (-> mod path))
      (let ((m::Module (duplicate::Module mod
			  (exports (create-hashtable :size 32 :weak 'open-string))
			  (imports (create-hashtable :size 32 :weak 'open-string))
			  (decls (create-hashtable :size 32 :weak 'open-string))
			  (defs (create-hashtable :size 32 :weak 'open-string))
			  (classes (create-hashtable :size 32 :weak 'open-string)))))
	 (hashtable-put! *modules-by-path* (-> mod path) m)
	 (list->decls! (-> m imports) (-> mod imports))
	 (list->decls! (-> m exports) (-> mod exports))
	 (list->defs! (-> m defs) (-> mod exports))
	 (list->classes! (-> m classes) (-> mod classes))
	 m)))

;*---------------------------------------------------------------------*/
;*    filecache-read ...                                               */
;*---------------------------------------------------------------------*/
(define (filecache-read path lib-path cache-dir expand)
   (with-trace 'module5-cache "filecache-read"
      (trace-item "path=" path)
      (trace-item "lib-path=" lib-path)
      (trace-item "cache-dir=" cache-dir)
      (let ((p (open-input-binary-file path)))
	 (unwind-protect
	    (unserialize (input-obj p) lib-path cache-dir expand)
	    (close-binary-port p)))))

;*---------------------------------------------------------------------*/
;*    lock-path ...                                                    */
;*---------------------------------------------------------------------*/
(define (lock-path cache-dir)
   (make-file-name cache-dir "LOCK"))

;*---------------------------------------------------------------------*/
;*    filecache-get ...                                                */
;*---------------------------------------------------------------------*/
(define (filecache-get path lib-path cache-dir expand parse)
   (with-trace 'module5-cache "filecache-get"
      (trace-item "path=" path)
      (trace-item "cache-dir=" cache-dir)
      (when (string? cache-dir)
	 (multiple-value-bind (apath cname cpath)
	    (filecache-dirs path cache-dir)
	    (trace-item "apath=" apath)
	    (trace-item "cname=" cname)
	    (trace-item "cpath=" cpath)
	    (unless (directory? cache-dir)
	       (make-directories cache-dir))
	    (call-with-output-file (lock-path cache-dir)
	       (lambda (lock)
		  (lockf lock 'lock)
		  (unwind-protect
		     (when (and (file-exists? cpath)
				(>=elong (file-modification-time cpath)
				   (file-modification-time apath)))
			(let ((m (filecache-read cpath
				    lib-path cache-dir expand)))
			   (with-access::Module m (classes exports decls defs)
			      (trace-item "classes=" (hashtable-size classes))
			      (trace-item "exports=" (hashtable-size exports))
			      (trace-item "decls=" (typeof decls))
			      (trace-item "defs=" (hashtable-size defs))
			      (hashtable-put! *modules-by-path* path m)
			      m)))
		     (lockf lock 'ulock))))))))

;*---------------------------------------------------------------------*/
;*    filecache-put! ...                                               */
;*---------------------------------------------------------------------*/
(define (filecache-put! path mod::Module)
   (with-trace 'module5-cache "filecache-put!"
      (trace-item "path=" path)
      (when (string? (-> mod cache-dir))
	 (unless (directory? (-> mod cache-dir))
	    (make-directories (-> mod cache-dir)))
	 (multiple-value-bind (apath cname cpath)
	    (filecache-dirs path (-> mod cache-dir))
	    (call-with-output-file (lock-path (-> mod cache-dir))
	       (lambda (lock)
		  (lockf lock 'lock)
		  (unwind-protect
		     (let ((p (open-output-binary-file cpath)))
			(unwind-protect
			   (output-obj p (serialize mod))
			   (close-binary-port p)))
		     (lockf lock 'ulock))))))))

;*---------------------------------------------------------------------*/
;*    module-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (module-read path::bstring lib-path cache-dir expand parse)
   (with-trace 'module5 "module-read"
      (synchronize module-mutex
	 (trace-item "path=" path
	    (if (hashtable-get *modules-by-path* path) " [in-mem-cache]" ""))
	 (if (hashtable-get *modules-by-path* path)
	     (with-access::Module (hashtable-get *modules-by-path* path) (exports)
		(trace-item "exports=" (hashtable-map exports
					  (lambda (k d::Decl)
					     (cons (-> d id) (-> d xid)))))))
	 (or (hashtable-get *modules-by-path* path)
	     (filecache-get path lib-path cache-dir expand parse)
	     (let ((exprs (call-with-input-file path
			     (lambda (p) (port->sexp-list p #t)))))
		(if (null? exprs)
		    (error "module5-read" "Missing module clause" path)
		    (parse exprs path
		       :lib-path lib-path
		       :cache-dir cache-dir
		       :expand expand)))))))
   
;*---------------------------------------------------------------------*/
;*    module5-read ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-read path::bstring #!key (lib-path '()) cache-dir expand)
   (module-read path (or lib-path '()) cache-dir expand module5-parse))

;*---------------------------------------------------------------------*/
;*    module4-read ...                                                 */
;*    -------------------------------------------------------------    */
;*    Read and parse a module4 when imported from a module 5.          */
;*---------------------------------------------------------------------*/
(define (module4-read path::bstring #!key (lib-path '()) cache-dir expand)
   (module-read path lib-path cache-dir expand module4-parse))

;*---------------------------------------------------------------------*/
;*    module5-read-library ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-read-library path::bstring expr mod)
   (with-trace 'module5 "module5-read-library"
      (trace-item "path=" path)
      (trace-item "expr=" expr)
      (let ((init (module5-read-library-init! path)))
	 (match-case init 
	    ((declare-library! (quote ?id) . ?rest)
	     (let ((srfi (memq :srfi rest)))
		(match-case srfi
		   ((:srfi (quote ?srfis) . ?-)
		    (for-each register-srfi! srfis))))
	     (let ((heap (memq :heap rest)))
		(match-case heap
		   ((:heap (and (? string?) ?file . ?-))
		    (module5-read-heap
		       (make-file-name (dirname path) file)
		       expr mod))
		   (else
		    (module5-read-heap
		       (make-file-name (dirname path)
			  (string-append (prefix (basename path)) ".heap5"))
		       expr mod)))))
	    (else
	     (error/loc mod "Illegal library" path init))))))

;*---------------------------------------------------------------------*/
;*    module5-read-library-init! ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-read-library-init! path)
   (let ((init (call-with-input-file path read)))
      (module5-declare-library! init)
      init))
	
;*---------------------------------------------------------------------*/
;*    module5-declare-library! ...                                     */
;*    -------------------------------------------------------------    */
;*    Calling declare-library! is mandatory as it stores information   */
;*    about the library that is used by the compier, for instance,     */
;*    for associating the actual .so or .a file to the Bigloo          */
;*    library name.                                                    */
;*---------------------------------------------------------------------*/
(define (module5-declare-library! init)
   (apply declare-library!
      (map (lambda (e)
	      (match-case e
		 ((quote ?x) x)
		 (else e)))
	 (cdr init))))

;*---------------------------------------------------------------------*/
;*    module5-read-heap ...                                            */
;*---------------------------------------------------------------------*/
(define (module5-read-heap path::bstring expr mod)
   (with-trace 'module5 "module5-read-heap"
      (trace-item "path=" path)
      (synchronize heap-mutex
	 (or (hashtable-get *heaps-by-path* path)
	     (let ((port (open-input-binary-file path)))
		(if (not (binary-port? port))
		    (if (file-exists? path)
			(error/loc mod "Cannot read heap file" path expr)
			(error/loc mod "Cannot find heap file" path expr))
		    (unwind-protect
		       (let ((m (heap->module5 (input-obj port) path expr mod)))
			  (hashtable-put! *heaps-by-path* path m)
			  m)
		       (close-binary-port port))))))))

;*---------------------------------------------------------------------*/
;*    module5-write-heap ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-write-heap path::bstring mod::Module)
   (with-trace 'module5 "module5-write-heap"
      (trace-item "heap=" path)
      (trace-item "mod=" (-> mod id))
      (trace-item "mod-path=" (-> mod path))
      (let ((port (open-output-binary-file path)))
	 (if (not (binary-port? port))
	     (error/loc mod "Cannot write heap file" path path)
	     (unwind-protect
		(output-obj port (module5->heap mod))
		(close-binary-port port))))))

;*---------------------------------------------------------------------*/
;*    *heap-signature* ...                                             */
;*---------------------------------------------------------------------*/
(define *heap-signature* 17051966)

;*---------------------------------------------------------------------*/
;*    heap->module5 ...                                                */
;*---------------------------------------------------------------------*/
(define (heap->module5::Module heap path expr mod)
   (with-trace 'module5 "heap->module5"
      (trace-item "path=" path)
      (trace-item "expr=" expr)
      (cond
	 ((or (not (vector? heap))
	      (not (=fx (vector-length heap) 4))
	      (not (=fx (vector-ref-ur heap 0) *heap-signature*)))
	  (error/loc mod "Corrupted head" path expr))
	 ((not (equal? (vector-ref-ur heap 1)
		  (bigloo-config 'release-number)))
	  (error/loc mod
	     (format "Heap incompatible, build-release ~s vs ~s"
		(vector-ref-ur heap 1) (bigloo-config 'release-number))
	     path expr))
	 ((not (equal? (vector-ref-ur heap 2)
		  (bigloo-config 'specific-version)))
	  (error/loc mod
	     (format "Heap incompatible, build-specific ~s vs ~s"
		(vector-ref-ur heap 2) (bigloo-config 'specific-version))
	     path expr))
	 (else
	  (let ((mod::Module (vector-ref-ur heap 3)))
	     (set! (-> mod heap) path)
	     (hashtable-for-each (-> mod exports)
		(lambda (key d::Decl)
		   (let ((imod::Module (-> d mod)))
		      (set! (-> imod heap) path))))
	     mod)))))

;*---------------------------------------------------------------------*/
;*    module5->heap ...                                                */
;*---------------------------------------------------------------------*/
(define (module5->heap mod::Module)
   (vector *heap-signature*
      (bigloo-config 'release-number)
      (bigloo-config 'specific-version)
      mod))

;*---------------------------------------------------------------------*/
;*    module5-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (module5-parse::Module exprs path::bstring #!key (lib-path '()) cache-dir expand)

   (define (parse5 id path clauses expr body)
      (let ((mod (instantiate::Module
		    (id id)
		    (path path)
		    (expr expr)
		    (body body)
		    (cache-dir cache-dir))))
	 (hashtable-put! *modules-by-path* path mod)
	 (let ((omod (hashtable-get *modules-by-id* (symbol->string id))))
	    (if omod
		(with-access::Module omod ((opath path))
		   (error/loc mod
		      (format "Module \"~a\" has already been declared in file ~s"
			 id opath)
		      path expr))
		(hashtable-put! *modules-by-id* (symbol->string id) mod)))
	 
	 (for-each (lambda (c)
		      (module5-parse-clause c expr mod lib-path cache-dir expand))
	    clauses)
	 (with-access::Module mod (imports exports)
	    (trace-item "imports="
	       (hashtable-map imports
		  (lambda (k d::Decl)
		     (let ((m::Module (-> d mod)))
			(format "~a"
			   (vector k
			      alias: (-> d alias) id: (-> d id) mod: (-> m id)))))))
	    (trace-item "exports="
	       (hashtable-map exports
		  (lambda (k d::Decl)
		     (let ((m::Module (-> d mod)))
			(format "~a"
			   (vector k
			      alias: (-> d alias) id: (-> d id) mod: (-> m id))))))))
	 
	 (with-access::Module mod (inits libraries)
	    (set! inits (delete-duplicates! inits
			   (lambda (x y)
			      (with-access::Module x ((xid id))
				 (with-access::Module y ((yid id))
				    (eq? xid yid))))))
	    (trace-item "libraries=" libraries))
	 mod))

   (with-trace 'module5-parse "module5-parse"
      (trace-item "path=" path)
      (trace-item "exprs=" exprs)
      (let ((expr (car exprs)))
	 (match-case expr
	    ((module (and (? symbol?) ?id) :version 5 . ?clauses)
	     (parse5 id path clauses expr (cdr exprs)))
	    ((module (and (? symbol?) ?id) . ?clauses)
	     (parse5 id path clauses expr (cdr exprs)))
	    (else
	     (error/loc #f "Illegal expression" expr #f))))))

;*---------------------------------------------------------------------*/
;*    module5-parse-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-parse-clause clause expr::pair mod::Module lib-path cache-dir expand)

   (define (unbound-error path id clause)
      (error/loc mod (format "Cannot find declaration in module \"~a\"" path)
	 id clause))

   (define (scope-error path id clause)
      (error/loc mod (format "Module \"~a\" does not export" path)
	 id clause))

   (define (hashtable-symbol-get table id)
      (hashtable-get table (symbol->string! id)))

   (define (hashtable-symbol-put! table id d)
      (hashtable-put! table (symbol->string! id) d))

   (define (parse-import-binding b imod::Module expr::pair mod::Module expand)
      (match-case b
	 ((? symbol?)
	  (let ((idecl (hashtable-symbol-get (-> imod exports) b)))
	     (if (isa? idecl Decl)
		 (let* ((i::Decl idecl)
			(d (duplicate::Decl idecl
			      (id b)
			      (alias b)
			      (scope 'import))))
		    (hashtable-symbol-put! (-> mod decls) b d)
		    (hashtable-symbol-put! (-> mod imports) b d)
		    d)
		 (unbound-error (-> imod path) b clause))))
	 (((and (? symbol?) ?alias) (and (? symbol?) ?id))
	  (let ((idecl (hashtable-symbol-get (-> imod exports) id)))
	     (if (isa? idecl Decl)
		 (let* ((i::Decl idecl)
			(d (duplicate::Decl idecl
			      (id id)
			      (alias alias)
			      (scope 'import))))
		    (hashtable-symbol-put! (-> mod decls) alias d)
		    (hashtable-symbol-put! (-> mod imports) id d)
		    d)
		 (unbound-error (-> imod path) id clause))))
	 (else
	  (error/loc mod "Illegal import binding" b clause))))

   (define (parse-reexport-binding b imod::Module expr::pair mod::Module expand)
      (match-case b
	 ((? symbol?)
	  (let ((idecl (hashtable-symbol-get (-> imod exports) b)))
	     (if (isa? idecl Decl)
		 (let* ((i::Decl idecl)
			(d (duplicate::Decl idecl
			     (xid (-> i id))
			     (alias b)
			     (scope 'import))))
		    (hashtable-symbol-put! (-> mod decls) b d)
		    (hashtable-symbol-put! (-> mod imports) b d)
		    d)
		 (unbound-error (-> imod path) b clause))))
	 (((and (? symbol?) ?alias) (and (? symbol?) ?id))
	  (let ((idecl (hashtable-symbol-get (-> imod exports) id)))
	     (if (isa? idecl Decl)
		 (let* ((i::Decl idecl)
		       (d (duplicate::Decl idecl
			     (xid id)
			     (alias alias)
			     (scope 'import))))
		    (hashtable-symbol-put! (-> mod decls) alias d)
		    (hashtable-symbol-put! (-> mod imports) alias d)
		    d)
		 (unbound-error (-> imod path) id clause))))
	 (else
	  (error/loc mod "Illegal reexport binding" b clause))))
   
   (define (parse-reexport-all clause::pair expr::pair mod::Module expand)
      (with-trace 'module5-parse "parse-reexport-all"
	 (trace-item "mod=" (-> mod id))
	 (let* ((path (cadr clause))
		(rfrom (module5-resolve-path path (-> mod path))))
	    (if (string? rfrom)
		(let ((imod::Module (module5-read rfrom
				       :lib-path lib-path
				       :cache-dir cache-dir
				       :expand expand))
		      (mid (-> mod id)))
		   (hashtable-for-each (-> imod exports)
		      (lambda (key d::Decl)
			 (let ((nd (duplicate::Decl d
				      (scope 'import))))
			    (trace-item "id=" (-> d id)
			       " imod=" (-> imod id)
			       " (reexport-all)")
			    (hashtable-put! (-> mod exports) key nd))))
		   (set! (-> mod inits) (append! (-> mod inits) (list imod))))
		(error/loc mod "Cannot find file" path clause)))))
   
   (define (parse-reexport4-all clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr (cddr clause)))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module4-read rfrom
				    :lib-path lib-path
				    :cache-dir cache-dir
				    :expand expand)))
		(hashtable-for-each (-> imod exports)
		   (lambda (key d::Decl)
		      (hashtable-put! (-> mod exports) key d)))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" path clause))))
   
   (define (parse-reexport-some clause::pair expr::pair mod::Module expand)
      (with-trace 'module5-parse "parse-reexport-some"
	 (trace-item "mod=" (-> mod id))
	 (let* ((path (cadr clause))
		(bindings (cddr clause))
		(rfrom (module5-resolve-path path (-> mod path))))
	    (if (string? rfrom)
		(let ((imod::Module (module5-read rfrom
				       :lib-path lib-path
				       :cache-dir cache-dir
				       :expand expand)))
		   (for-each (lambda (b)
				(let ((d::Decl (parse-reexport-binding b
						  imod expr mod expand)))
				   (with-access::Decl d (scope id alias)
				      (trace-item "id=" id
					 " alias=" alias
					 " imod=" (-> imod id)
					 " (reexport-some)")
				      (hashtable-symbol-put!
					 (-> mod exports) alias d))
				   d))
		      bindings)
		   (set! (-> mod inits) (append! (-> mod inits) (list imod))))
		(error/loc mod "Cannot find file" path clause)))))
   
   (define (parse-import-init clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path
				    :cache-dir cache-dir
				    :expand expand)))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" path clause))))
   
   (define (parse-import-all id::symbol clause::pair expr::pair mod::Module expand)
      (with-trace 'module5-parse "parse-import-all"
	 (trace-item "mod=" (-> mod id))
	 (let* ((path (cadr clause))
		(rfrom (module5-resolve-path path (-> mod path))))
	    (if (string? rfrom)
		(let ((imod::Module (module5-read rfrom
				       :lib-path lib-path
				       :cache-dir cache-dir
				       :expand expand)))
		   (hashtable-for-each (-> imod exports)
		      (lambda (key d::Decl)
			 (let* ((alias (module5-qualified-name (-> d alias) id))
				(nd (duplicate::Decl d
				       (alias alias)
				       (id (-> d alias))
				       (scope 'import))))
			    (trace-item "id=" (-> d id)
			       " alias=" alias
			       " imod=" (-> imod id) " (import-all)")
			    (hashtable-symbol-put! (-> mod decls) alias nd)
			    (hashtable-symbol-put! (-> mod imports) alias nd))))
		   (module-add-libraries! mod (-> imod libraries))
		   (set! (-> mod inits) (append! (-> mod inits) (list imod))))
		(error/loc mod "Cannot find file" path clause)))))
   
   (define (parse-import-some clause::pair expr::pair mod::Module expand)
      (with-trace 'module5-parse "parse-import-some"
	 (trace-item "mod=" (-> mod id))
	 (let* ((path (cadr clause))
		(bindings (cddr clause))
		(rfrom (module5-resolve-path path (-> mod path))))
	    (if (string? rfrom)
		(let ((imod::Module (module5-read rfrom
				       :lib-path lib-path
				       :cache-dir cache-dir
				       :expand expand)))
		   (for-each (lambda (b)
				(trace-item "b=" b
				   " imod=" (-> imod id) " (import-some)")
				(parse-import-binding b imod expr mod expand))
		      bindings)
		   (module-add-libraries! mod (-> imod libraries))
		   (set! (-> mod inits) (append! (-> mod inits) (list imod))))
		(error/loc mod "Cannot find file" path clause)))))

   (define (parse-import4-all clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr (cddr clause)))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module4-read rfrom
				    :lib-path lib-path
				    :cache-dir cache-dir
				    :expand expand)))
		(hashtable-for-each (-> imod exports)
		   (lambda (key d::Decl)
		      (let* ((alias (-> d alias))
			     (nd (duplicate::Decl d
				    (alias alias)
				    (scope 'import))))
			 (hashtable-put! (-> mod decls) key nd)
			 (hashtable-put! (-> mod imports) key nd)
			 )))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" path clause))))

   (define (parse-export clause expr::pair mod::Module expand)
      (for-each-expr (lambda (expr src)
			(match-case expr
			   ((and ?id (? symbol?))
			    (let ((decl (instantiate::Decl
					   (id id)
					   (alias id)
					   (mod mod)
					   (scope 'export)
					   (expr src))))
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)))
			   (((and ?alias (? symbol?)) (and ?id (? symbol?)))
			    (let ((decl (instantiate::Decl
					   (id id)
					   (alias alias)
					   (mod mod)
					   (scope 'export)
					   (expr expr))))
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) alias decl)))
			   (else
			    (error/loc mod "Illegal export clause" clause expr))))
	 (cdr clause)))

   (define (parse-main id expr::pair mod::Module expand)
      (set! (-> mod main) id))

   (define (parse-include clause expr::pair mod::Module expand)
      (for-each (lambda (f)
		   (cond
		      ((not (string? f))
		       (error/loc mod "Illegal include clause" f clause))
		      ((module5-resolve-path f (-> mod path))
		       =>
		       (lambda (f)
			  (call-with-input-file f
			     (lambda (p)
				(for-each (lambda (c)
					     (module5-parse-clause c clause mod
						lib-path cache-dir expand))
				   (port->sexp-list p #t))))))
		      (else
		       (error/loc mod "Cannot find file" f clause))))
	 (cdr clause)))

   (define (parse-cond-expand clause expr::pair mod::Module expand)
      (let ((ec (expand clause)))
	 (when (epair? ec)
	    (module5-parse-clause ec expr mod lib-path cache-dir expand))))

   (define (parse-library-all id::symbol clause expr::pair mod::Module expand)
      (let* ((lib (cadr clause))
	     (rlib (module5-resolve-library lib lib-path)))
	 (if (string? rlib)
	     (let ((lmod::Module (module5-read-library rlib clause mod)))
		(hashtable-for-each (-> lmod exports)
		   (lambda (k d::Decl)
		      (let* ((alias (module5-qualified-name (-> d alias) id))
			     (nd (duplicate::Decl d
				    (alias alias)
				    (scope 'import))))
			 (hashtable-put! (-> mod decls) k nd)
			 (hashtable-put! (-> mod imports) k nd)
			 )))
		(set! (-> mod inits)
		   (append! (-> mod inits) (list lmod)))
		(set! (-> mod libraries)
		   (cons (cons lib rlib) (-> mod libraries))))
	     (error/loc mod "Cannot find library" lib clause))))
   
   (define (parse-library-some clause expr::pair mod::Module expand)
      (let* ((lib (cadr clause))
	     (bindings (caddr clause))
	     (rlib (module5-resolve-library lib lib-path)))
	 (if (string? rlib)
	     (let ((lmod::Module (module5-read-library rlib clause mod)))
		(for-each (lambda (b)
			     (parse-import-binding b lmod expr mod expand))
		   bindings)
		(set! (-> mod libraries)
		   (cons (cons lib rlib) (-> mod libraries)))
		(set! (-> mod inits)
		   (append! (-> mod inits) (list lmod))))
	     (error/loc mod "Cannot find library" lib clause))))

   (define (parse-extern clause expr::pair mod::Module expand)
      (let* ((name (cadr clause))
	     (plugin (assoc name *extern-plugins*)))
	 (if plugin
	     ((cdr plugin) mod clause) 
	     (error/loc mod "No extern plugin" name clause))))
      
   (with-trace 'module5-parse "module5-parse-clause"
      (trace-item "mod=" (-> mod id))
      (trace-item "mod-path=" (-> mod path))
      (trace-item "lib-path=" lib-path)
      (trace-item "clause=" clause)
      (match-case clause
	 ((export (? string?))
	  (parse-reexport-all clause expr mod expand))
	 ((export (? string?) . ?-)
	  (parse-reexport-some clause expr mod expand))
	 ((export :version 4 (? string?))
	  (parse-reexport4-all clause expr mod expand))
	 ((import :version 5 . ?rest)
	  (module5-parse-clause
	     (localize `(import ,@rest) clause)
	     expr mod lib-path cache-dir expand))
	 ((import (? string?))
	  (parse-import-init clause expr mod expand))
	 ((import (? string?) . (and (? symbol?) ?id))
	  (parse-import-all id clause expr mod expand))
	 ((import (? string?) . (? list?))
	  (parse-import-some clause expr mod expand))
	 ((import :version 4 (? string?))
	  (parse-import4-all clause expr mod expand))
	 ((export . ?bindings)
	  (parse-export clause expr mod expand))
	 ((main)
	  (parse-main 'main expr mod expand))
	 ((main (and (? symbol?) ?main))
	  (parse-main main expr mod expand))
	 ((include . ?-)
	  (parse-include clause expr mod expand))
	 ((library (? symbol?) (and (? symbol?) ?id))
	  (parse-library-all id clause expr mod expand))
	 ((library (? symbol?) (? list?))
	  (parse-library-some clause expr mod expand))
	 ((extern (and (? string?) ?name) . ?clauses)
	  (parse-extern clause expr mod expand))
	 ((cond-expand . ?-)
	  (parse-cond-expand clause expr mod expand))
	 ((?id . ?rest)
	  (let ((p (synchronize module-mutex (assq id *plugins*))))
	     (if p
		 ((cdr p) mod clause)
		 (error/loc mod "Illegal module clause" clause expr))))
	 (else
	  (error/loc mod "Illegal module clause" clause expr)))))

;*---------------------------------------------------------------------*/
;*    module5-import-all! ...                                          */
;*    -------------------------------------------------------------    */
;*    Used by the compiler to force a compiled module to import        */
;*    previously loaded module. Used to restore the classes of         */
;*    a heap4 file (see comptime/Module/module5.scm).                  */
;*---------------------------------------------------------------------*/
(define (module5-import-all! mod::Module imod::Module)
   (hashtable-for-each (-> imod exports)
      (lambda (key d::Decl)
	 (let* ((alias (-> d alias))
		(nd (duplicate::Decl d
		       (alias alias)
		       (scope 'import))))
	    (hashtable-put! (-> mod decls) key nd)
	    (hashtable-put! (-> mod imports) key nd))))
   (module-add-libraries! mod (-> imod libraries))
   (set! (-> mod inits) (append! (-> mod inits) (list imod))))

;*---------------------------------------------------------------------*/
;*    module-add-libraries! ...                                        */
;*---------------------------------------------------------------------*/
(define (module-add-libraries! mod::Module libs::pair-nil)
   (for-each (lambda (l)
		(module5-read-library-init! (cdr l))
		(unless (assq (car l) (-> mod libraries))
		   (set! (-> mod libraries) (cons l (-> mod libraries)))))
      libs))

;*---------------------------------------------------------------------*/
;*    module5-expand-and-resolve! ...                                  */
;*---------------------------------------------------------------------*/
(define (module5-expand-and-resolve! mod::Module init-xenv #!key (heap-modules '()))
   (unless (-> mod resolved)
      (with-trace 'module5-resolve "module5-expand-and-resolve!"
	 (trace-item (-> mod id) " resolved=" (-> mod resolved))
	 (trace-item "decls="
	    (hashtable-map (-> mod decls)
	       (lambda (k d::Decl)
		  (format "~a/~a(~a)" (-> d id) (-> d alias) (-> d scope)))))
	 (trace-item "defs="
	    (hashtable-map (-> mod defs)
	       (lambda (k d::Def)
		  (format "~a::~a" (-> d id) (typeof d)))))
	 (trace-item "exports="
	    (hashtable-map (-> mod exports)
	       (lambda (k d::Decl)
		  (format "~a" (-> d id)))))
	 (trace-item "heap-modules="
	    (map (lambda (m)
		    (with-access::Module m (id) id))
	       heap-modules))
	 (set! (-> mod resolved) #t)
	 ;; force import the "heap-modules", i.e., the modules that
	 ;; come from a Bigloo heap file
	 (for-each (lambda (m) (module5-import-all! mod m)) heap-modules)
	 (let* ((xenv (init-xenv (make-module5-xenv) mod))
		(kx (define-class-expander mod xenv))
		(ko (co-instantiate-expander mod))
		(ki (include-expander mod)))
	    (install-module5-expander xenv 'define-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'define-wide-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'define-final-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'define-abstract-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'co-instantiate
	       '(co-instantiate) ko)
	    (install-module5-expander xenv 'include
	       '(include) ki)
	    (hashtable-for-each (-> mod decls)
	       (lambda (k d::Decl)
		  (with-access::Decl d ((imod mod) alias id xid def scope)
		     (trace-item "d=" id " xid=" xid
			" alias=" alias
			" mod=" (-> imod id)
			" scope=" scope
			" def=" (typeof def))
		     (unless (eq? imod mod)
			(module5-expand-and-resolve! imod init-xenv
			   :heap-modules heap-modules)
			(let ((idef (module5-get-export-def imod (or xid id))))
			   (with-access::Def idef (kind expr ci)
			      (case kind
				 ((macro)
				  (trace-item "bind-macro alias="
 				     alias " id=" id)
				  (install-module5-expander xenv alias expr
				     (eval! (macro->expander expr))))
				 ((expander)
				  (trace-item "bind-expander alias="
				     alias " id=" id)
				  (install-module5-expander xenv alias expr
				     (eval! (caddr expr))))
				 ((class)
				  (trace-item "bind-class alias="
				     alias " id=" id)
				  (set! def idef)
				  (with-access::KDef idef (ci decl)
				     (module5-bind-class! mod id ci)
				     (install-class-expanders ci xenv mod))))))))))
	    (when (pair? (-> mod body))
	       (trace-item "body before-expand=" (-> mod body))
	       (set! (-> mod body)
		  (map (lambda (x) (expand/env x xenv)) (-> mod body)))
	       (trace-item "body after-expand=" (-> mod body)))
	    ;; Macro and class definitions are disgarded by the macro-expansion.
	    ;; Because these definitions are needed to resolve the module
	    ;; exports, INSTALL-MODULE5-EXPANDER (runtime/macro.scm),
	    ;; stores these definitions in XENV.
	    (let ((dm (hashtable-filter-map xenv (lambda (k e) (car e)))))
	       (collect-defines! mod dm))
	    (collect-defines! mod (-> mod body))
	    (collect-classes! mod)
	    ;; bind all the classes
	    (check-unbounds mod))
	 (ronly! mod)
	 (filecache-put! (-> mod path) mod)
	 (trace-item "decls="
	    (hashtable-map (-> mod decls)
	       (lambda (k d) (with-access::Decl d (id) id))))
	 (trace-item "exports="
	    (hashtable-map (-> mod exports)
	       (lambda (k d) (with-access::Decl d (id) id))))
	 (trace-item "defs="
	    (hashtable-map (-> mod defs)
	       (lambda (k d) (with-access::Def d (id kind) (cons id kind)))))))
   mod)

;*---------------------------------------------------------------------*/
;*    module4-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (module4-parse::Module exprs path::bstring #!key (lib-path '()) cache-dir expand)

   (define (define-class? x)
      (match-case x
	 ((define-class . ?-) #t)
	 ((define-wide-class . ?-) #t)
	 ((define-final-class . ?-) #t)
	 ((define-abstract-class . ?-) #t)
	 (else #f)))
   
   (define (parse4 id path clauses expr body)
      (let ((mod (instantiate::Module
		    (id id)
		    (path path)
		    (version 4)
		    (expr expr)
		    (body body)
		    (resolved #f))))
	 (hashtable-put! *modules-by-path* path mod)
	 (let ((omod (hashtable-get *modules-by-id* (symbol->string id))))
	    (if omod
		(with-access::Module omod ((opath path))
		   (error/loc mod
		      (format "Module \"~a\" has already been declared in file ~s"
			 id opath)
		      path expr))
		(hashtable-put! *modules-by-id* (symbol->string id) mod)))
	 (let* ((nbody (append-map (lambda (c)
				      (module4-parse-clause c expr mod
					 lib-path cache-dir expand))
			  clauses))
		(nclasses (filter define-class? nbody))
		(others (filter (lambda (x) (not (define-class? x))) nbody)))
	    (with-access::Module mod (body)
	       (set! body (append! nclasses (append! others body)))))
	 (with-access::Module mod (inits)
	    (set! inits (delete-duplicates! inits)))
	 mod))

   (with-trace 'module5-parse "module4-parse"
      (trace-item "path=" path)
      (trace-item "exprs=" exprs)
      (let ((expr (car exprs)))
	 (match-case expr
	    ((module (and (? symbol?) ?id) :version 4 . ?clauses)
	     (parse4 id path clauses expr (cdr exprs)))
	    ((module (and (? symbol?) ?id) . ?clauses)
	     (parse4 id path clauses expr (cdr exprs)))
	    (else
	     (error/loc #f "Illegal expression" expr #f))))))

;*---------------------------------------------------------------------*/
;*    module4-parse-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (module4-parse-clause::pair-nil clause expr::pair mod::Module lib-path cache-dir expand)
   
   (define (unbound-error path id clause)
      (error/loc mod (format "Cannot find declaration in module \"~a\"" path)
	 id clause))
   
   (define (scope-error path id clause)
      (error/loc mod (format "Module \"~a\" does not export" path)
	 id clause))
   
   (define (hashtable-symbol-get table id)
      (hashtable-get table (symbol->string! id)))
   
   (define (hashtable-symbol-put! table id d)
      (hashtable-put! table (symbol->string! id) d))

   (define (procedure4 expr id args nexpr)
      (let ((lnexpr (localize nexpr expr)))
	 (multiple-value-bind (name type)
	    (parse-ident id expr)
	    (let ((decl (instantiate::Decl
			   (id name)
			   (alias name)
			   (mod mod)
			   (scope 'export)
			   (ronly #t)
			   (expr expr))))
	       (values name decl)))))
   
   (define (inline4 expr id args)
      (multiple-value-bind (name type)
	 (parse-ident id expr)
	 (let ((decl (instantiate::Decl
			(id name)
			(alias name)
			(mod mod)
			(scope 'export)
			(ronly #t)
			(expr expr))))
	 (values name decl))))
   
   (define (variable4 expr id)
      (multiple-value-bind (name type)
	 (parse-ident id expr)
	 (let ((decl (instantiate::Decl
			(id name)
			(alias name)
			(mod mod)
			(scope 'export)
			(expr expr))))
	    (values name decl))))

   (define (class4 expr id kind scope)
      (multiple-value-bind (name type)
	 (parse-ident id expr)
	 (let ((decl (instantiate::Decl
			(id name)
			(alias name)
			(mod mod)
			(scope scope)
			(ronly #t)
			(def #unspecified)
			(expr expr))))
	    (values name decl
	       (localize `(,kind ,@(cdr expr)) expr)))))

   (define (parse-import-all id path)
      (let ((imod::Module (module4-read path
			     :lib-path lib-path
			     :cache-dir cache-dir
			     :expand expand)))
	 (hashtable-for-each (-> imod exports)
	    (lambda (key d::Decl)
	       (let* ((alias (if id
				 (string->symbol
				    (format "~a.~a"
				       id (-> d alias)))
				 (-> d alias)))
		      (nd (duplicate::Decl d
			     (alias alias)
			     (scope 'import))))
		  (hashtable-put! (-> mod decls) key nd)
		  (hashtable-put! (-> mod imports) key nd)
		  )))
	 (module-add-libraries! mod (-> imod libraries))
	 (set! (-> mod inits)
	    (append! (-> mod inits) (list imod)))))

   (define (parse-import-some syms id path)
      (let ((imod::Module (module4-read path
			     :lib-path lib-path
			     :cache-dir cache-dir
			     :expand expand)))
	 (hashtable-for-each (-> imod exports)
	    (lambda (key d::Decl)
	       (when (memq (-> d id) syms)
		  (let* ((alias (if id
				    (string->symbol
				       (format "~a.~a"
					  id (-> d alias)))
				    (-> d alias)))
			 (nd (duplicate::Decl d
				(alias alias)
				(scope 'import))))
		     (hashtable-put! (-> mod decls) key nd)
		     (hashtable-put! (-> mod imports) key nd)
		     ))))
	 (module-add-libraries! mod (-> imod libraries))
	 (set! (-> mod inits)
	    (append! (-> mod inits) (list imod)))))
   
   (define (parse-import import expr::pair mod::Module expand)
      (match-case import
	 ((and (? symbol?) ?id)
	  (let ((path (module4-resolve-module-path mod id)))
	     (if (string? path)
		 (parse-import-all id path)
		 (error (-> mod id)
		    (format "Cannot find \"~a\" module source file" (-> mod id))
		    id))))
	 (((and (? symbol?) ?id) (and (? string?) ?path))
	  (parse-import-all id path))
	 ((??- (and (? symbol?) ?id) (and (? string?) ?path))
	  (parse-import-some (cddr (reverse import)) id path))
	 (else
	  (tprint "parse-import (4) not implemented " import))))
	  
   (define (parse-static::pair-nil clause expr::pair mod::Module expand)
      
      (define nbody '())
      
      (for-each-expr (lambda (expr src)
			(match-case expr
			   ((class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-class 'static)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (set! nbody (cons expr nbody))))
			   ((wide-class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-wide-class 'static)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (set! nbody (cons expr nbody))))
			   ((final-class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-final-class 'static)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (set! nbody (cons expr nbody))))
			   ((abstract-class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-abstract-class 'static)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (set! nbody (cons expr nbody))))))
	 (cdr clause))

      (reverse! nbody))
			   
   (define (parse-export::pair-nil clause expr::pair mod::Module expand)
      
      (define nbody '())
      
      (for-each-expr (lambda (expr src)
			(match-case expr
			   ((inline ?id . ?args)
			    (multiple-value-bind (id decl)
			       (inline4 expr id args)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)))
			   ((generic ?id . ?args)
			    (multiple-value-bind (id decl)
			       (procedure4 expr id args
				  `(define-generic ,(cons id args) #unspecified))
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)))
			   ((class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-class 'export)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)
			       (set! nbody (cons expr nbody))))
			   ((wide-class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-wide-class 'export)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)
			       (set! nbody (cons expr nbody))))
			   ((final-class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-final-class 'export)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)
			       (set! nbody (cons expr nbody))))
			   ((abstract-class ?k . ?rest)
			    (multiple-value-bind (id decl expr)
			       (class4 expr k 'define-abstract-class 'export)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)
			       (set! nbody (cons expr nbody))))
			   ((?id . ?args)
			    (multiple-value-bind (id decl)
			       (procedure4 expr id args
				  `(define ,(cons id args) #unspecified))
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)))
			   ((and ?id (? symbol?))
			    (multiple-value-bind (id decl)
			       (variable4 expr id)
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)))
			   (else
			    (error/loc mod "Illegal export4 clause" clause expr))))
	 (cdr clause))

      (reverse! nbody))
   
   (define (parse-include::pair-nil clause expr::pair mod::Module expand)
      (append-map (lambda (f)
		     (cond
			((not (string? f))
			 (error/loc mod "Illegal include clause" f clause))
			((module5-resolve-path f (-> mod path))
			 =>
			 (lambda (f)
			    (let ((exprs (call-with-input-file f
					    (lambda (p)
					       (port->sexp-list p #t)))))
			       (match-case exprs
				  (((directives . ?clauses) . ?rest)
				   (append 
				      (append-map (lambda (c)
						     (module4-parse-clause c clause
							mod lib-path cache-dir
							expand))
					 clauses)
				      rest))
				  (else
				   exprs)))))
			(else
			 (error/loc mod "Cannot find file" f clause))))
	 (cdr clause)))

   (define (parse-cond-expand clause expr mod expand)
      (let ((ec (expand clause)))
	 (if (epair? ec)
	     (module4-parse-clause ec expr mod lib-path cache-dir expand)
	     '())))
   
   (with-trace 'module5-parse "module4-parse-clause"
      (trace-item "clause=" clause)
      (trace-item "lib-path=" lib-path)

      (match-case clause
	 ((import :version 4 . ?imports)
	  (for-each (lambda (i) (parse-import i clause mod expand)) imports)
	  '())
	 ((import . ?imports)
	  (for-each (lambda (i) (parse-import i clause mod expand)) imports)
	  '())
	 ((export . ?bindings)
	  (parse-export clause expr mod expand))
	 ((main (and (? symbol?) ?main))
	  '())
	 ((include . ?-)
	  (parse-include clause expr mod expand))
	 ((library (? symbol?))
	  '())
	 ((library (? symbol?) . ?-)
	  '())
	 ((use . ?-)
	  '())
	 ((from . ?-)
	  '())
	 ((static . ?-)
	  (parse-static clause expr mod expand))
	 ((option . ?x)
	  (for-each eval x)
	  '())
	 ((cond-expand . ?-)
	  (parse-cond-expand clause expr mod expand))
	 ((?id . ?-)
	  (let ((plugin (assq id *plugins4*)))
	     (if plugin
		 ((cdr plugin) mod clause) 
		 (error/loc mod "Illegal module4 clause" clause expr))))
	 (else
	  (error/loc mod "Illegal module4 clause" clause expr)))))

;*---------------------------------------------------------------------*/
;*    module4-resolve-module-path ...                                  */
;*    -------------------------------------------------------------    */
;*    Module4 module identifier resolution. Contrary to module5,       */
;*    module4 path are relative to the direction from where the        */
;*    compiler is invoked, as are module4 .afile files.                */
;*---------------------------------------------------------------------*/
(define (module4-resolve-module-path mod::Module ident::symbol)
   
   (define (fallback dir ident)
      (any (lambda (suffix)
	      (let ((p (make-file-name dir
			  (format "~a.~a" ident suffix))))
		 (when (file-exists? p)
		    p)))
	 '("scm" "bgl")))
   
   (let* ((dir (pwd))
	  (afile (module4-load-afile (dirname (-> mod path)))))
      (if (pair? afile)
	  (let ((path (assq ident afile)))
	     (if (pair? path)
		 (cadr path)
		 (fallback dir ident)))
	  (fallback dir ident))))

;*---------------------------------------------------------------------*/
;*    *afiles* ...                                                     */
;*---------------------------------------------------------------------*/
(define *afiles* '())

;*---------------------------------------------------------------------*/
;*    module4-load-afile ...                                           */
;*---------------------------------------------------------------------*/
(define (module4-load-afile dir::bstring)
   (synchronize module-mutex
      (let loop ((dir dir))
	 (if (string=? dir "/")
	     '()
	     (let ((afile (assoc dir *afiles*)))
		(if (pair? afile)
		    (cdr afile)
		    (let ((aname (make-file-name dir ".afile")))
		       (if (file-exists? aname)
			   (let ((afile (call-with-input-file aname read)))
			      (set! *afiles* (cons (cons dir afile) *afiles*))
			      afile)
			   (loop (dirname dir))))))))))

;*---------------------------------------------------------------------*/
;*    check-unbounds ...                                               */
;*---------------------------------------------------------------------*/
(define (check-unbounds mod::Module)
   (with-trace 'module5 "check-unbounds"
      (trace-item "mod=" (-> mod id))
      (with-access::Module mod (decls (mid id) defs)
	 (let ((unbounds '()))
	    (hashtable-for-each decls
	       (lambda (k d)
		  (with-access::Decl d (def id scope (dmod mod))
		     (unless (or (isa? def Def) (not (eq? dmod mod)))
			(when (or (eq? scope 'export) (eq? scope 'static))
			   (set! unbounds (cons d unbounds)))))))
	    (when (pair? unbounds)
	       (trace-item "decls="
		  (hashtable-map decls
		     (lambda (k d)
			(with-access::Decl d (id) id))))
	       (for-each (lambda (d)
			    (with-access::Decl d (id expr)
			       (with-handler
				  exception-notify
				  (error/loc mod "Cannot find definition"
				     id expr))))
		  (reverse! unbounds))
	       (error mid "Unbound exported identifier"
		  (map (lambda (d) (with-access::Decl d (id) id))
		     unbounds)))))))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id src)
   (let* ((s (symbol->string id))
	  (l (string-length s)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i (-fx l 2))
	     (values id #unspecified))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (values (string->symbol (substring s 0 i))
		    (substring s (+fx i 2)))
		 (loop (+fx i 1))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    collect-defines! ...                                             */
;*---------------------------------------------------------------------*/
(define (collect-defines! mod body)
   
   (define (module-define! mod kind::symbol id::symbol type src)
      (with-access::Module mod (defs decls)
	 (let* ((name (symbol->string! id))
		(old (hashtable-get defs name))
		(decl (hashtable-get decls name)))
	    (if old
		(error/loc mod
		   (format "Identifier ~s has already been declared" name)
		   (with-access::Def old (expr) expr)
		   (with-access::Decl decl (expr) expr))
		(let ((def (instantiate::Def
				 (id id)
				 (type type)
				 (kind kind)
				 (expr src))))
		   (hashtable-put! defs name def)
		   (when decl
		      (with-access::Decl decl (scope (ddef def))
			 (with-access::Def def ((ddecl decl))
			    (case scope
			       ((export)
				(set! ddef def)
				(set! ddecl decl))
			       ((static)
				(set! ddecl decl))))))
		   def)))))
   
   (define (collect-define! mod expr)
      (match-case expr
	 ((define (and (? symbol?) ?id) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'variable name type expr)))
	 ((define ((and (? symbol?) ?id) . ?-) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'variable name type expr)))
	 ((define-inline ((and (? symbol?) ?id) . ?-) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'inline name type expr)))
	 ((define-generic ((and (? symbol?) ?id) . ?-) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'generic name type expr)))
	 ((define-macro ((and (? symbol?) ?name) . ?-) . ?-)
	  (module-define! mod 'macro name #unspecified expr))
	 ((define-expander (and (? symbol?) ?name) . ?-)
	  (module-define! mod 'expander name #unspecified expr))
	 ((begin . ?exprs)
	  (collect-defines! mod exprs))))

   (for-each (lambda (expr) (collect-define! mod expr)) body))

;*---------------------------------------------------------------------*/
;*    collect-classes! ...                                             */
;*---------------------------------------------------------------------*/
(define (collect-classes! mod::Module)

   (define (klass-def ci)
      (let ((id (class-info-id ci)))
	 (instantiate::KDef
	    (id id)
	    (depth (class-info-depth ci))
	    (type 'class)
	    (kind 'class)
	    (ronly #t)
	    (ctor (class-info-ctor ci))
	    (expr (class-info-expr ci))
	    (registration (class-info-registration ci))
	    (super (when (class-info-super ci)
		      (class-info-id (class-info-super ci))))
	    (kkind (class-info-kind ci))
	    (ci ci)
	    (properties (filter-map (lambda (p)
				       (when (eq? (prop-info-class p) id)
					  `((id . ,(prop-info-id p))
					    (expr . ,(prop-info-expr p))
					    (type . ,(prop-info-type p))
					    (ronly . ,(prop-info-ronly? p))
					    (defvalue . ,(prop-info-defv? p))
					    (value . ,(prop-info-value p))
					    (get . ,(prop-info-get p))
					    (set . ,(prop-info-set p)))))
			   (class-info-properties ci))))))

   (hashtable-for-each (-> mod classes)
      (lambda (k ci)
	 (with-access::Module mod (defs decls)
	    (let* ((name (symbol->string! (class-info-id ci)))
		   (old (hashtable-get defs name))
		   (decl (hashtable-get decls name)))
	       (if old
		   (error/loc mod
		      (format "Identifier ~s has already been declared" name)
		      (with-access::Def old (expr) expr)
		      (with-access::Decl decl (expr) expr))
		   (let ((def (klass-def ci)))
		      (hashtable-put! defs name def)
		      (if decl
			 (with-access::Decl decl (scope ronly (ddef def))
			    (set! ronly #t)
			    (with-access::Def def ((ddecl decl))
			       (set! ddef def)
			       (set! ddecl decl)))
			 ;; class needs to preserve the invariable that
			 ;; all class definition has an associated declation
			 (let* ((id (class-info-id ci))
				(decl (instantiate::Decl
					(id id)
					(alias id)
					(expr (class-info-expr ci))
					(mod mod)
					(scope 'static)
					(ronly #t)
					(def def))))
			    (hashtable-put! decls (symbol->string! id) decl)
			    (with-access::Def def ((ddecl decl))
			       (set! ddecl decl))))
		      def)))))))

;*---------------------------------------------------------------------*/
;*    ronly! ...                                                       */
;*---------------------------------------------------------------------*/
(define (ronly! mod::Module)

   (define (args-id args)
      (cond
	 ((null? args)
	  '())
	 ((pair? args)
	  (if (not (symbol? (car args)))
	      (args-id (cdr args))
	      (multiple-value-bind (name type)
		 (parse-ident (car args) args)
		 (cons name (args-id (cdr args))))))
	 (else
	  (multiple-value-bind (name type)
	     (parse-ident args args)
	     (list name)))))

   (define (binding-id binding)
      (multiple-value-bind (name type)
	 (if (pair? binding)
	     (parse-ident (car binding) binding)
	     (parse-ident binding binding))
	 name))
	  
   (define (bindings-id bindings)
      (map binding-id bindings))
   
   (define (ronly-exprs! exprs env defs)
      (let loop ((exprs exprs))
	 (cond
	    ((pair? exprs)
	     (ronly-expr! (car exprs) env defs)
	     (ronly-exprs! (cdr exprs) env defs))
	    ((null? exprs)
	     #unspecified)
	    (else
	     (ronly-expr! exprs env defs)))))
	  
   (define (ronly-expr! expr env defs)
      (when (pair? expr)
	 (match-case expr
	    ((set! (and (? symbol?) ?id) . ?val)
	     (ronly-expr! val env defs)
	     (let ((def (hashtable-get defs (symbol->string! id))))
		(when (isa? def Def)
		   (let ((l (memq id env)))
		      (unless (pair? l)
			 (with-access::Def def (ronly)
			    (when (eq? ronly #t)
			       (error/loc mod "Illegal assignment" id expr))
			    (set! ronly #f)))))))
	    ((define (and (? symbol?) ?id) ?expr)
	     (ronly-expr! expr env defs))
	    ((define ((and (? symbol?) ?id) . ?args) . ?body)
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-inline ((and (? symbol?) ?id) . ?args) . ?body)
	     (multiple-value-bind (name type)
		(parse-ident id (cadr expr))
		(let ((def (hashtable-get defs (symbol->string! name))))
		   (if (isa? def Def)
		       (with-access::Def def (ronly kind)
			  (unless (eq? ronly #unspecified)
			     (error/loc mod "Illegally mutated inline function"
				id expr))
			  (begin
			     (set! ronly #t)
			     (set! kind 'inline))))))
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-generic ((and (? symbol?) ?id) . ?args) . ?body)
	     (multiple-value-bind (name type)
		(parse-ident id (cadr expr))
		(let ((def (hashtable-get defs (symbol->string! name))))
		   (if (isa? def Def)
		       (with-access::Def def (ronly kind)
			  (unless (eq? ronly #unspecified)
			     (error/loc mod "Illegally mutated generic function"
				id expr))
			  (begin
			     (set! ronly #t)
			     (set! kind 'generic))))))
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-class (and (? symbol?) ?id) . ?-)
	     (multiple-value-bind (name type)
		(parse-ident id (cdr expr))
		(let ((def (hashtable-get defs (symbol->string! name))))
		   (if (isa? def Def)
		       (with-access::Def def (ronly kind)
			  (unless (eq? ronly #unspecified)
			     (error/loc mod "Illegally mutated class"
				id expr))
			  (begin
			     (set! ronly #t)
			     (set! kind 'class))))))
	     '(ronly-exprs! body (append (args-id args) env) defs))
	    ((begin . ?exprs)
	     (ronly-exprs! exprs env defs))
	    ((lambda ?args . ?exprs)
	     (ronly-exprs! exprs (append (args-id args) env) defs))
	    ((let (and (? symbol?) ?loop) ?bindings . ?exprs)
	     (multiple-value-bind (name type)
		(parse-ident loop (cdr expr))
		(let ((nenv (cons name (append (bindings-id bindings) env))))
		   (ronly-exprs! exprs nenv defs))))
	    (((or let* letrec letrec*) ?bindings . ?exprs)
	     (let ((nenv (append (bindings-id bindings) env)))
		(ronly-exprs! exprs env defs)))
	    (else
	     (ronly-exprs! expr env defs)))))
	 
   (with-access::Module mod (body defs)
      (for-each (lambda (expr)
		   (ronly-expr! expr '() defs))
	 body)
      (hashtable-for-each defs
	 (lambda (k d)
	    (with-access::Def d (ronly expr kind)
	       (when (eq? ronly #unspecified)
		  (set! ronly #t)
		  (match-case expr
		     ((define ?- (lambda . ?-))
		      (set! kind 'procedure))
		     ((define (?- . ?-) . ?-)
		      (set! kind 'procedure)))))))))

;*---------------------------------------------------------------------*/
;*    module5-checksum ...                                             */
;*---------------------------------------------------------------------*/
(define (module5-checksum! mod::Module)
   
   (define (add-hash n checksum)
      (bit-and #xffffff (+fx n checksum)))
   
   (define (scope-number scope)
      (case scope
	 ((export) 124)
	 ((static) 23544)
	 ((export) 33)
	 (else 8843)))
   
   (define (kind-number kind)
      (case kind
	 ((procedure) 98)
	 ((inline) 2443)
	 ((generic) 223)
	 ((class) 33455)
	 ((variable) 8739284)
	 (else 3493)))
   
   (with-access::Module mod (defs decls checksum)
      (when (<fx checksum 0)
	 (let ((cs (+fx (hashtable-size defs) (hashtable-size decls))))
	    (hashtable-for-each decls
	       (lambda (k d)
		  (with-access::Decl d (alias scope)
		     (set! cs (add-hash (scope-number scope) cs))
		     
		     (set! cs (add-hash (get-hashnumber k) cs))
		     (set! cs (add-hash (get-hashnumber alias) cs)))))
	    (hashtable-for-each defs
	       (lambda (k d)
		  (with-access::Def d (kind)
		     (set! cs (add-hash (+fx 3 (get-hashnumber k)) cs))
		     (set! cs (add-hash (kind-number kind) cs)))))
	    (set! checksum cs)))
      mod))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc mod msg obj container)
   (let ((id (if (isa? mod Module)
		 (with-access::Module mod (id) id)
		 "module5")))
      (match-case (cond
		   ((epair? obj) (cer obj))
		   ((epair? container) (cer container))
		   (else #f))
	 ((at ?fname ?loc) (error/location id msg obj fname loc))
	 (else (error id msg obj)))))

;*---------------------------------------------------------------------*/
;*    for-each-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (for-each-expr proc lst)
   (let loop ((lst lst))
      (when (pair? lst)
	 (proc (car lst) lst)
	 (loop (cdr lst)))))

;*---------------------------------------------------------------------*/
;*    module5-get-decl ...                                             */
;*---------------------------------------------------------------------*/
(define (module5-get-decl mod::Module id src)
   (with-access::Module mod (decls (mid id))
      (let ((decl (hashtable-get decls (symbol->string! id))))
	 (if (isa? decl Decl)
	     decl
	     (error/loc mod "Cannot find declaration" id src)))))

;*---------------------------------------------------------------------*/
;*    module5-get-def ...                                              */
;*---------------------------------------------------------------------*/
(define (module5-get-def mod::Module id src)
   (with-access::Module mod (defs decls (mid id) resolved)
      (unless resolved
	 (error/loc mod "Module definitions not resolved yet" id #f))
      (let ((def (hashtable-get defs (symbol->string! id))))
	 (if (isa? def Def)
	     def
	     (error/loc mod "Cannot find definition" id src)))))

;*---------------------------------------------------------------------*/
;*    module5-get-export-def ...                                       */
;*---------------------------------------------------------------------*/
(define (module5-get-export-def mod::Module id)
   (with-trace 'module5-resolve "module5-get-export-def"
      (trace-item "mod=" (-> mod id))
      (trace-item "id=" id)
      (with-access::Module mod (exports (mid id) resolved defs decls)
	 (unless resolved
	    (error/loc mod "Module definitions not resolved yet" id #f))
	 (let ((decl (hashtable-get exports (symbol->string! id))))
	    (if (isa? decl Decl)
		(with-access::Decl decl (def expr (dmod mod))
		   (if (isa? def Def)
		       def
		       (error/loc mod "Cannot find exported definition" id expr)))
		(error/loc mod "Cannot find exported declaration" id #f))))))

;*---------------------------------------------------------------------*/
;*    define-class-expander ...                                        */
;*---------------------------------------------------------------------*/
(define (define-class-expander mod::Module xenv)
   (lambda (x e)
      (let ((ci (parse-class x mod)))
	 ;; bind the class in the module
	 (let ((o (module5-get-class mod (class-info-id ci))))
	    (if o
		(error/loc mod
		   (format "Class \"~a\" has already been declared in module ~a"
		      (class-info-id ci) (-> mod id))
		   x x)
		(module5-bind-class! mod (class-info-id ci) ci)))
	 ;; check the super class
	 (when (class-info-super ci)
	    (let ((si (module5-get-class mod (class-info-super ci))))
	       (if si
		   ;; update the super class info and add additional props
		   (begin
		      (class-info-super-set! ci si)
		      (class-info-properties-set! ci
			 (append (class-info-properties si)
			    (class-info-properties ci)))
		      (cond
			 ((class-info-ctor ci)
			  ;; use the class ctor
			  (class-info-register-ctor-set! ci
			     (class-info-ctor ci)))
			 ((class-info-register-ctor si)
			  ;; use the super class ctor
			  (if (hashtable-contains? (-> mod imports)
				 (symbol->string! (class-info-id si)))
			      ;; imported class
			      (begin
				 (class-info-ctor-set! ci
				    `((@ class-constructor __object)
				      ,(class-info-id ci)))
				 (class-info-register-ctor-set! ci
				    `((@ class-constructor __object)
				      ,(class-info-id si))))
			      ;; local class
			      (begin
				 (class-info-ctor-set! ci
				    (class-info-ctor si))
				 (class-info-register-ctor-set! ci
				    (class-info-ctor si)))))))
		   (error/loc mod
		      (format "Cannot find \"~a\" super class"
			 (class-info-id ci))
		      (class-info-super ci) x))))
	 ;; install the expanders
	 (install-class-expanders ci xenv mod)
	 ;; expanded class registration form
	 (class-info-registration-set! ci (e (registration-expand ci mod) e))
	 #unspecified)))

;*---------------------------------------------------------------------*/
;*    install-class-expanders ...                                      */
;*---------------------------------------------------------------------*/
(define (install-class-expanders ci xenv mod)
   (install-module5-expander xenv
      (string->symbol (format "instantiate::~a" (class-info-id ci)))
      #f (instantiate-expander ci mod))
   (install-module5-expander xenv
      (string->symbol (format "with-access::~a" (class-info-id ci)))
      #f (with-access-expander ci mod)))

;*---------------------------------------------------------------------*/
;*    include-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (include-expander mod::Module)
   (lambda (x e)
      (match-case x
	 ((include ?path)
	  `(begin
	      ,@(map (lambda (x) (e x e))
		   (call-with-input-file path
		      (lambda (p) (port->sexp-list p))))))
	 (else
	  (error/loc mod "Wrong include format" x x)))))

;*---------------------------------------------------------------------*/
;*    module5-get-class ...                                            */
;*---------------------------------------------------------------------*/
(define (module5-get-class mod::Module id::symbol)
   (hashtable-get (-> mod classes) (symbol->string! id)))

;*---------------------------------------------------------------------*/
;*    module5-bind-class! ...                                          */
;*---------------------------------------------------------------------*/
(define (module5-bind-class! mod::Module id::symbol ci)
   (with-trace 'module5-resolve "module5-bind-class!"
      (trace-item "class=" id " module=" (-> mod id))
      (hashtable-put! (-> mod classes) (symbol->string! id) ci)))

;*---------------------------------------------------------------------*/
;*    localize ...                                                     */
;*---------------------------------------------------------------------*/
(define (localize new old)
   (if (epair? old)
       (econs (car new) (cdr new) (cer old))
       new))   

