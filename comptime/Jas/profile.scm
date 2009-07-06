(module jas_profile
   (import jas_lib)
   (export (jas-profile::pair-nil l::pair-nil)
	   *jas-profile-mode*)
   (static (class _env this
    jstring has_clinit clinit res names lnames lines
	   ))
   )

(define *jas-profile-mode* 0)

;;
;; Entry point
;;
(define (jas-profile::pair-nil l::pair-nil)
   (if (>fx *jas-profile-mode* 0)
       ;; CARE use profile-mode == 1 to profile only functions entry points
       (let ( (r (profiling l)) )
	  ; (pp r)
	  r )
       l ))


;;
;; Main function
;;
(define (profiling l)
   (match-case l
      ((?key (and ?this (? symbol?))
	     (and ?extend (? symbol?))
	     (and ?implements ((? symbol?) ...))
	     (declare . ?decls) . ?infos )
       (let ( (env (make-env this decls infos)) )
	  `(,key ,this ,extend ,implements
		 (declare ,@(profile-declarations env decls))
		 ,@(profile-infos env infos) )))
       (else (error "jas" "bad module definition" l)) ))

;;
;;
;;
(define (make-env this decls infos)
   (define (get-id str l)
      (if (null? l)
	  #f
	  (let ( (decl (car l)) )
	     (let ( (name (car decl)) (val (cadr decl)) )
		(let ( (ty (car val)) (def (cdr val)) )
		   (if (eq? ty 'class)
		       (if (string=? (cadr def) str)
			   name
			   (get-id str (cdr l)) )
		       (if (string=? (cadddr def) str)
			   name
			   (get-id str (cdr l)) )))))))
   (define (patch-decl val)
      (let ( (r (gensym "__prof")) )
	 (set-cdr! val (cons `(,r ,val) (cdr val)))
	 r ))
   (define (get-name var l)
      (cond ((null? l) (error "jas" "undef" var))
	    ((eq? (caar l) var)
	     (car (cddddr (cadar l))) )
	    (else (get-name var (cdr l))) ))
   (define (get-lnames infos decls)
      (cond ((null? infos) '())
	    ((memq (caar infos) '(sourcefile sde fields))
	     (get-lnames (cdr infos) decls) )
	    (else
	     (map (lambda (m) (get-name (cadr m) decls)) infos) )))
   (let ( (vclinit (get-id "<clinit>" decls))
	  (lnames (get-lnames infos decls)) )
      (instantiate::_env
	 (this this)
	 (jstring (or (get-id "java.lang.String" decls)
		      (patch-decl `(class () "java.lang.String")) ))
	 (has_clinit vclinit)
	 (clinit (or vclinit (gensym "__profiler")))
	 (res '__profile_res)
	 (names '__profile_names)
	 (lines '__profile_lines)
	 (lnames lnames)
	 )))

;;
;; Declaration
;;
(define (profile-declarations env decls)
   (with-access::_env env (this jstring has_clinit clinit res names lines)
      (define (mf name ty str)
	 `(,name (field ,this (public static) ,ty ,str)) )
      `(,(mf lines '(vector (vector int)) "__profiler__lines")
	,(mf names `(vector ,jstring) "__profiler__names")
	,(mf res '(vector (vector int)) "__profiler__res")
	,@(if has_clinit
	      '()
	      `((,clinit (method ,this (public static) void "<clinit>"))) )
	,@decls )))

;;
;; Code
;;
(define (profile-infos env infos)
   (define (get id l)
      (cond ((null? l) (error "jas" "cant reach clinit" id))
	    ((eq? (cadar l) id)
	     (cdddar l) )
	    (else (get id (cdr l))) ))
   (cond
      ((null? infos) '())
      ((or (eq? (caar infos) 'sourcefile) (eq? (caar infos) 'sde))
	  (cons (car infos) (profile-infos env (cdr infos))) )
      ((eq? (caar infos) 'fields)
       (with-access::_env env (this has_clinit clinit res names lines)
	  (cons `(fields ,res ,names ,lines ,@(cdar infos))
		(profile-infos env (cdr infos)) )))
      (else
       (let ( (extra (profile-extra-clinit env infos)) )
	  (let ( (r (profile-methods env infos)) )
	     (with-access::_env env (has_clinit clinit)
		(if has_clinit
		    (let ( (slot (get clinit r)) )
		       (set-cdr! slot
				 (append extra (cdr slot)) )
		       r )
		    (cons `(,clinit () () ,@extra) r) )))))))

(define (profile-methods env methods)
   (define (profile-method i m)
      (match-case m
	 ((method ?gname ?params ?locals . ?code)
	  `(method ,gname ,params ,locals ,@(profile-code env i code)) )
	 (else (error "as" "bad method definition" m)) ))
   (let loop ( (l methods) (i 0) (r '()) )
      (if (null? l)
	  (reverse! r)
	  (loop (cdr l) (+fx i 1) (cons (profile-method i (car l)) r)) )))

(define (profile-code env cur-fnt l)
   (define (prof i)
      (with-access::_env env (res)
	 `(
	   (iastore)
	   (iadd)
	   (iconst_1)
	   (iaload)
	   (dup2)
	   ,(push-int i)
	   (aaload)
	   ,(push-int cur-fnt)
	   (getstatic ,res)
;	   (invokestatic foreign-print)
;	   (ldc ,(string-append (integer->string cur-fnt) " -> "
;				(integer->string i) ))
	   )))
   (define (skip l i r extra)
      (cond ((null? l) (error "profile" "empty basic block" l))
	    ((or (not (pair? (car l)))
		 (memq (caar l) '(handler line comment localvar)) )
	     (skip (cdr l) i (cons (car l) r) extra) )
	    (else
	     (walk l i (append extra r)) )))
   (define (walk l i r)
      (cond ((null? l) (reverse! r))
	    ((not (pair? (car l)))
	     (skip l (+fx i 1) r (prof i)) )
	    (else (walk (cdr l) i (cons (car l) r))) ))
   (walk (cons '***start*** l) 0 '()) )

(define (push-int n)      
   (case n
      ((-1) '(iconst_m1))
      ((0)  '(iconst_0))
      ((1)  '(iconst_1))
      ((2)  '(iconst_2))
      ((3)  '(iconst_3))
      ((4)  '(iconst_4))
      ((5)  '(iconst_5))
      (else
       (cond ((and (> n -129) (< n 128)) `(bipush ,n))
	     ((and (> n -32769) (< n 32768)) `(sipush ,n))
	     (else `(ldc ,n)) ))))

;;
;; Creation of vectors in clinit
;;
(define (profile-extra-clinit env methods)
   (with-access::_env env (jstring res names lines lnames)
      (let ( (size (length lnames)) )
	 `(,@(profile-make-names size names lnames jstring)
	     ,@(profile-make-lines size lines methods)
	     ,@(profile-make-res size res methods) ))))

(define (profile-make-names size names l jstring)
   (define (walk code i names)
      (if (null? names)
	 code
	 (walk (cons* '(aastore) `(ldc ,(car names)) (push-int i) '(dup) code)
	       (+fx i 1)
	       (cdr names) )))
   (reverse!
    (cons `(putstatic ,names)
	  (walk `((anewarray ,jstring) ,(push-int size)) 0 l) )))

(define (profile-make-lines size lines methods)
   (define (skip l)
      (cond ((null? l) '())
	    ((or (not (pair? (car l)))
		 (memq (caar l) '(handler line comment localvar)) )
	     (skip (cdr l)))
	    (else l) ))
   (define (do-labs l i r)
      (cond ((null? l) (reverse! r))
	    ((not (pair? (car l)))
	     (do-labs (skip (cdr l)) i (cons i r)) )
	    (else (do-labs (cdr l) (+fx i 1) r)) ))
   (define (do-labels m)
      (match-case m
	 ((method ?gname ?params ?locals . ?code)
	  (do-labs (cons '***start*** code) 0 '()) )
	 (else (error "as" "bad method definition" m)) ))
   (define (walk1 code i labs)
      (if (null? labs)
	  code
	  (walk1 (cons* '(iastore)
			(push-int (car labs))
			(push-int i)
			'(dup)
			code )
		 (+fx i 1)
		 (cdr labs) )))
   (define (walk code i methods)
      (if (null? methods)
	 code
	 (walk (let ( (labs (do-labels (car methods))) )
		  (cons* '(aastore)
			 (walk1 (cons* '(newarray int)
				       (push-int (length labs))
				       (push-int i)
				       '(dup)
				       code )
				0
				labs )))
	       (+fx i 1)
	       (cdr methods) )))
   (reverse!
    (cons `(putstatic ,lines)
	  (walk `((anewarray (vector int)) ,(push-int size)) 0 methods) )))


(define (profile-make-res size res methods)
   (define (skip l)
      (cond ((null? l) '())
	    ((or (not (pair? (car l)))
		 (memq (caar l) '(handler line comment localvar)) )
		 (skip (cdr l)))
	    (else l) ))
   (define (nb-labs l r)
      (cond ((null? l) r)
	    ((not (pair? (car l)))
	     (nb-labs (skip (cdr l)) (+fx r 1)) )
	    (else (nb-labs (cdr l) r)) ))
   (define (nb-labels m)
      (match-case m
	 ((method ?gname ?params ?locals . ?code)
	  (nb-labs (cons '***start*** code) 0) )
	 (else (error "as" "bad method definition" m)) ))
   (define (walk code i methods)
      (if (null? methods)
	 code
	 (walk (cons* '(aastore)
		      '(newarray int)
		      (push-int (nb-labels (car methods)))
		      (push-int i)
		      '(dup)
		      code )
	       (+fx i 1)
	       (cdr methods) )))
   (reverse!
    (cons `(putstatic ,res)
	  (walk `((anewarray (vector int)) ,(push-int size)) 0 methods) )))
