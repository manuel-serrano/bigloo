;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Expand/map.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec  4 18:08:53 1992                          */
;*    Last change :  Wed Mar 13 06:53:19 2019 (serrano)                */
;*    Copyright   :  1992-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    `map' and `for-each' compile-time macro expansion.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_map
   (include "Tools/location.sch")
   (import  tools_misc
	    tools_error
	    tools_location
	    engine_param
	    type_type
	    ast_ident)
   (export  (expand-map ::obj ::procedure)
	    (expand-for-each ::obj ::procedure)
	    (expand-any ::obj ::procedure)
	    (expand-every ::obj ::procedure)
	    (expand-reduce ::obj ::procedure)
	    (expand-filter ::obj ::procedure)
	    (expand-map! ::obj ::procedure)
	    (expand-find ::obj ::procedure)
	    (expand-append-map ::obj ::procedure)
	    (expand-vector-for-each ::obj ::procedure)
	    (expand-vector-map ::obj ::procedure)
	    (expand-vector-map! ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    epairify! ...                                                    */
;*---------------------------------------------------------------------*/
(define (epairify! x res)
   (replace! x (epairify-rec res x)))

;*---------------------------------------------------------------------*/
;*    inline-map-lambda? ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-map-lambda? expr)
   (match-case expr
      ((? symbol?)
       #t)
      ((lambda . ?-)
       #t)
      ((begin ?expr)
       (inline-map-lambda? expr))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    list-expected ...                                                */
;*---------------------------------------------------------------------*/
(define (list-expected proc var loc)
   (if (location? loc)
       `((@ bigloo-type-error/location __error)
	 ,proc "list" ,var
	 ,(location-full-fname loc)
	 ,(location-pos loc))
       `((@ bigloo-type-error __error)
	 ,proc "list" ,var)))

;*---------------------------------------------------------------------*/
;*    expand-map ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-map x e)
   (match-case x
      ((?- ?-)
       (user-warning 'map "used with only two arguments" x)
       ''())
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'map)))
	      (head  (mark-symbol-non-user! (gensym 'head)))
	      (tail  (mark-symbol-non-user! (gensym 'tail)))
	      (ntail (mark-symbol-non-user! (gensym 'newtail)))
	      (loc   (find-location x))
	      (loop  (if *unsafe-type*
			 `(let ((,l ,list))
			     (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				 '()
				 ,(match-case fun
				     ((not (?- . ?-))
				      ;; la fonction n'est pas un appel
				      ;; fonctionnel,on peut donc generer
				      ;; du code qui ne cons
				      ;; pas trop!
				      `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
										       '())))
					  (let ,lname ((,l   ((@ cdr __r4_pairs_and_lists_6_3) ,l))
						       (,tail ,head))
					       (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
						   ,head
						   (let ((,ntail 
							  ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
											     '())))
						      ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail
											     ,ntail)
						      (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
							      ,ntail))))))
				     (else
				      `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) '() '())))
					  (let ,lname ((,l    ,l)
						       (,tail ,head))
					       (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
						   ((@ cdr __r4_pairs_and_lists_6_3) ,head)
						   (let ((,ntail 
							  ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
											     '())))
						      ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						      (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
							      ,ntail)))))))))
			 `(let ((,l ,list))
			     (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				 '()
				 ,(match-case fun
				     ((not (?- . ?-))
				      ;; meme remarque que ci-dessus.
				      `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,l)) '())))
					  (let ,lname ((,l    ((@ cdr __r4_pairs_and_lists_6_3) ,l))
						       (,tail ,head))
					       (cond
						  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
						   (let ((,ntail 
							    ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
											       '())))
						      ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						      (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
							 ,ntail)))
						  (((@ null? __r4_pairs_and_lists_6_3) ,l)
						   ,head)
						  (else
						   ,(list-expected "map" l loc))))))
				     (else
				      `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) '() '())))
					  (let ,lname ((,l    ,l)
						       (,tail ,head))
					       (cond
						  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
						   (let ((,ntail 
							  ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
											     '())))
						      ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						      (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
							      ,ntail)))
						  (((@ null? __r4_pairs_and_lists_6_3) ,l)
						   ((@ cdr __r4_pairs_and_lists_6_3) ,head))
						  (else
						   ,(list-expected "map" l loc)))))))))))
	      (body (epairify-propagate loop x))
	      (res (e body e)))
	  (epairify! x res)))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'map)))
	      (head  (mark-symbol-non-user! (gensym 'head)))
	      (tail  (mark-symbol-non-user! (gensym 'tail)))
	      (ntail (mark-symbol-non-user! (gensym 'newtail)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loc   (find-location x))
	      (loop  `(let ((,lfun ,fun))
			 ,(if *unsafe-type*
			      `(let ((,l ,list))
				  (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				      '()
				      ,(match-case fun
					  ((not (?- . ?-))
					   ;; la fonction n'est pas un appel
					   ;; fonctionnel,on peut donc generer
					   ;; du code qui ne cons
					   ;; pas trop!
					   `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
											    '())))
					       (let ,lname ((,l   ((@ cdr __r4_pairs_and_lists_6_3) ,l))
							    (,tail ,head))
						    (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
							,head
							(let ((,ntail 
							       ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
												  '())))
							   ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail
												  ,ntail)
							   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
								   ,ntail))))))
					  (else
					   `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) '() '())))
					       (let ,lname ((,l    ,l)
							    (,tail ,head))
						    (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
							((@ cdr __r4_pairs_and_lists_6_3) ,head)
							(let ((,ntail 
							       ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
												  '())))
							   ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
							   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
								   ,ntail)))))))))
			      `(let ((,l ,list))
				  (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				      '()
				      ,(match-case fun
					  ((not (?- . ?-))
					   ;; meme remarque que ci-dessus.
					   `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l)) '())))
					       (let ,lname ((,l    ((@ cdr __r4_pairs_and_lists_6_3) ,l))
							    (,tail ,head))
						    (cond
						       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
							(let ((,ntail 
							       ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
												  '())))
							   ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
							   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
								   ,ntail)))
						       (((@ null? __r4_pairs_and_lists_6_3) ,l)
							,head)
						       (else
							,(list-expected "map" l loc))))))
					  (else
					   `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) '() '())))
					       (let ,lname ((,l    ,l)
							    (,tail ,head))
						    (cond
						       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
							(let ((,ntail 
							       ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
												  '())))
							   ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
							   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)
								   ,ntail)))
						       (((@ null? __r4_pairs_and_lists_6_3) ,l)
							((@ cdr __r4_pairs_and_lists_6_3) ,head))
						       (else
							,(list-expected "map" l loc))))))))))))
	      (body (epairify-propagate loop x))
	      (res (e body e)))
	  (epairify! x res)))
      ((and (?- (and ?fun (? inline-map-lambda?)) ?l1 ?l2)
	    (? (lambda (_) *unsafe-type*)))
       (let ((ll1   (mark-symbol-non-user! (gensym 'll)))
	     (ll2   (mark-symbol-non-user! (gensym 'll)))
	     (head  (mark-symbol-non-user! (gensym 'head)))
	     (tail  (mark-symbol-non-user! (gensym 'tail)))
	     (ntail (mark-symbol-non-user! (gensym 'newtail)))
	     (lname (mark-symbol-non-user! (gensym 'map)))
	     (loc   (find-location x)))
	  (let* ((body (epairify-propagate
			`(let ((,ll1 ,l1)
			       (,ll2 ,l2))
			    (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
				'()
				,(match-case fun
				    ((not (?- . ?-))
				     ;; la fonction n'est pas un appel
				     ;; fonctionnel,
				     ;; on peut donc generer du code
				     ;; qui ne cons pas trop!
				     `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,ll1)
											    ((@ car __r4_pairs_and_lists_6_3) ,ll2))
										      '())))
					 (let ,lname ((,ll1   ((@ cdr __r4_pairs_and_lists_6_3) ,ll1))
						      (,ll2   ((@ cdr __r4_pairs_and_lists_6_3) ,ll2))
						      (,tail ,head))
					      (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
						  ,head
						  (let ((,ntail
							 ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,ll1)
												  ((@ car __r4_pairs_and_lists_6_3) ,ll2))
											    '())))
						     ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						     (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,ll1)
							     ((@ cdr __r4_pairs_and_lists_6_3) ,ll2)
							     ,ntail))))))
				    (else
				     `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) '() '())))
					 (let ,lname ((,ll1   ,ll1)
						      (,ll2   ,ll2)
						      (,tail ,head))
					      (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
						  ((@ cdr __r4_pairs_and_lists_6_3) ,head)
						  (let ((,ntail
							 ((@ cons __r4_pairs_and_lists_6_3) (,fun ((@ car __r4_pairs_and_lists_6_3) ,ll1)
												  ((@ car __r4_pairs_and_lists_6_3) ,ll2))
											    '())))
						     ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						     (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,ll1)
							     ((@ cdr __r4_pairs_and_lists_6_3) ,ll2)
							     ,ntail)))))))))
			x))
		 (res (e body e)))
	     (epairify! x res))))
      ((and (?- ?fun ?l1 ?l2)
	    (? (lambda (_) *unsafe-type*)))
       (let ((ll1   (mark-symbol-non-user! (gensym 'll)))
	     (ll2   (mark-symbol-non-user! (gensym 'll)))
	     (head  (mark-symbol-non-user! (gensym 'head)))
	     (tail  (mark-symbol-non-user! (gensym 'tail)))
	     (ntail (mark-symbol-non-user! (gensym 'newtail)))
	     (lname (mark-symbol-non-user! (gensym 'map)))
	     (lfun  (mark-symbol-non-user! (gensym 'fun)))
	     (loc   (find-location x)))
	  (let* ((body (epairify-propagate
			`(let ((,ll1 ,l1)
			       (,ll2 ,l2)
			       (,lfun ,fun))
			    (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
				'()
				,(match-case fun
				    ((not (?- . ?-))
				     ;; la fonction n'est pas un appel
				     ;; fonctionnel,
				     ;; on peut donc generer du code
				     ;; qui ne cons pas trop!
				     `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,ll1)
											     ((@ car __r4_pairs_and_lists_6_3) ,ll2))
										      '())))
					 (let ,lname ((,ll1   ((@ cdr __r4_pairs_and_lists_6_3) ,ll1))
						      (,ll2   ((@ cdr __r4_pairs_and_lists_6_3) ,ll2))
						      (,tail ,head))
					      (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
						  ,head
						  (let ((,ntail
							 ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,ll1)
												   ((@ car __r4_pairs_and_lists_6_3) ,ll2))
											    '())))
						     ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						     (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,ll1)
							     ((@ cdr __r4_pairs_and_lists_6_3) ,ll2)
							     ,ntail))))))
				    (else
				     `(let ((,head ((@ cons __r4_pairs_and_lists_6_3) '() '())))
					 (let ,lname ((,ll1   ,ll1)
						      (,ll2   ,ll2)
						      (,tail ,head))
					      (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
						  ((@ cdr __r4_pairs_and_lists_6_3) ,head)
						  (let ((,ntail
							 ((@ cons __r4_pairs_and_lists_6_3) (,lfun ((@ car __r4_pairs_and_lists_6_3) ,ll1)
												   ((@ car __r4_pairs_and_lists_6_3) ,ll2))
											    '())))
						     ((@ set-cdr! __r4_pairs_and_lists_6_3) ,tail ,ntail)
						     (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,ll1)
							     ((@ cdr __r4_pairs_and_lists_6_3) ,ll2)
							     ,ntail)))))))))
			x))
		 (res (e body e)))
	     (epairify! x res))))
      ((?- ?fun . ?lists)
       (let ((res `(map ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `map' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-for-each ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-for-each x e)
   (match-case x
      ((?- ?-)
       (user-warning 'for-each "used with only two arguments" x)
       #unspecified)
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'for-each)))
	      (loc   (find-location x))
	      (loop  (if *unsafe-type*
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				   (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))
				  (else
				   #t)))
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				   (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #t)
				  (else
				   ,(list-expected "for-each" l loc)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'for-each)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loc   (find-location x))
	      (loop  `(let ((,lfun ,fun))
			 ,(if *unsafe-type*
			      `(let ,lname ((,l ,list))
				    (cond
				       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
					(,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					(,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))
				       (else
					#t)))
			      `(let ,lname ((,l ,list))
				    (cond
				       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
					(,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					(,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))
				       (((@ null? __r4_pairs_and_lists_6_3) ,l)
					#t)
				       (else
					,(list-expected "for-each" l loc))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((and (?- (and ?fun (? inline-map-lambda?)) ?l1 ?l2)
	    (? (lambda (_) *unsafe-type*)))
       (let* ((ll1   (mark-symbol-non-user! (gensym 'll)))
	      (ll2   (mark-symbol-non-user! (gensym 'll)))
	      (lname (mark-symbol-non-user! (gensym 'for-each)))
	      (loop  `(let ,lname ((,ll1 ,l1)
				   (,ll2 ,l2))
			   (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
			       #t
			       (begin
				  (,fun ((@ car __r4_pairs_and_lists_6_3) ,ll1) (car ,ll2))
				  (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,ll1) ((@ cdr __r4_pairs_and_lists_6_3) ,ll2)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((and (?- ?fun ?l1 ?l2)
	    (? (lambda (_) *unsafe-type*)))
       (let* ((ll1   (mark-symbol-non-user! (gensym 'll)))
	      (ll2   (mark-symbol-non-user! (gensym 'll)))
	      (lname (mark-symbol-non-user! (gensym 'for-each)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loop  `(let ((,lfun ,fun))
			 (let ,lname ((,ll1 ,l1)
				      (,ll2 ,l2))
			      (if ((@ null? __r4_pairs_and_lists_6_3) ,ll1)
				  #t
				  (begin
				     (,lfun ((@ car __r4_pairs_and_lists_6_3) ,ll1) (car ,ll2))
				     (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,ll1) ((@ cdr __r4_pairs_and_lists_6_3) ,ll2))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?lists)
       (let ((res `(for-each ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `for-each' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-any ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-any x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'any)))
	      (loc   (find-location x))
	      (loop  (if *unsafe-type*
			 `(let ,lname ((,l ,list))
			       (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #f
				  (or (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
 				      (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #f)
				  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				   (or (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				       (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))))
				  (else
				   ,(list-expected "any" l loc)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'any)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loc   (find-location x))
	      (loop  `(let ((,lfun ,fun))
			 ,(if *unsafe-type*
			      `(let ,lname ((,l ,list))
				    (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
					#f
				        (or (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					    (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))
			      `(let ,lname ((,l ,list))
				    (cond
				       (((@ null? __r4_pairs_and_lists_6_3) ,l)
					#f)
				       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
					(or (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					    (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))))
				       (else
					,(list-expected "any" l loc))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?lists)
       (let ((res `(any ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `any' form" x))))
       
;*---------------------------------------------------------------------*/
;*    expand-every ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-every x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'every)))
	      (loc   (find-location x))
	      (loop  (if *unsafe-type*
			 `(let ,lname ((,l ,list))
			       (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #t
				   (let ((nv (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))))
				      (and nv (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))))))
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #t)
				  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				   (let ((nv (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))))
				      (and nv (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))
				  (else
				   ,(list-expected "every" l loc)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'every)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loc   (find-location x))
	      (loop  `(let ((,lfun ,fun))
			 ,(if *unsafe-type*
			      `(let ,lname ((,l ,list))
				    (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
					#t
					(let ((nv (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))))
					   (and nv (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))))))
			      `(let ,lname ((,l ,list))
				    (cond
				       (((@ null? __r4_pairs_and_lists_6_3) ,l)
					#t)
				       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
					(let ((nv (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))))
					   (and nv (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))
				       (else
					,(list-expected "every" l loc))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?lists)
       (let ((res `(every ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `every' form" x))))
       
;*---------------------------------------------------------------------*/
;*    expand-reduce ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-reduce x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?ridentity ?list)
       (let* ((l (mark-symbol-non-user! (gensym 'l)))
	      (a (mark-symbol-non-user! (gensym 'ans)))
	      (loop (mark-symbol-non-user! (gensym 'loop))))
	  (let ((res `(let ((,l ,list))
			 (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
			     ,ridentity
			     (let ,loop ((,l (cdr ,l))
					 (,a (car ,l)))
				  (if ((@ pair? __r4_pairs_and_lists_6_3) ,l)
				      (,loop (cdr ,l) (,fun (car ,l) ,a))
				      ,a))))))
	     (epairify! x (e res e)))))
      ((?- ?fun ?ridentity ?list)
       (let ((res `(reduce ,(e fun e) ,(e ridentity e) ,(e list e))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `reduce' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-filter ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-filter x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let ((loop (mark-symbol-non-user! (gensym 'loop)))
	     (l (mark-symbol-non-user! (gensym 'l)))
	     (h (mark-symbol-non-user! (gensym 'h)))
	     (nh (mark-symbol-non-user! (gensym 'nh)))
	     (hook (mark-symbol-non-user! (gensym 'hook))))
	  (let ((res `(let ((,hook (cons #f '())))
			 (let ,loop ((,l ,list)
				     (,h ,hook))
			      (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				  ((@ cdr __r4_pairs_and_lists_6_3) ,hook)
				  (if (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				      (let ((,nh ((@ cons __r4_pairs_and_lists_6_3) ((@ car __r4_pairs_and_lists_6_3) ,l) '())))
					 ((@ set-cdr! __r4_pairs_and_lists_6_3) ,h ,nh)
					 (,loop ((@ cdr __r4_pairs_and_lists_6_3) ,l) ,nh))
				      (,loop ((@ cdr __r4_pairs_and_lists_6_3) ,l) ,h)))))))
	     (epairify! x (e res e)))))
      ((?- ?fun . ?lists)
       (let ((res `(filter ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `filter' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-map! ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-map! x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let ((loop (mark-symbol-non-user! (gensym 'loop)))
	     (l (mark-symbol-non-user! (gensym 'l)))
	     (l0 (mark-symbol-non-user! (gensym 'l0))))
	  (let ((res `(let ((,l0 ,list))
			 (let ,loop ((,l ,l0))
			      (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
				  ,l0
				  (begin
				     ((@ set-car! __r4_pairs_and_lists_6_3)
				      ,l (,fun ((@ car __r4_pairs_and_lists_6_3) ,l)))
				     (,loop ((@ cdr __r4_pairs_and_lists_6_3) ,l))))))))
	     (epairify! x (e res e)))))
      ((?- ?fun . ?lists)
       (let ((res `(map! ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `map!' form" x))))
   
;*---------------------------------------------------------------------*/
;*    expand-find ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-find x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let ((lp (mark-symbol-non-user! (gensym 'lp)))
	     (lv (mark-symbol-non-user! (gensym 'list))))
	  (let ((res `(let ,lp ((,lv ,list))
			 (when ((@ pair? __r4_pairs_and_lists_6_3) ,lv)
			    (if (,fun ((@ car __r4_pairs_and_lists_6_3) ,lv))
				((@ car __r4_pairs_and_lists_6_3) ,lv)
				(,lp ((@ cdr __r4_pairs_and_lists_6_3) ,lv)))))))
	     (epairify! x (e res e)))))
      ((?- ?fun . ?lists)
       (let ((res `(find ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `find' form" x))))
   
;*---------------------------------------------------------------------*/
;*    expand-append-map ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-append-map x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let ((loop (mark-symbol-non-user! (gensym 'loop)))
	     (l (mark-symbol-non-user! (gensym 'l))))
	  (let ((res `(let ,loop ((,l ,list))
			 (if ((@ null? __r4_pairs_and_lists_6_3) ,l)
			     '()
			     ((@ append-2 __r4_pairs_and_lists_6_3)
			      (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
			      (,loop ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))))
	     (epairify! x (e res e)))))
      ((?- ?fun . ?lists)
       (let ((res `(append-map ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `append-map' form" x))))
   
;*---------------------------------------------------------------------*/
;*    expand-vector-for-each ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-vector-for-each x e)
   (match-case x
      ((?- ?-)
       (user-warning 'vector-for-each "used with only two arguments" x)
       #unspecified)
      ((?- (and ?fun (? inline-map-lambda?)) ?vec)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v     (mark-symbol-non-user! (gensym 'v)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lname (mark-symbol-non-user! (gensym 'vector-for-each)))
	      (loc   (find-location x))
	      (loop `(let* ((,v ,vec)
			    (,len (vector-length ,v)))
			(let ,lname ((,i 0))
			     (when ((@ <fx __r4_numbers_6_5_fixnum) ,i ,len)
				(,fun ((@ vector-ref-ur __r4_vectors_6_8) ,v ,i))
				(,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?vec)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v     (mark-symbol-non-user! (gensym 'v)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lfun  (mark-symbol-non-user! (gensym 'lfun)))
	      (lname (mark-symbol-non-user! (gensym 'vector-for-each)))
	      (loc   (find-location x))
	      (loop `(let* ((,lfun ,fun)
			    (,v ,vec)
			    (,len (vector-length ,v)))
			(let ,lname ((,i 0))
			     (when ((@ <fx __r4_numbers_6_5_fixnum) ,i ,len)
				(,lfun ((@ vector-ref-ur __r4_vectors_6_8) ,v ,i))
				(,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- (and ?fun (? inline-map-lambda?)) ?vec1 ?vec2)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v1    (mark-symbol-non-user! (gensym 'v1)))
	      (v2    (mark-symbol-non-user! (gensym 'v2)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lname (mark-symbol-non-user! (gensym 'vector-for-each)))
	      (loc   (find-location x))
	      (loop `(let* ((,v1 ,vec1)
			    (,v2 ,vec2)
			    (,len (vector-length ,v1)))
			(let ,lname ((,i 0))
			     (when ((@ <fx __r4_numbers_6_5_fixnum) ,i ,len)
				(,fun ((@ vector-ref-ur __r4_vectors_6_8) ,v1 ,i)
				   ((@ vector-ref __r4_vectors_6_8) ,v2 ,i))
				(,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?vec1 ?vec2)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v1    (mark-symbol-non-user! (gensym 'v1)))
	      (v2    (mark-symbol-non-user! (gensym 'v2)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lfun  (mark-symbol-non-user! (gensym 'lfun)))
	      (lname (mark-symbol-non-user! (gensym 'vector-for-each)))
	      (loc   (find-location x))
	      (loop `(let* ((,lfun fun)
			    (,v1 ,vec1)
			    (,v2 ,vec2)
			    (,len (vector-length ,v1)))
			(let ,lname ((,i 0))
			     (when ((@ <fx __r4_numbers_6_5_fixnum) ,i ,len)
				(,lfun ((@ vector-ref-ur __r4_vectors_6_8) ,v1 ,i)
				   ((@ vector-ref __r4_vectors_6_8) ,v2 ,i))
				(,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?vecs)
       (let ((res `(vector-for-each ,(e fun e) ,@(map (lambda (l) (e l e)) vecs))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `vector-for-each' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-vector-map ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-vector-map x e)
   (match-case x
      ((?- ?-)
       (user-warning 'vector-map "used with only two arguments" x)
       #unspecified)
      ((?- (and ?fun (? inline-map-lambda?)) ?vec)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v     (mark-symbol-non-user! (gensym 'v)))
	      (nv    (mark-symbol-non-user! (gensym 'nv)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map)))
	      (loc   (find-location x))
	      (loop `(let* ((,v ,vec)
			    (,len (vector-length ,v))
			    (,nv ((@ make-vector __r4_vectors_6_8) ,len)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,nv
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,nv ,i
				     (,fun ((@ vector-ref-ur __r4_vectors_6_8) ,v ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?vec)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v     (mark-symbol-non-user! (gensym 'v)))
	      (nv    (mark-symbol-non-user! (gensym 'nv)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lfun  (mark-symbol-non-user! (gensym 'lfun)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map)))
	      (loc   (find-location x))
	      (loop `(let* ((,lfun ,fun)
			    (,v ,vec)
			    (,len (vector-length ,v))
			    (,nv ((@ make-vector __r4_vectors_6_8) ,len)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,nv
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,nv ,i
				     (,lfun ((@ vector-ref-ur __r4_vectors_6_8) ,v ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- (and ?fun (? inline-map-lambda?)) ?vec1 ?vec2)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v1    (mark-symbol-non-user! (gensym 'v1)))
	      (v2    (mark-symbol-non-user! (gensym 'v2)))
	      (nv    (mark-symbol-non-user! (gensym 'nv)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map)))
	      (loc   (find-location x))
	      (loop `(let* ((,v1 ,vec1)
			    (,v2 ,vec2)
			    (,len (vector-length ,v1))
			    (,nv ((@ make-vector __r4_vectors_6_8) ,len)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,nv
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,nv ,i
				     (,fun ((@ vector-ref-ur __r4_vectors_6_8) ,v1 ,i)
					((@ vector-ref __r4_vectors_6_8) ,v2 ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?vec1 ?vec2)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v1    (mark-symbol-non-user! (gensym 'v1)))
	      (v2    (mark-symbol-non-user! (gensym 'v2)))
	      (nv    (mark-symbol-non-user! (gensym 'nv)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lfun  (mark-symbol-non-user! (gensym 'lfun)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map)))
	      (loc   (find-location x))
	      (loop `(let* ((,lfun fun)
			    (,v1 ,vec1)
			    (,v2 ,vec2)
			    (,len (vector-length ,v1))
			    (,nv ((@ make-vector __r4_vectors_6_8) ,len)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,nv
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,nv ,i
				     (,lfun ((@ vector-ref-ur __r4_vectors_6_8) ,v1 ,i)
					((@ vector-ref __r4_vectors_6_8) ,v2 ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?vecs)
       (let ((res `(vector-map ,(e fun e) ,@(map (lambda (l) (e l e)) vecs))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `vector-map' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-vector-map! ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-vector-map! x e)
   (match-case x
      ((?- ?-)
       (user-warning 'vector-map! "used with only two arguments" x)
       #unspecified)
      ((?- (and ?fun (? inline-map-lambda?)) ?vec)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v     (mark-symbol-non-user! (gensym 'v)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map!)))
	      (loc   (find-location x))
	      (loop `(let* ((,v ,vec)
			    (,len (vector-length ,v)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,v
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,v ,i
				     (,fun ((@ vector-ref-ur __r4_vectors_6_8) ,v ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?vec)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v     (mark-symbol-non-user! (gensym 'v)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lfun  (mark-symbol-non-user! (gensym 'lfun)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map!)))
	      (loc   (find-location x))
	      (loop `(let* ((,lfun ,fun)
			    (,v ,vec)
			    (,len (vector-length ,v)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,v
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,v ,i
				     (,lfun ((@ vector-ref-ur __r4_vectors_6_8) ,v ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- (and ?fun (? inline-map-lambda?)) ?vec1 ?vec2)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v1    (mark-symbol-non-user! (gensym 'v1)))
	      (v2    (mark-symbol-non-user! (gensym 'v2)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map!)))
	      (loc   (find-location x))
	      (loop `(let* ((,v1 ,vec1)
			    (,v2 ,vec2)
			    (,len (vector-length ,v1)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,v1
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,v1 ,i
				     (,fun ((@ vector-ref-ur __r4_vectors_6_8) ,v1 ,i)
					((@ vector-ref __r4_vectors_6_8) ,v2 ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?vec1 ?vec2)
       (let* ((i     (mark-symbol-non-user! (gensym 'i)))
	      (v1    (mark-symbol-non-user! (gensym 'v1)))
	      (v2    (mark-symbol-non-user! (gensym 'v2)))
	      (len   (mark-symbol-non-user! (gensym 'len)))
	      (lfun  (mark-symbol-non-user! (gensym 'lfun)))
	      (lname (mark-symbol-non-user! (gensym 'vector-map!)))
	      (loc   (find-location x))
	      (loop `(let* ((,lfun fun)
			    (,v1 ,vec1)
			    (,v2 ,vec2)
			    (,len (vector-length ,v1)))
			(let ,lname ((,i 0))
			     (if ((@ =fx __r4_numbers_6_5_fixnum)  ,i ,len)
				 ,v1
				 (begin
				    ((@ vector-set-ur! __r4_vectors_6_8)
				     ,v1 ,i
				     (,lfun ((@ vector-ref-ur __r4_vectors_6_8) ,v1 ,i)
					((@ vector-ref __r4_vectors_6_8) ,v2 ,i)))
				    (,lname ((@ +fx __r4_numbers_6_5_fixnum) ,i 1))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?vecs)
       (let ((res `(vector-map! ,(e fun e) ,@(map! (lambda (l) (e l e)) vecs))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `vector-map!' form" x))))

