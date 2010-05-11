;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/map.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec  4 18:08:53 1992                          */
;*    Last change :  Tue May 11 13:45:14 2010 (serrano)                */
;*    Copyright   :  1992-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    `map' and `for-each' compile-time macro expansion.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_map
   (import tools_misc
	   tools_error
	   engine_param
	   type_type
	   ast_ident)
   (export (expand-map      ::obj ::procedure)
	   (expand-for-each ::obj ::procedure)
	   (expand-any?     ::obj ::procedure)
	   (expand-every?   ::obj ::procedure)
	   (expand-reduce   ::obj ::procedure)))

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
						   ((@ error __error)
						    "map"
						    "argument not a list"
						    ,l))))))
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
						   ((@ error __error)
						    "map"
						    "argument not a list"
						    ,l)))))))))))
	      (body (epairify-propagate loop x))
	      (res (e body e)))
	  (epairify! x res)))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'map)))
	      (head  (mark-symbol-non-user! (gensym 'head)))
	      (tail  (mark-symbol-non-user! (gensym 'tail)))
	      (ntail (mark-symbol-non-user! (gensym 'newtail)))
	      (lfun   (mark-symbol-non-user! (gensym 'fun)))
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
							((@ error __error)
							 "map"
							 "argument not a list"
							 ,l))))))
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
							((@ error __error)
							 "map"
							 "argument not a list"
							 ,l))))))))))))
	      (body (epairify-propagate loop x))
	      (res (e body e)))
	  (epairify! x res)))
      ((?- (and ?fun (? inline-map-lambda?)) ?l1 ?l2)
       (let ((ll1   (mark-symbol-non-user! (gensym 'll)))
	     (ll2   (mark-symbol-non-user! (gensym 'll)))
	     (head  (mark-symbol-non-user! (gensym 'head)))
	     (tail  (mark-symbol-non-user! (gensym 'tail)))
	     (ntail (mark-symbol-non-user! (gensym 'newtail)))
	     (lname (mark-symbol-non-user! (gensym 'map))))
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
      ((?- ?fun ?l1 ?l2)
       (let ((ll1   (mark-symbol-non-user! (gensym 'll)))
	     (ll2   (mark-symbol-non-user! (gensym 'll)))
	     (head  (mark-symbol-non-user! (gensym 'head)))
	     (tail  (mark-symbol-non-user! (gensym 'tail)))
	     (ntail (mark-symbol-non-user! (gensym 'newtail)))
	     (lname (mark-symbol-non-user! (gensym 'map)))
	     (lfun  (mark-symbol-non-user! (gensym 'fun))))
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
				   ((@ error __error) "for-each"
						      "argument not a list"
						      ,l)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'for-each)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
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
					((@ error __error) "for-each"
							   "argument not a list"
							   ,l))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- (and ?fun (? inline-map-lambda?)) ?l1 ?l2)
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
      ((?- ?fun ?l1 ?l2)
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
;*    expand-any? ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-any? x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'any)))
	      (loop  (if *unsafe-type*
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #f)
				  ((,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				   #t)
				  (else
				   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #f)
				  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				   (if (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				       #t
				       (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))))
				  (else
				   ((@ error __error) "any?"
						      "argument not a list"
						      ,l)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'any)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loop  `(let ((,lfun ,fun))
			 ,(if *unsafe-type*
			      `(let ,lname ((,l ,list))
				    (cond
				       (((@ null? __r4_pairs_and_lists_6_3) ,l)
					#f)
				       ((,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					#t)
				       (else
					(,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))))
			      `(let ,lname ((,l ,list))
				    (cond
				       (((@ null? __r4_pairs_and_lists_6_3) ,l)
					#f)
				       (((@ pair? __r4_pairs_and_lists_6_3) ,l)
					(if (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					    #t
					    (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))))
				       (else
					((@ error __error) "any?"
							   "argument not a list"
							   ,l))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?lists)
       (let ((res `(any? ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `any?' form" x))))
       
;*---------------------------------------------------------------------*/
;*    expand-every? ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-every? x e)
   (match-case x
      ((?- (and ?fun (? inline-map-lambda?)) ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'every)))
	      (loop  (if *unsafe-type*
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #t)
				  ((,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))
				  (else
				   #f)))
			 `(let ,lname ((,l ,list))
			       (cond
				  (((@ null? __r4_pairs_and_lists_6_3) ,l)
				   #t)
				  (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				   (if (,fun ((@ car __r4_pairs_and_lists_6_3) ,l))
				       (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))
				       #f))
				  (else
				   ((@ error __error) "every?"
						      "argument not a list"
						      ,l)))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun ?list)
       (let* ((l     (mark-symbol-non-user! (gensym 'l)))
	      (lname (mark-symbol-non-user! (gensym 'every)))
	      (lfun  (mark-symbol-non-user! (gensym 'fun)))
	      (loop  `(let ((,lfun ,fun))
			 (if *unsafe-type*
			     `(let ,lname ((,l ,list))
				   (cond
				      (((@ null? __r4_pairs_and_lists_6_3) ,l)
				       #t)
				      ((,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
				       (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l)))
				      (else
				       #f)))
			     `(let ,lname ((,l ,list))
				   (cond
				      (((@ null? __r4_pairs_and_lists_6_3) ,l)
				       #t)
				      (((@ pair? __r4_pairs_and_lists_6_3) ,l)
				       (if (,lfun ((@ car __r4_pairs_and_lists_6_3) ,l))
					   (,lname ((@ cdr __r4_pairs_and_lists_6_3) ,l))
					   #f))
				      (else
				       ((@ error __error) "every?"
							  "argument not a list"
							  ,l))))))))
	  (let ((res (e loop e)))
	     (epairify! x res))))
      ((?- ?fun . ?lists)
       (let ((res `(every? ,(e fun e) ,@(map (lambda (l) (e l e)) lists))))
	  (epairify! x res)))
      (else
       (error #f "Illegal `every?' form" x))))
       
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
       
       
   
