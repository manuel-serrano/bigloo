;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/thread.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  2 16:39:03 2003                          */
;*    Last change :  Tue Aug  9 10:52:02 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Display thread monitoring                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem_thread
   (include "html.sch")
   (import  html
	    bmem_tools
	    bmem)
   (export  (make-thread-table thinfo)))

;*---------------------------------------------------------------------*/
;*    make-thread-table ...                                            */
;*---------------------------------------------------------------------*/
(define (make-thread-table thinfo)
   (let ((cs (context-switches thinfo))
	 (sa (scheduler-awake thinfo)))
      (html-div :class "profile"
		(html-table
		 :width "50%"
		 :cellpadding "0"
		 :cellspacing "10"
		 `(,(html-tr `(,(html-th :align "left"
					 "Number of fair thread context switches:")
			       ,(html-td :align "right"
					 (integer->string cs))))
		   ,(html-tr `(,(html-th :align "left"
					 "Number of scheduler awake:")
			       ,(html-td :align "right"
					 (integer->string sa))))
		   ,(html-tr `(,(html-td :align "right"
					 :class "olegend"
					 "Total number of context switches:")
			       ,(html-td :align "left"
					 :class "osize"
					 (integer->string (+ cs sa))))))))))

;*---------------------------------------------------------------------*/
;*    context-switches ...                                             */
;*---------------------------------------------------------------------*/
(define (context-switches thinfo)
   (let ((cell (assq 'context-switches (cdr thinfo))))
      (if (pair? cell)
	  (cadr cell)
	  0)))
			  
;*---------------------------------------------------------------------*/
;*    scheduler-awake ...                                              */
;*---------------------------------------------------------------------*/
(define (scheduler-awake thinfo)
   (let ((cell (assq 'scheduler-awake (cdr thinfo))))
      (if (pair? cell)
	  (cadr cell)
	  0)))
			  
