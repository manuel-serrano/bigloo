;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/csv/examples/gflu.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 23 10:27:33 2012                          */
;*    Last change :  Thu Feb 23 10:53:55 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An example of the CSV parsing facility                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gflu
   (library csv)
   (main main))

;*---------------------------------------------------------------------*/
;*    gflu-base-url ...                                                */
;*---------------------------------------------------------------------*/
(define gflu-base-url 
   "http://www.google.org/flutrends/about/data/flu")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((url gflu-base-url))
      (args-parse (cdr argv)
	 (("-c" ?country (help "Select one particular country"))
	  (set! url (string-append url "/" country)))
	 ((("-h" "--help") (help "Help"))
	  (args-parse-usage #f))
	 (else
	  (args-parse-usage #f)))
      (flutrends (string-append url "/data.txt"))))

;*---------------------------------------------------------------------*/
;*    flutrends ...                                                    */
;*---------------------------------------------------------------------*/
(define (flutrends url)
   (call-with-input-file url
      (lambda (p)
	 (let loop ((i 0))
	    (when (<fx i 11)
	       (read-line p)
	       (loop (+fx i 1))))
	 (for-each print (read-csv-records p)))))
	       
       
