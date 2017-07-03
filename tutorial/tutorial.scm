;*=====================================================================*/
;*    serrano/prgm/project/bigloo/tutorial/tutorial.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 28 22:23:35 1997                          */
;*    Last change :  Thu Oct 10 14:34:48 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The CD cover drawer                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tutorial
   (include "tex.sch")
   (import  tex
	    front
	    back
	    cdfile
	    (*xmcd-db* xmcd))
   (extern  (macro printf::int (string int) "printf"))
   (export  *height*
	    *side-width*
	    *side-margin*
	    *front-width*
	    *front-margin*
	    *front-title-height*
	    *double-frame-size*
	    *title-song-space* 
	    *back-width*
	    *back-margin*
	    *back-height*
	    *max-songs*
	    *src*)
   (main     main))

;*---------------------------------------------------------------------*/
;*    Action to execute                                                */
;*---------------------------------------------------------------------*/
(define *action* 'adddb)

;*---------------------------------------------------------------------*/
;*    verbosity                                                        */
;*---------------------------------------------------------------------*/
(define *verbose* 1)

;*---------------------------------------------------------------------*/
;*    file handling ...                                                */
;*---------------------------------------------------------------------*/
(define *src* "toto-98.cd")
(define *in* (current-input-port))

;*---------------------------------------------------------------------*/
;*    CD boxes dimensions                                              */
;*    -------------------------------------------------------------    */
;*    Here are the CD boxes dimensions:                                */
;*                                                                     */
;*           ^ +----------------------+-+-------------------+          */
;*           | |   Back Part.......   | |+-----------------+| ^        */
;*           | |                      |.||                 || | theight*/
;*           | |     ............     |.|+-----------------+| V ^      */
;*           | |         ....         |.|+-----------------+|   V      */
;*    height | |       ........       |.||                 || title-   */
;*           | |        ......        |.||                 ||  song-   */
;*           | |          ..          |.||                 ||   space  */
;*           | |       ........       | ||                 ||          */
;*           | |       ........       |.||                 ||          */
;*           | |                      | |+-----------------+|          */
;*           V +----------------------+-+-------------------+          */
;*                                                                     */
;*             <------- bwidth -------> <----- fwidth ------>          */
;*                                  <swidth>                           */
;*---------------------------------------------------------------------*/
;*--- height ----------------------------------------------------------*/
(define *height* 12.1)
;*--- side ------------------------------------------------------------*/
(define *side-width* 0.7)
(define *side-margin* 0.15)
;*--- front -----------------------------------------------------------*/
(define *front-width* *height*)
(define *front-margin* 0.2)
(define *front-title-height* 2)
(define *double-frame-size* 0.1)
(define *title-song-space* 0.1)
;*--- back ------------------------------------------------------------*/
(define *back-width* 13.7)
(define *back-margin* 0.3)
(define *back-height* (- *height* 0.2))
;*--- songs & author --------------------------------------------------*/
(define *max-songs* 16)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; we parse the arguments
   (parse-args (car argv) (cdr argv))
   ;; start io
   (start-io!)
   (unwind-protect
      ;; we parse the input file
      (multiple-value-bind (title author editor notes musicians songs id kind)
	 (parse-file *in*)
	 (case *action*
	    ((cover)
	     (cover title author editor notes musicians songs))
	    ((adddb)
	     (add-xmcd-db id kind title author songs))
	    (else
	     (error "cdisc" "Unknown action" *action*))))
      (stop-io!)))

;*---------------------------------------------------------------------*/
;*    cover ...                                                        */
;*---------------------------------------------------------------------*/
(define (cover title author editor notes musicians songs)
   ;; we emit the tex prelude
   (tex-prelude)
   ;; the back cover
   (tex-environment "document"
		    ""
		    (tex-psprelude)
		    (back-cover title author editor musicians songs)
		    ;; the front cover
		    (front-cover title author editor notes musicians songs))
   ;; the tex postlude
   (tex-postlude))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args name args)
   (define (usage args-parse-usage)
      (print "usage: " name)
      (newline)
      (args-parse-usage #f)
      (exit 0))
   (args-parse args
      (("-help" (help "This help message"))
       (usage args-parse-usage))
      (("-s" (help "Silent mode"))
       (set! *verbose* 0))
      (("-v" (help "Verbose mode"))
       (set! *verbose* 2))
      (("-xmcddb" ?dir (help "The xmcd database directory"))
       (set! *xmcd-db* dir))
      (("-cover" (help "Produce cover"))
       (set! *action* 'cover))
      (("-adddb" (help "Add to data base"))
       (set! *action* 'adddb))
      (else
       (if (string? *src*)
	   (usage args-parse-usage)
	   (set! *src* else)))))
			 
;*---------------------------------------------------------------------*/
;*    start-io! ...                                                    */
;*---------------------------------------------------------------------*/
(define (start-io!)
   (if (string? *src*)
       (begin
	  (if *verbose* (fprint (current-error-port) *src* #\:))
	  (set! *in* (open-input-file *src*)))
       (if (not (input-port? *in*))
	   (error "cdisc" "Cannot open file for input" *src*))))

;*---------------------------------------------------------------------*/
;*    stop-io! ...                                                     */
;*---------------------------------------------------------------------*/
(define (stop-io!)
   (if (and (string? *src*) (input-port? *in*))
       (close-input-port *in*)))
