;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/examples/ctl.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:03:11 2011                          */
;*    Last change :  Tue Jul 12 10:09:40 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    alsa ctl demo.                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ctl
   (library pthread alsa)
   (main main))

;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define card "default")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((c (instantiate::alsa-snd-ctl
	       (card (if (pair? (cdr argv)) (cadr argv) card)))))
      (alsa-snd-ctl-open c)
      (let ((i (instantiate::alsa-snd-ctl-card-info
		  (ctl c))))
	 (with-access::alsa-snd-ctl-card-info i
	       ((num card) id driver name longname mixername components)
	    (print "Card " card " '" name "'/'" longname "' (#" num ")")
	    (print "  Id: " id)
	    (print "  Driver: " driver)
	    (print "  Mixer name: " mixername)
	    (print "  Components: " components)))
      (alsa-snd-ctl-close c)))
