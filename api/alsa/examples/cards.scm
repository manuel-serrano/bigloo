;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/examples/cards.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 27 19:39:20 2011                          */
;*    Last change :  Tue Jun 28 07:46:08 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Introspecting cards                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module pcm-min
   (library pthread alsa)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (print "alsa-version: " (alsa-snd-version))
   (print "pcm:")
   (for-each (lambda (s) (print "  " s)) (alsa-snd-devices-list "pcm"))
   (print "cards:")
   (for-each (lambda (s) (print "  " s))  (alsa-snd-cards-list)))
  
