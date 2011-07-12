;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/examples/mixer.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:03:11 2011                          */
;*    Last change :  Tue Jul 12 10:41:11 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    alsa mixer demo.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mixer
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
   (let ((m (instantiate::alsa-snd-mixer)))
      (alsa-snd-mixer-attach m (if (pair? (cdr argv)) (cadr argv) card))
      (alsa-snd-mixer-load m)
      (print "count: " (alsa-snd-mixer-get-count m))
      (alsa-snd-mixer-close m)))
