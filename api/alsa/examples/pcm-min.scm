;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/examples/pcm-min.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:03:11 2011                          */
;*    Last change :  Tue Jun 28 08:23:44 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    pcm-min, an extra small demo sends a random samples to your      */
;*    speakers.                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module pcm-min
   (library pthread alsa)
   (main main))

;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define device "default")
(define buffer (make-string (*fx 16 1024)))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for range . body)
   (let ((for (gensym 'for))
	 (stop (gensym 'stop)))
      `(let ((,stop ,(caddr range)))
	  (let ,for ((,(car range) ,(cadr range)))
	       (when (<fx ,(car range) ,stop)
		  ,@body
		  (,for (+fx ,(car range) 1)))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   
   ;; randomly fill the random buffer
   (for (i 0 (string-length buffer))
      (string-set! buffer i (integer->char (random 255))))
   
   ;; open the pcm device
   (let ((pcm (instantiate::alsa-snd-pcm
		 (device device))))

      (print "device=" (alsa-snd-pcm-name pcm))
      
      (alsa-snd-pcm-set-params! pcm :format 'u8
	 :access 'rw-interleaved
	 :channels 1
	 :rate 48000
	 :soft-resample 1
	 :latency 500000)

      ;; play the sound
      (for (i 0 16)
	 (alsa-snd-pcm-writei pcm buffer (string-length buffer)))
      
      (alsa-snd-pcm-close pcm)))
