;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgencoder.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Ludovic Courtès                                   */
;*    Creation    :  Tue Mar 27 09:11:24 2012                          */
;*    Last change :  Mon Sep  2 11:15:36 CEST 2013                     */
;*    Copyright   :  2013 Inria                                        */
;*    -------------------------------------------------------------    */
;*    Phidget Encoders                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_encoder

   (include "pdg.sch")

   (extern (export $make-encoder "bgl_encoder_new")
	   (macro $pdg-encoder-builtin&::$pdg-encoder*
	      (::phidget-encoder)
	      "BGL_PHIDGET_ENCODER_BUILTIN"))

   (import __phidget_types
	   __phidget_ctypes
	   __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-encoder::phidget
	       (encoder-count::int
		  read-only
		  (get (lambda (o)
                          (with-handler
                             (lambda (_) 0)
                             (with-access::phidget o ($builtin)
                                ($pdg-phidget-encoder-get-encoder-count
                                   ($pdg-phidget->encoder $builtin) o)))))))

            (phidget-encoder-input-count::int ::phidget-encoder)
            (phidget-encoder-input-enabled?::bool ::phidget-encoder ::int)

            (phidget-encoder-position::int ::phidget-encoder ::int)
            (phidget-encoder-position-set! ::phidget-encoder ::int ::int)
            (phidget-encoder-index-position::int ::phidget-encoder ::int)

            (phidget-encoder-enabled?::bool ::phidget-encoder ::int)
            (phidget-encoder-enable! ::phidget-encoder ::int)
            (phidget-encoder-disable! ::phidget-encoder ::int)

            ($make-encoder::obj ::$pdg-encoder)))

;*---------------------------------------------------------------------*/
;*    $make-encoder ...                                         */
;*---------------------------------------------------------------------*/
(define ($make-encoder mc::$pdg-encoder)
   (instantiate::phidget-encoder
      ($builtin ($pdg-encoder->phidget mc))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-encoder)
   (phidget-init!)
   (phidget-return
      ($pdg-encoder-create ($pdg-encoder-builtin& o))
      "phidget-encoder" o))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-encoder ...          */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-encoder event proc)
   (with-access::phidget-encoder o ($builtin)
      (phidget-return
       ($pdg-phidget-encoder-add-event-listener!
	($pdg-phidget->encoder $builtin) event o proc)
       "phidget-add-event-listener!" o)))


(define-syntax define-getter
   (syntax-rules ()
      ((_ name dollar-name)
       (define (name o i)
          (with-access::phidget o ($builtin)
             (dollar-name ($pdg-phidget->encoder $builtin)
                i o))))))

(define-syntax define-setter
   (syntax-rules ()
      ((_ name dollar-name)
       (define (name o i v)
          (with-access::phidget o ($builtin)
             (dollar-name ($pdg-phidget->encoder $builtin)
                i v))))))

(define-getter phidget-encoder-input-enabled?
   $pdg-phidget-encoder-get-input-enabled?)
(define-getter phidget-encoder-position
   $pdg-phidget-encoder-get-position)
(define-setter phidget-encoder-position-set!
   $pdg-phidget-encoder-set-position!)
(define-getter phidget-encoder-index-position
   $pdg-phidget-encoder-get-index-position)
(define-getter phidget-encoder-enabled?
   $pdg-phidget-encoder-get-enabled?)

(define (phidget-encoder-enable! e::phidget-encoder i::int)
   (with-access::phidget e ($builtin)
      ($pdg-phidget-encoder-enable! ($pdg-phidget->encoder $builtin) i))
   #t)

(define (phidget-encoder-disable! e::phidget-encoder i::int)
   (with-access::phidget e ($builtin)
      ($pdg-phidget-encoder-disable! ($pdg-phidget->encoder $builtin) i))
   #t)

;*---------------------------------------------------------------------*/
;*    phidget-encoder-input-count ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-encoder-input-count o)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-encoder-get-input-count
	 ($pdg-phidget->encoder $builtin) o)))
