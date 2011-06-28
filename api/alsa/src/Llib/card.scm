;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/card.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 27 19:17:51 2011                          */
;*    Last change :  Tue Jun 28 07:23:11 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Alsa card wrapper                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_card

   (include "alsa.sch")

   (import __alsa_alsa)
   
   (export (class alsa-card::alsa-object
	      (card::int read-only)
	      (name::bstring read-only)
	      (longname::bstring read-only))

	   (alsa-snd-cards-list::pair-nil)))

;*---------------------------------------------------------------------*/
;*    alsa-snd-cards-list ...                                          */
;*---------------------------------------------------------------------*/
(define (alsa-snd-cards-list)
   (let loop ((i 0))
      (if ($snd-card-load i)
	  (cons (instantiate::alsa-card
		   (card i)
		   (name ($bgl-snd-card-get-name i))
		   (longname ($bgl-snd-card-get-longname i)))
	     (loop (+fx i 1)))
	  '())))
