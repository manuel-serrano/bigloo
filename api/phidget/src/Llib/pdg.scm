;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phidget/src/Llib/pdg.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 28 13:18:16 2012                          */
;*    Last change :  Wed Nov 28 13:18:19 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget ctypes                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_ctypes

   (include "pdg.sch")

   (export
      (symbol->servo-type::$pdg-servo-type sym::symbol)
      (servo-type->symbol::symbol type::$pdg-servo-type)
      (pdgbool->bool::bool i::int)))

;*---------------------------------------------------------------------*/
;*    symbol->servo-type ...                                           */
;*---------------------------------------------------------------------*/
(define (symbol->servo-type sym)
   (case sym
      ((default)
	 $pdg-phidget-servo-default)
      ((raw-us-mode)
	 $pdg-phidget-servo-raw-us-mode)
      ((hitec-hs322hd)
	 $pdg-phidget-servo-hitec-hs322hd)
      ((hitec-hs5245mg)
	 $pdg-phidget-servo-hitec-hs5245mg)
      ((hitec-805bb)
	 $pdg-phidget-servo-hitec-805bb)
      ((hitec-hs422)
	 $pdg-phidget-servo-hitec-hs422)
      ((towerpro-mg90)
	 $pdg-phidget-servo-towerpro-mg90)
      ((hitec-hsr1425cr)
	 $pdg-phidget-servo-hitec-hsr1425cr)
      ((hitec-hs785hb)
	 $pdg-phidget-servo-hitec-hs785hb)
      ((hitec-hs485hb)
	 $pdg-phidget-servo-hitec-hs485hb)
      ((hitec-hs645mg)
	 $pdg-phidget-servo-hitec-hs645mg)
      ((hitec-815bb)
	 $pdg-phidget-servo-hitec-815bb)
      ((firgelli-l12-30-50-06-r)
	 $pdg-phidget-servo-firgelli-l12-30-50-06-r)
      ((firgelli-l12-50-100-06-r)
	 $pdg-phidget-servo-firgelli-l12-50-100-06-r)
      ((firgelli-l12-50-210-06-r)
	 $pdg-phidget-servo-firgelli-l12-50-210-06-r)
      ((firgelli-l12-100-50-06-r)
	 $pdg-phidget-servo-firgelli-l12-100-50-06-r)
      ((firgelli-l12-100-100-06-r)
	 $pdg-phidget-servo-firgelli-l12-100-100-06-r)
      ((springrc-sm-s2313m)
	 $pdg-phidget-servo-springrc-sm-s2313m)
      ((springrc-sm-s3317m)
	 $pdg-phidget-servo-springrc-sm-s3317m)
      ((springrc-sm-s3317sr)
	 $pdg-phidget-servo-springrc-sm-s3317sr)
      ((springrc-sm-s4303r)
	 $pdg-phidget-servo-springrc-sm-s4303r)
      ((springrc-sm-s4315m)
	 $pdg-phidget-servo-springrc-sm-s4315m)
      ((springrc-sm-s4315r)
	 $pdg-phidget-servo-springrc-sm-s4315r)
      ((springrc-sm-s4505b)
	 $pdg-phidget-servo-springrc-sm-s4505b)
      (else
       (error "symbol->servo-type"
	  "Unknown servo type" sym))))

;*---------------------------------------------------------------------*/
;*    servo-type->symbol ...                                           */
;*---------------------------------------------------------------------*/
(define (servo-type->symbol::symbol type::$pdg-servo-type)
   (cond
      ((=fx type $pdg-phidget-servo-default)
	 'default)
      ((=fx type $pdg-phidget-servo-raw-us-mode)
	 'raw-us-mode)
      ((=fx type $pdg-phidget-servo-hitec-hs322hd)
	 'hitec-hs322hd)
      ((=fx type $pdg-phidget-servo-hitec-hs5245mg)
	 'hitec-hs5245mg)
      ((=fx type $pdg-phidget-servo-hitec-805bb)
	 'hitec-805bb)
      ((=fx type $pdg-phidget-servo-hitec-hs422)
	 'hitec-hs422)
      ((=fx type $pdg-phidget-servo-towerpro-mg90)
	 'towerpro-mg90)
      ((=fx type $pdg-phidget-servo-hitec-hsr1425cr)
	 'hitec-hsr1425cr)
      ((=fx type $pdg-phidget-servo-hitec-hs785hb)
	 'hitec-hs785hb)
      ((=fx type $pdg-phidget-servo-hitec-hs485hb)
	 'hitec-hs485hb)
      ((=fx type $pdg-phidget-servo-hitec-hs645mg)
	 'hitec-hs645mg)
      ((=fx type $pdg-phidget-servo-hitec-815bb)
	 'hitec-815bb)
      ((=fx type $pdg-phidget-servo-firgelli-l12-30-50-06-r)
	 'firgelli-l12-30-50-06-r)
      ((=fx type $pdg-phidget-servo-firgelli-l12-50-100-06-r)
	 'firgelli-l12-50-100-06-r)
      ((=fx type $pdg-phidget-servo-firgelli-l12-50-210-06-r)
	 'firgelli-l12-50-210-06-r)
      ((=fx type $pdg-phidget-servo-firgelli-l12-100-50-06-r)
	 'firgelli-l12-100-50-06-r)
      ((=fx type $pdg-phidget-servo-firgelli-l12-100-100-06-r)
	 'firgelli-l12-100-100-06-r)
      ((=fx type $pdg-phidget-servo-springrc-sm-s2313m)
	 'springrc-sm-s2313m)
      ((=fx type $pdg-phidget-servo-springrc-sm-s3317m)
	 'springrc-sm-s3317m)
      ((=fx type $pdg-phidget-servo-springrc-sm-s3317sr)
	 'springrc-sm-s3317sr)
      ((=fx type $pdg-phidget-servo-springrc-sm-s4303r)
	 'springrc-sm-s4303r)
      ((=fx type $pdg-phidget-servo-springrc-sm-s4315m)
	 'springrc-sm-s4315m)
      ((=fx type $pdg-phidget-servo-springrc-sm-s4315r)
	 'springrc-sm-s4315r)
      ((=fx type $pdg-phidget-servo-springrc-sm-s4505b)
	 'springrc-sm-s4505b)
      (else
       (error "servo-type->symbol"
	  "Unknown servo type" type))))

;*---------------------------------------------------------------------*/
;*    pdgbool->bool ...                                                */
;*---------------------------------------------------------------------*/
(define (pdgbool->bool::bool i::int)
   (=fx i $pdg-true))
