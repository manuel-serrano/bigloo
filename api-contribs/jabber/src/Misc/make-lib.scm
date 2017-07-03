;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __jabber_makelib

	(option (set! *dlopen-init* "jabber_e"))
	
	(import bigloojabber)
	
	(eval (export-all)))

