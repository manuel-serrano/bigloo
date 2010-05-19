;*=====================================================================*/
;*    .../prgm/project/bigloo/comptime/BackEnd/dotnet-class.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Feb 28 13:17:58 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The C backend class definition                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_dotnet_class
   (import type_type
	   backend_backend
	   backend_bvm)
   (export (class dotnet::bvm
	      (out::output-port (default (open-output-string)))
	      (qname::bstring (default "")))))
