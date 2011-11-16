;*=====================================================================*/
;*    .../prgm/project/bigloo/comptime/BackEnd/dotnet_class.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Nov 14 18:52:09 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
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
   (include "BackEnd/dotnet_class.sch")
   (export (class dotnet::bvm
	      (out::output-port (default (open-output-string)))
	      (qname::bstring (default "")))))
