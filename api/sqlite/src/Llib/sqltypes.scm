;*=====================================================================*/
;*    .../project/bigloo/5.0.x/api/sqlite/src/Llib/sqltypes.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 10 09:09:05 2026                          */
;*    Last change :  Thu Jul  9 10:35:06 2026 (serrano)                */
;*    Copyright   :  2026 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Sqlitiny classes                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_types
   
   (export (class $sqltiny
	      ($version::bstring (default ($sqltiny-version)))
	      path::bstring
	      (sync::symbol read-only (default 'automatic))
	      (tables::pair-nil (default '()))
 	      (mutex::mutex (default (make-mutex)))
	      (transaction::bool (default #f))
	      (last-insert-rowid::long (default 0)))
	   (class $sqltiny-table
	      (name::bstring read-only)
	      (mutex::mutex (default (make-mutex)))
	      (rowid::long (default 0))
	      (removable::bool read-only (default #t))
	      (columns::pair-nil (default '()))
	      (*columns::pair-nil (default '()))
	      (rows::pair-nil (default '()))
	      (constraints::pair-nil (default '()))
	      (keycheck::procedure (default (lambda (obj r rs replacep) #t)))
	      (last-row-pair::pair-nil (default '())))
	   (class $sqltiny-column
	      (name::bstring read-only)
	      (type::symbol read-only (default 'OBJ))
	      (index::int (default -1))
	      (primkey::bool read-only (default #f))
	      (default::obj read-only (default #unspecified)))
	   ($sqltiny-version::bstring)))

;*---------------------------------------------------------------------*/
;*    $sqltiny-version ...                                             */
;*---------------------------------------------------------------------*/
(define ($sqltiny-version)
   "1.0.1")

