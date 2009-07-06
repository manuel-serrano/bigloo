;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Engine/param.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 07:15:39 1999                          */
;*    Last change :  Thu Mar 17 08:39:15 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Configuration variables                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_param
   (import  tools_date)
   (export  ;; version and author
            *bdb-version*
	    *bdb-name*
	    *bdb-level*
	    *bdb-author*
	    *bdb-email*
	    *bdb-date*
	    *bdb-gui-host?*
	    ;; rc file
	    *bdb-rc*
	    *bdb-rc-variables*
	    ;; gdb settings
	    *gdb*
	    *gdb-options*
	    *gdb-prompt*
	    ;; arguments parsing
	    *exec*
	    *args*
	    *root-directory*
	    *afile*
	    *etags*
	    *src-suffix*
	    ;; misc
	    *verbose*
	    *log-name*
	    *active-prompt*
	    *bigloo-type*
	    *bigloo-classes*
	    *bigloo-failure*
	    *pre-command-hook*
	    *post-command-hook*
	    *exit*
	    *gdb-run-hook*
	    *gdb-call-hook*
	    *gdb-busy-hook*
	    ;; programming environment
	    *bee-client?*
	    *emacs-client?*
	    *heap-explorer?*
	    *base-address?*
	    *force-tty?*
	    *tty-init-hook*
	    ;; pretty printing
	    *write-circle?*
	    ;; input and output
	    *prompt*
	    *pin*
	    *pout*
	    *perr*
	    *bout*
	    *bin*
	    ;; bdb modes
	    *bdb-mode*
	    *bdb-scheme-mode-enabled?*
	    *c-std-unmangled*
	    ;; footprint mark
	    *footprint-mark*
	    ;; possible output fontification
	    *bdb-fontification?*)
   (eval    (export *bdb-fontification?*)
	    (export *write-circle?*)
	    (export *bdb-mode*)))

;*---------------------------------------------------------------------*/
;*    Version and authoring ...                                        */
;*---------------------------------------------------------------------*/
(define *bdb-version*       "1.5")
(define *bdb-level*         #\b)
(define *bdb-name*          (string-append "Bdb (v" *bdb-version*")"))
(define *bdb-author*        "Manuel Serrano")
(define *bdb-email*         "Manuel.Serrano@sophia.inria.fr")
(define *bdb-date*          (bdb-date))
(define *bdb-gui-host?*     #f)

;*---------------------------------------------------------------------*/
;*    rc file                                                          */
;*---------------------------------------------------------------------*/
(define *bdb-rc*            ".bdbrc")
(define *bdb-rc-variables*  '())

;*---------------------------------------------------------------------*/
;*    Gdb configuration                                                */
;*---------------------------------------------------------------------*/
(define *gdb*               "gdb")
(define *gdb-options*       '("--quiet" "--fullname" "--nx" "--annotate=2"))
(define *gdb-prompt*        "(gdb) ")

;*---------------------------------------------------------------------*/
;*    Arguments parsing ...                                            */
;*---------------------------------------------------------------------*/
(define *exec*              #f)
(define *args*              '())
(define *root-directory*    ".")
(define *afile*             ".afile")
(define *etags*             ".etags")
(define *src-suffix*        '("scm" "bgl"))

;*---------------------------------------------------------------------*/
;*    Misc                                                             */
;*---------------------------------------------------------------------*/
(define *verbose*           0)
(define *log-name*          "/tmp/bdb.log")
(define *active-prompt*     #t)
(define *bigloo-type*       '("union scmobj *" "obj_t"))
(define *bigloo-classes*    '())
(define *bigloo-failure*    "bigloo_abort")
(define *post-command-hook* '())
(define *pre-command-hook*  '())
(define *exit*              exit)
(define *gdb-run-hook*      '())
(define *gdb-call-hook*     '())
(define *gdb-busy-hook*     '())

;*---------------------------------------------------------------------*/
;*    Programming environment                                          */
;*---------------------------------------------------------------------*/
(define *bee-client?*       #f)
(define *emacs-client?*     #f)
(define *heap-explorer?*    #t)
(define *force-tty?*        #f)
(define *tty-init-hook*     '())
(define *base-address?*     #f)

;*---------------------------------------------------------------------*/
;*    Pretty printing                                                  */
;*---------------------------------------------------------------------*/
(define *write-circle?*     #t)

;*---------------------------------------------------------------------*/
;*    Input and output                                                 */
;*---------------------------------------------------------------------*/
(define *prompt*            "(bdb) ")
(define *pin*               (current-input-port))
(define *pout*              (current-output-port))
(define *perr*              (current-error-port))
(define *bout*              (current-output-port))
(define *bin*               (current-input-port))

;*---------------------------------------------------------------------*/
;*    Bdb mode                                                         */
;*---------------------------------------------------------------------*/
(define *bdb-mode*                 'mixte)
(define *bdb-scheme-mode-enabled?* #t)

;*---------------------------------------------------------------------*/
;*    *c-std-unmangled* ...                                            */
;*---------------------------------------------------------------------*/
(define *c-std-unmangled* '("main" "_main" "__main"))

;*---------------------------------------------------------------------*/
;*    *footprint-mark* ...                                             */
;*---------------------------------------------------------------------*/
(define *footprint-mark* "footprint")

;*---------------------------------------------------------------------*/
;*    *bdb-fontification?*                                             */
;*---------------------------------------------------------------------*/
(define *bdb-fontification?* #f)
