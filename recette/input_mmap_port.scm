;*=====================================================================*/
;*    /bigloo/recette/input_mmap_port.scm                              */
;*    -------------------------------------------------------------    */
;*    Author      :  Joseph Donaldson                                  */
;*    Creation    :  Tue Jan 16 07:26:45 2024                          */
;*    Last change :  Tue Jan 16 09:00:00 2023 (Joseph Donaldson)       */
;*    Copyright   :  2024 Joseph Donaldson                             */
;*    -------------------------------------------------------------    */
;*    MMAP testing                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module input-mmap-port
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-input-mmap-port)))

;*---------------------------------------------------------------------*/
;*    test-mmap ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-input-mmap-port)
   (test-module "test-input-mmap-port" "input-mmap-port.scm")
   (let* ((path "misc/input.txt")
          (mm (open-mmap path :read #t))
          (mmap-input (open-input-mmap mm))
          (c (with-input-from-file path (lambda () (read-char))))
          (s (with-input-from-file path (lambda () (read-chars 20)
                                           (read-chars 9)))))
      (test "input-mmap-port?.1" (input-mmap-port? mmap-input) #t)
      (test "input-mmap-port?.2" (input-mmap-port? list) #f)
      (test "input-mmap-port?.3" (input-mmap-port? 1) #f)
      (test "input-mmap-port?.4" (input-mmap-port? "foo") #f)
      (test "input-mmap-port read-char" (read-char mmap-input) c)
      (input-port-reopen! mmap-input)
      (test "input-mmap-port input-port-reopen! then read-char" (read-char mmap-input) c)
      (set-input-port-position! mmap-input 20)
      (test "input-mmap-port set-input-port-position!" (read-chars 9 mmap-input) s)
      (test "input-mmap-port close-input-port" (close-input-port mmap-input) mmap-input)
      (close-mmap mm)))
