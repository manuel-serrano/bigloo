;*=====================================================================*/
;*    .../project/bigloo/5.0.x/api/multimedia/src/Llib/jpeg.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Aug 25 22:47:31 2026                          */
;*    Last change :  Tue Aug 25 23:01:44 2026 (serrano)                */
;*    Copyright   :  2026 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Jpeg tools                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    jpeg                                                             */
;*---------------------------------------------------------------------*/
(module __multimedia-jpeg
   (export (jpeg-parse-dimensions ::input-port)
           (jpeg-dimensions ::bstring)))

;*---------------------------------------------------------------------*/
;*    jpeg-parse-dimensions ...                                        */
;*---------------------------------------------------------------------*/
(define (jpeg-parse-dimensions ip::input-port)
   
   (define (read-u8 ip::input-port)
      (read-byte ip))
   
   (define (read-u16-be ip::input-port)
      (+fx (bit-lsh (read-u8 ip) 8)
         (read-u8 ip)))
   
   (define (sof-marker?::bool marker::long)
      (when (and (>=fx marker #xc0) (<=fx marker #xcf))
         (and (not (=fx marker #xc4)) (not (=fx marker #xc8)))))

   (if (and (=fx (read-u8 ip) #xff) (=fx (read-u8 ip) #xd8))
       (let loop ()
          (let ((byte (read-u8 ip)))
             (cond
                ((eof-object? byte)
                 (error "jpeg" "JPEG has no SOF marker"
                    (input-port-name ip)))
                ;; Skip bytes until FF.
                ((not (=fx byte #xff))
                 (loop))
                (else
                 ;; Skip fill FF bytes.
                 (let skip-ff ((marker (read-u8 ip)))
                    (cond
                       ((eof-object? marker)
                        (error "jpeg" "Unexpected end of JPEG"
                           (input-port-name ip)))
                       ((=fx marker #xff)
                        (skip-ff (read-u8 ip)))
                       ;; Standalone markers have no length.
                       ((or (=fx marker #xd8)   ;; SOI
                            (=fx marker #xd9)   ;; EOI
                            (and (>=fx marker #xd0)
                                 (<=fx marker #xd7))) ;; RST0-RST7
                        (loop))
                       ((sof-marker? marker)
                        ;; Segment length, precision, height, width...
                        (read-u16-be ip) ;; length
                        (read-u8 ip) ;; bits per sample
                        (let ((height (read-u16-be ip))
                              (width (read-u16-be ip)))
                           (values width height)))
                       (else
                        ;; Skip this marker's segment.
                        (let ((length (read-u16-be ip)))
                           (when (<fx length 2)
                              (error "jpeg"
                                 "Invalid JPEG marker length"
                                 (input-port-name ip)))
                           (let skip ((n (-fx length 2)))
                              (if (>fx n 0)
                                  (begin
                                     (read-u8 ip)
                                     (skip (-fx n 1)))
                                  (loop)))))))))))
       (error "jpeg" "Not a JPEG file" (input-port-name ip))))

;*---------------------------------------------------------------------*/
;*    jpeg-dimensions ...                                              */
;*---------------------------------------------------------------------*/
(define (jpeg-dimensions path::bstring)
   (call-with-input-file path jpeg-parse-dimensions))
