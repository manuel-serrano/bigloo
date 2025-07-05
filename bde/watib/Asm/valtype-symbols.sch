;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Binary representation of types. (section 5.3.1, 5.3.2, 5.3.3 and 5.3.8 of the
;; spectification)
;;
;; We are sloppier than earlier because the types we output have already been
;; validated. We, thus, do not need do distinguish number types, from vector
;; types, etc.

'((i32 . #x7F)
  (i64 . #x7E)
  (f32 . #x7D)
  (f64 . #x7C)
  (v128 . #x7B)
  (i8 . #x78)
  (i16 . #x77)
  (nofunc . #x73)
  (noextern . #x72)
  (none . #x71)
  (func . #x70)
  (exn . #x69)
  (extern . #x6F)
  (any . #x6E)
  (eq . #x6D)
  (i31 . #x6C)
  (struct . #x6B)
  (array . #x6A))
