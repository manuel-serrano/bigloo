;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Constant instructions table. (section 3.4.13 of the spectification)

'((i32.const . #t)
  (i64.const . #t)
  (f32.const . #t)
  (f64.const . #t)

  (i32.add . #t)
  (i64.add . #t)
  (i32.sub . #t)
  (i64.sub . #t)
  (i32.mul . #t)
  (i64.mul . #t)

  (ref.null           . #t)
  (ref.i31            . #t)
  (ref.func           . #t)
  (struct.new         . #t)
  (struct.new_default . #t)
  (array.new          . #t)
  (array.new_default  . #t)
  (array.new_fixed    . #t)
  (any.convert_extern . #t)
  (extern.convert_any . #t))
