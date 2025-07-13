;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Table of instructions' side effects.
;;
;; Instructions not in the table are considered to have side effects to avoid
;; soundess issues due to oversights

'((i32.const . #f)
  (i64.const . #f)
  (f32.const . #f)
  (f64.const . #f)

  ; todo : rewrite these with symbol append to shorten them
  (i32.eqz  . #f)
  (i32.eq   . #f)
  (i32.ne   . #f)
  (i32.lt_s . #f)
  (i32.lt_u . #f)
  (i32.gt_s . #f)
  (i32.gt_u . #f)
  (i32.le_s . #f)
  (i32.le_u . #f)
  (i32.ge_s . #f)
  (i32.ge_u . #f)

  (i64.eqz  . #f)
  (i64.eq   . #f)
  (i64.ne   . #f)
  (i64.lt_s . #f)
  (i64.lt_u . #f)
  (i64.gt_s . #f)
  (i64.gt   . #f)
  (i64.le_s . #f)
  (i64.le_u . #f)
  (i64.ge_s . #f)
  (i64.ge_u . #f)

  (f32.eq . #f)
  (f32.ne . #f)
  (f32.lt . #f)
  (f32.gt . #f)
  (f32.le . #f)
  (f32.ge . #f)

  (f64.eq . #f)
  (f64.ne . #f)
  (f64.lt . #f)
  (f64.gt . #f)
  (f64.le . #f)
  (f64.ge . #f)

  ; divisions and remainders can trap
  (i32.clz    . #f)
  (i32.ctz    . #f)
  (i32.popcnt . #f)
  (i32.add    . #f)
  (i32.sub    . #f)
  (i32.mul    . #f)
  (i32.div_s  . #t)
  (i32.div_u  . #t)
  (i32.rem_s  . #t)
  (i32.rem_u  . #t)
  (i32.and    . #f)
  (i32.or     . #f)
  (i32.xor    . #f)
  (i32.shl    . #f)
  (i32.shr_s  . #f)
  (i32.shr_u  . #f)
  (i32.rotl   . #f)
  (i32.rotr   . #f)

  (i64.clz    . #f)
  (i64.ctz    . #f)
  (i64.popcnt . #f)
  (i64.add    . #f)
  (i64.sub    . #f)
  (i64.mul    . #f)
  (i64.div_s  . #t)
  (i64.div_u  . #t)
  (i64.rem_s  . #t)
  (i64.rem_u  . #t)
  (i64.and    . #f)
  (i64.or     . #f)
  (i64.xor    . #f)
  (i64.shl    . #f)
  (i64.shr_s  . #f)
  (i64.shr_u  . #f)
  (i64.rotl   . #f)
  (i64.rotr   . #f)

  (f32.abs      . #f)
  (f32.neg      . #f)
  (f32.ceil     . #f)
  (f32.floor    . #f)
  (f32.trunc    . #f)
  (f32.nearest  . #f)
  (f32.sqrt     . #f)
  (f32.add      . #f)
  (f32.sub      . #f)
  (f32.mul      . #f)
  (f32.div      . #f)
  (f32.min      . #f)
  (f32.max      . #f)
  (f32.copysign . #f)

  (f64.abs      . #f)
  (f64.neg      . #f)
  (f64.ceil     . #f)
  (f64.floor    . #f)
  (f64.trunc    . #f)
  (f64.nearest  . #f)
  (f64.sqrt     . #f)
  (f64.add      . #f)
  (f64.sub      . #f)
  (f64.mul      . #f)
  (f64.div      . #f)
  (f64.min      . #f)
  (f64.max      . #f)
  (f64.copysign . #f)

  ; truncs can trap
  (i32.wrap_i64        . #f)
  (i32.trunc_f64_s     . #t)
  (i32.trunc_f64_u     . #t)
  (i64.extend_i32_s    . #f)
  (i64.extend_i32_u    . #f)
  (i64.trunc_f64_s     . #t)
  (i64.trunc_f64_u     . #t)
  (f64.convert_i32_s   . #f)
  (f64.convert_i32_u   . #f)
  (f64.convert_i64_s   . #f)
  (f64.convert_i64_u   . #f)
  (f32.demote_f64      . #f)
  (f64.convert_i64_s   . #f)
  (f64.promote_f32     . #f)
  (i32.reinterpret_f32 . #f)
  (i64.reinterpret_f64 . #f)
  (f32.reinterpret_i32 . #f)
  (f64.reinterpret_i64 . #f)

  (ref.null        . #f)
  (ref.func        . #f)
  (ref.is_null     . #f)
  (ref.as_non_null . #t) ; can trap
  (ref.eq          . #f)
  (ref.test        . #f)
  (ref.cast        . #t) ; can trap

  ; actually most of these instructions trap when one of their arguments is a
  ; null reference, we can assert they have no side effect when their arguments
  ; are known to be non-null
  (struct.new         . #f)
  (struct.new_default . #f)
  (struct.get         . #t) ; can trap
  (struct.get_u       . #t) ; can trap
  (struct.get_s       . #t) ; can trap
  (struct.set         . #t)

  (array.new         . #f)
  (array.new_default . #f)
  (array.new_fixed   . #f)
  (array.new_data    . #t) ; can trap
  (array.get         . #t) ; can trap
  (array.get_u       . #t) ; can trap
  (array.get_s       . #t) ; can trap
  (array.set         . #t)
  (array.len         . #t) ; can trap
  (array.copy        . #t) ; can trap
  (array.init_data   . #t) ; can trap

  (ref.i31   . #f)
  (i31.get_u . #t) ; can trap
  (i31.get_s . #t) ; can trap

  (drop . #f)

  (local.get  . #f)
  (local.set  . #t)
  (local.tee  . #t)
  (global.get . #f)
  (global.set . #t)

  (nop . #f))
