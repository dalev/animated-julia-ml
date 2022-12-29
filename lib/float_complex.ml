open! Base
include Caml.Complex

let norm2 { re; im } = Caml.Float.fma re re (im *. im)
let of_real re = { re; im = 0.0 }

let ( = ) z w =
  let open Float.O in
  z.re = w.re && z.im = w.im
;;

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
