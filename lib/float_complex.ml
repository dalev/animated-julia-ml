open! Base
include Caml.Complex

let of_real re = { re; im = 0.0 }

let ( = ) z w =
  let open Float.O in
  z.re = w.re && z.im = w.im
;;

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
