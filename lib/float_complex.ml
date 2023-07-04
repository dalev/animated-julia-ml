open! Base
include Stdlib.Complex

let norm2 { re; im } = Stdlib.Float.fma re re (im *. im)
let of_real re = { re; im = 0.0 }
let sq { re; im } = { re = (re +. im) *. (re -. im); im = 2.0 *. re *. im }

let ( = ) z w =
  let open Float.O in
  z.re = w.re && z.im = w.im
;;

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
