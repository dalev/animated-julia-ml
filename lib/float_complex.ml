open! Base
include Caml.Complex

let ( = ) z w =
  let open Float.O in
  z.re = w.re && z.im = w.im
;;
