open! Base

type t =
  { re : float
  ; im : float
  }

let create ~re ~im = { re; im }

let polar ~radius ~angle =
  let re = radius *. Float.cos angle
  and im = radius *. Float.sin angle in
  { re; im }
;;

let dot z z' = (z.re *. z'.re) +. (z.im *. z'.im)
let quadrance z = dot z z
let scale { re; im } n = { re = re *. n; im = im *. n }
let zero = { re = 0.0; im = 0.0 }

module Infix = struct
  let ( + ) z w =
    let re = z.re +. w.re
    and im = z.im +. w.im in
    { re; im }
  ;;

  let ( - ) z w =
    let re = z.re -. w.re
    and im = z.im -. w.im in
    { re; im }
  ;;

  let ( * ) { re = a; im = b } { re = c; im = d } =
    let open Float.O in
    let re = (a * c) - (b * d)
    and im = (a * d) + (b * c) in
    { re; im }
  ;;
end