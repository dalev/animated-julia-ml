open! Base

type t = int

let make ~r ~g ~b ~a = (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor a
let of_int i = i
let to_int t = t
let red t = (t lsr 24) land 0xFF
let green t = (t lsr 16) land 0xFF
let blue t = (t lsr 8) land 0xFF
let alpha t = (t lsr 0) land 0xFF

let lerp t lo hi =
  let clamp b = Int.clamp_exn b ~min:0 ~max:255 in
  let f c =
    let a = Float.of_int (c lo)
    and b = Float.of_int (c hi) in
    let combo = ((1.0 -. t) *. a) +. (t *. b) in
    clamp (Float.to_int combo)
  in
  make ~r:(f red) ~g:(f green) ~b:(f blue) ~a:(f alpha)
;;
