open! Base
module Complex = Float_complex
module Task = Domainslib.Task
module Bigstring = Base_bigstring

module Rgba : sig
  type t = int

  val make : r:int -> g:int -> b:int -> t
end = struct
  type t = int

  let make ~r ~g ~b =
    let alpha = 0xff in
    (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor alpha
  ;;
end

let clamp f = Float.clamp_exn f ~min:0.0 ~max:1.0

let make_rgb hue i =
  (* hsv -> rgb conversion from wikipedia *)
  let open Float.O in
  let h = 0.1 + (0.9 * clamp hue) in
  let s = 1.0 in
  let v = Float.of_int (min i 1) in
  let to_byte f = Int.of_float_unchecked (f *. 255.0) in
  let f n =
    let k = Float.mod_float (n + (6.0 * h)) 6.0 in
    let x = v * (1.0 - (s * clamp (Float.min k (4.0 - k)))) in
    to_byte x
  in
  Rgba.make ~r:(f 5.0) ~g:(f 3.0) ~b:(f 1.0)
;;

let color ?(max_iter = 64) z c =
  let rec loop i z hue =
    let q = Complex.norm2 z in
    if i <= 0 || Float.O.(q > 4.0)
    then make_rgb hue i
    else begin
      let z = Complex.(c + (z * z))
      and hue = hue +. Float.exp (-.q) in
      loop (i - 1) z hue
    end
  in
  loop max_iter z 0.0
;;

let pixel_to_complex ~width ~height x y =
  let center i limit = (0.5 +. Float.of_int i) /. Float.of_int limit in
  let q =
    let re = center x width in
    let im = center y height in
    { Complex.re; im }
  in
  Complex.(of_real 4.0 * (q - { re = 0.5; im = 0.5 }))
;;

let blit buf ~pool ~width ~c ~max_iter =
  let finish = (Bigarray.Array1.dim buf / 4) - 1 in
  let height = Bigarray.Array1.dim buf / 4 / width in
  let pixel_to_z = pixel_to_complex ~width ~height in
  let chunk_size = width in
  Task.run pool (fun () ->
    Task.parallel_for pool ~chunk_size ~start:0 ~finish ~body:(fun offset ->
      let x = offset % width
      and y = offset / width in
      let z = pixel_to_z x y in
      let rgba = color z c ~max_iter in
      Bigstring.set_uint32_le_exn buf ~pos:(4 * offset) rgba))
;;
