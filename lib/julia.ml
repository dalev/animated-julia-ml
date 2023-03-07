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

let color ?(max_iter = 64) zr zi c =
  (* we manually unpack the components of z to avoid allocation in the loop *)
  let rec loop i zr zi hue c =
    let q = (Complex.norm2 [@inlined]) { re = zr; im = zi } in
    if i <= 0 || Float.O.(q > 4.0)
    then make_rgb hue i
    else begin
      let { Complex.re; im } = (Complex.sq [@inlined]) { re = zr; im = zi } in
      let zr = re +. c.Complex.re
      and zi = im +. c.Complex.im
      and hue = hue +. Float.exp (-.q) in
      loop (i - 1) zr zi hue c
    end
  in
  loop max_iter zr zi 0.0 c
;;

let pixel_to_complex ~width ~height x y =
  let rwidth = 1.0 /. Float.of_int width in
  let rheight = 1.0 /. Float.of_int height in
  let center i rlimit = 4.0 *. (((0.5 +. Float.of_int i) *. rlimit) -. 0.5) in
  let re = center x rwidth in
  let im = center y rheight in
  { Complex.re; im }
;;

let blit buf ~pool ~width ~c ~max_iter =
  let num_pixels = Bigarray.Array1.dim buf / 4 in
  let finish = num_pixels - 1 in
  let height = num_pixels / width in
  let rwidth = 1 // width in
  let rheight = 1 // height in
  let pixel_to_z x y =
    let center i rlimit = 4.0 *. (((0.5 +. Float.of_int i) *. rlimit) -. 0.5) in
    let re = center x rwidth in
    let im = center y rheight in
    { Complex.re; im }
  in
  let chunk_size = width in
  Task.run pool (fun () ->
    Task.parallel_for pool ~chunk_size ~start:0 ~finish ~body:(fun offset ->
      let x = offset % width
      and y = offset / width in
      let { Complex.re = zr; im = zi } = pixel_to_z x y in
      let rgba = color zr zi c ~max_iter in
      Bigstring.unsafe_set_uint32_le buf ~pos:(4 * offset) rgba))
;;
