open! Base
module Complex = Float_complex
module Task = Domainslib.Task

module Rgba : sig
  type t = Int32.t

  val make : r:int -> g:int -> b:int -> t
end = struct
  type t = Int32.t

  let make ~r ~g ~b =
    let a = 255 in
    let rgba = (r lsl 24) + (g lsl 16) + (b lsl 8) + (a lsl 0) in
    Int.to_int32_trunc rgba
  ;;
end

let clamp_unit = Float.clamp_exn ~min:0.0 ~max:1.0
let fractional f = f -. Float.round_down f

let color ?(max_iter = 64) z c =
  let make_rgb hue i =
    (* hsv -> rgb conversion from wikipedia *)
    let open Float.O in
    let h = 0.1 + (0.9 * hue) in
    let s = 1.0 in
    let v = Float.of_int (min i 1) in
    let to_byte f = Float.to_int (f *. 255.0) in
    let f n =
      let k = Float.mod_float (n + (6.0 * h)) 6.0 in
      let x = v * (1.0 - (s * Float.max 0.0 (Float.min (Float.min k (4.0 - k)) 1.0))) in
      to_byte x
    in
    Rgba.make ~r:(f 5.0) ~g:(f 3.0) ~b:(f 1.0)
  in
  let rec loop i z hue =
    if i <= 0
    then make_rgb hue i
    else begin
      let q = Complex.norm2 z in
      if Float.O.(q > 4.0)
      then make_rgb hue i
      else (
        let z = Complex.(add c (mul z z)) in
        let hue = hue +. Float.exp (-.q) in
        loop (i - 1) z hue)
    end
  in
  loop max_iter z 0.0
;;

let pixel_to_complex ~width ~height x y =
  let center i = 0.5 +. Float.of_int i in
  let q =
    let re = center x /. Float.of_int width in
    let im = center y /. Float.of_int height in
    { Complex.re; im }
  in
  Complex.(mul (sub q { re = 0.5; im = 0.5 }) { re = 4.0; im = 0.0 })
;;

let blit buf ~pool ~pitch ~c ~max_iter =
  let finish = Bigarray.Array1.dim buf - 1 in
  let nrows = Bigarray.Array1.dim buf / pitch in
  let ncols = pitch in
  let pixel_to_z x y = pixel_to_complex ~width:ncols ~height:nrows x y in
  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish ~body:(fun offset ->
      let x = offset % ncols
      and y = offset / ncols in
      let z = pixel_to_z x y in
      let rgba = color z c ~max_iter in
      buf.{offset} <- rgba))
;;
