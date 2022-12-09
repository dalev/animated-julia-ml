open! Base
module Complex = Float_complex
module Task = Domainslib.Task

module Rgba : sig
  type t = Int32.t

  val make : r:float -> g:float -> b:float -> t
end = struct
  type t = Int32.t

  let to_byte f = Float.to_int (f *. 255.0)

  let make ~r ~g ~b =
    let a = 1.0 in
    let rgba =
      (to_byte r lsl 24) + (to_byte g lsl 16) + (to_byte b lsl 8) + (to_byte a lsl 0)
    in
    Int.to_int32_trunc rgba
  ;;
end

module Hsv = struct
  type t =
    { h : float
    ; s : float
    ; v : float
    }

  let make (hue : float) escaped =
    let open Float.O in
    let h = 0.1 + (0.9 * hue) in
    let v = if escaped then 0.0 else 1.0 in
    { h; s = 1.0; v }
  ;;

  let clamp_unit = Float.clamp_exn ~min:0.0 ~max:1.0
  let fractional f = f -. Float.round_down f

  let to_rgb { h; s = _; v } =
    let open Float.O in
    let f a =
      let x = (6.0 * fractional (a + h)) - 3.0 in
      v * clamp_unit (Float.abs x - 1.0)
    in
    Rgba.make ~r:(f 1.0) ~g:(f @@ (2.0 /. 3.0)) ~b:(f @@ (1.0 /. 3.0))
  ;;
end

let color ?(max_iter = 64) z c =
  let make_rgb hue i = Hsv.to_rgb @@ Hsv.make hue (i >= max_iter) in
  let rec loop i z hue =
    if i < max_iter
    then begin
      let q = Complex.norm2 z in
      if Float.O.(q > 4.0)
      then make_rgb hue i
      else (
        let z = Complex.(add c (mul z z)) in
        let hue = hue +. Float.exp (-.q) in
        loop (i + 1) z hue)
    end
    else make_rgb hue i
  in
  loop 0 z 0.0
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
