module C = Complex
open! Base
module Complex = C

module Rgb = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    }

  let to_byte f = Float.iround_down_exn @@ (f *. 255.0)

  let make ~r ~g ~b =
    let r = to_byte r
    and g = to_byte g
    and b = to_byte b in
    { r; g; b }
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
    Rgb.make ~r:(f 1.0) ~g:(f @@ (2.0 /. 3.0)) ~b:(f @@ (1.0 /. 3.0))
  ;;
end

let color ?(max_iter = 64) z c =
  let make_rgb hue i = Hsv.to_rgb @@ Hsv.make hue (i >= max_iter) in
  let rec loop i z hue =
    if i < max_iter
    then begin
      let q = Complex.quadrance z in
      if Float.O.(q > 4.0)
      then make_rgb hue i
      else (
        let z = Complex.Infix.(c + (z * z)) in
        let hue = hue +. Float.exp (-.q) in
        loop (i + 1) z hue)
    end
    else make_rgb hue i
  in
  loop 0 z 0.0
;;

let blit buf ~pitch ~c ~max_iter =
  let nrows = Bigarray.Array1.dim buf / pitch in
  let ncols = pitch / 3 in
  let pixel_to_z x y =
    let center i = 0.5 +. Float.of_int i in
    let q =
      let re = center x /. Float.of_int ncols in
      let im = center y /. Float.of_int nrows in
      Complex.create ~re ~im
    in
    Complex.scale Complex.Infix.(q - Complex.create ~re:0.5 ~im:0.5) 4.0
  in
  for y = 0 to nrows - 1 do
    let y_off = y * pitch in
    for x = 0 to ncols - 1 do
      let z = pixel_to_z x y in
      let { Rgb.r; g; b } = color z c ~max_iter in
      let offset = y_off + (x * 3) in
      buf.{offset + 0} <- r;
      buf.{offset + 1} <- g;
      buf.{offset + 2} <- b
    done
  done
;;
