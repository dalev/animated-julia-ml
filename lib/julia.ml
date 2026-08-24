open! Base
module Complex = Float_complex
module Task = Domainslib.Task
module Bigstring = Base_bigstring

let log2 x = Float.log2 x

let color ?(max_iter = 64) ~radius ~palette z c =
  let r2 = radius *. radius in
  let logB x = Float.log x /. Float.log radius in
  (* This loop is ugly, but it needs to be this way to avoid allocating a ton of boxed float values *)
  let[@inline] mk_q re im = (Complex.norm2 [@inlined]) { re; im } in
  let i = ref 0
  and zr = ref z.Complex.re
  and zi = ref z.Complex.im in
  let q = ref @@ mk_q !zr !zi in
  while !i < max_iter && Float.O.(!q <= r2) do
    let { Complex.re; im } = (Complex.sq [@inlined]) { re = !zr; im = !zi } in
    zr := re +. c.Complex.re;
    zi := im +. c.Complex.im;
    q := mk_q !zr !zi;
    i := !i + 1
  done;
  let di = 1.0 -. log2 (logB (Float.sqrt !q)) in
  Palette.find palette !i di
;;

let center i rlimit = 4.0 *. (((0.5 +. Float.of_int i) *. rlimit) -. 0.5)

let pixel_to_complex ~width ~height x y =
  let re = center x (1 // width)
  and im = center y (1 // height) in
  { Complex.re; im }
;;

let blit buf ~pool ~width ~c ~max_iter ~radius ~palette =
  let num_pixels = Bigarray.Array1.dim buf / 4 in
  let finish = num_pixels - 1 in
  let height = num_pixels / width in
  let rwidth = 1 // width in
  let rheight = 1 // height in
  let pixel_to_z x y =
    let re = center x rwidth in
    let im = center y rheight in
    { Complex.re; im }
  in
  let chunk_size = width in
  Task.run pool (fun () ->
    Task.parallel_for pool ~chunk_size ~start:0 ~finish ~body:(fun offset ->
      let x = offset % width
      and y = offset / width
      and pos = 4 * offset in
      let rgba = color (pixel_to_z x y) c ~max_iter ~radius ~palette in
      Bigstring.unsafe_set_uint32_le buf ~pos (Rgba.to_int rgba)))
;;
