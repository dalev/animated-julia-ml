open! Base

type t = int -> float -> Rgba.t

let make ~first ~last ~stops ~size =
  let stops = ref (stops @ [ 1.0, last ]) in
  let c0 = ref first
  and c1 = ref (snd @@ List.hd_exn !stops)
  and a = ref 0.0
  and b = ref (fst @@ List.hd_exn !stops) in
  stops := List.tl_exn !stops;
  let rec find_color p =
    if Float.(p <= !b)
    then begin
      let open Float.O in
      let t = (p - !a) / (!b - !a) in
      Rgba.lerp t !c0 !c1
    end
    else begin
      let b', c1' = List.hd_exn !stops in
      c0 := !c1;
      a := !b;
      c1 := c1';
      b := b';
      stops := List.tl_exn !stops;
      find_color p
    end
  in
  let colors =
    Array.init (size + 1) ~f:(fun i ->
      let p = i // size in
      find_color p)
  in
  fun i di ->
    if Float.is_nan di then colors.(i) else Rgba.lerp di colors.(i - 1) colors.(i)
;;

let find t i di = t i di
