open Base

type t =
  | Animate
  | Follow_mouse

let term =
  let open Cmdliner in
  let doc = "Animate automatically or follow the mouse pointer" in
  let choices = [ "animate", Animate; "follow-mouse", Follow_mouse ] in
  let docv = String.concat ~sep:"|" (List.map ~f:fst choices) in
  Arg.(value & opt (enum choices) Animate & info [ "mode" ] ~docv ~doc)
;;
