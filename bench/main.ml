open! Base
open Core_bench
module J = Animated_julia.Julia

let max_iter = 64

let blit_dimension ~pool ~c (width, height) =
  let name = Printf.sprintf "(%d, %d)" width height in
  let buf =
    Stdlib.Bigarray.Array1.create
      Stdlib.Bigarray.char
      Stdlib.Bigarray.c_layout
      (width * height * 4)
  in
  Bench.Test.create ~name (fun () ->
    Sys.opaque_identity (J.blit buf ~pool ~width ~c ~max_iter))
;;

let () =
  let c = Stdlib.Complex.{ re = 0.; im = 0. } in
  let pool =
    let num_domains = Stdlib.Domain.recommended_domain_count () - 1 in
    Domainslib.Task.setup_pool ~name:"compute-pool" ~num_domains ()
  in
  Command_unix.run
  @@ Bench.make_command
       [ Bench.Test.create_group
           ~name:"Julia.blit"
           (let small = 320, 240 in
            let medium = 800, 600 in
            let large = 1280, 1024 in
            List.map ~f:(blit_dimension ~pool ~c) [ small; medium; large ])
       ]
;;
