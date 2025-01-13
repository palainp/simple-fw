external create_tap_interface : string -> (Unix.file_descr * string) = "tuntap_create_tap_interface"

let relay tap1 tap2 =
  let buf = Bytes.create 1600 in
  let rec loop () =
    let read_set, _, _ = Unix.select [tap1; tap2] [] [] (-1.) in
    List.iter (fun fd ->
        let len = Unix.read fd buf 0 1600 in
        let wr_fd = if fd == tap1 then tap2 else tap1 in
        let len' = Unix.write wr_fd buf 0 len in
        assert (len = len'))
      read_set;
    loop ()
  in
  loop ()

[@@@ocaml.warning "-21"]
let () =
  if Array.length Sys.argv != 3 then
    (Printf.eprintf "Usage: %s <tap-interface-1> <tap-interface-2>\n" Sys.argv.(0);
     exit 2);
  let (tap1, tap1_name) = create_tap_interface Sys.argv.(1) in
  let (tap2, tap2_name) = create_tap_interface Sys.argv.(2) in

  Printf.printf "Relaying packets between %s and %s...\n" tap1_name tap2_name;
  try relay tap1 tap2
  with Unix.Unix_error _ ->
    Unix.close tap1;
    Unix.close tap2
