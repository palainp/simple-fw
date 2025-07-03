let t0 = Miou_solo5.clock_monotonic ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Miou_solo5.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

(* - ARPv4 from [ic] go to [oc]
   - IPv4 from [ic] which validates [is_forwarded] go to [oc]
 *)
let listen ~is_forwardable (ic, cfg) oc =
  let open Miou_solo5_net in
  let bstr = Bstr.create (14 + cfg.Miou_solo5.Net.mtu) in
  let rec go () =
    let len = Miou_solo5.Net.read_bigstring ic bstr in
    let pkt = Miou_solo5_net.Ethernet.Packet.decode bstr ~len in
    let res = Result.map (fun (pkt, payload) -> pkt.Ethernet.Packet.protocol, payload) pkt in
    begin match res with
    | Ok (Some Ethernet.ARPv4, _) -> Miou_solo5.Net.write_bigstring oc ~off:0 ~len bstr
    | Ok (Some Ethernet.IPv4, payload) when is_forwardable payload ->
      Miou_solo5.Net.write_bigstring oc ~off:0 ~len bstr
    | _ -> () end;
    go () in
  Miou.async go

let run _ =
  Miou_solo5.(run [ net "public"; net "private" ]) @@ fun (public, cfg0) (priv, cfg1) () ->
  let rules = Rules.init true in
  let is_forwardable sbstr =
    let open Miou_solo5_net in
    match IPv4.Packet.decode sbstr with
    | Ok (pkt, sbstr) -> Rules.filter rules (pkt, sbstr)
    | Error _ -> false in
  let prm0 = listen ~is_forwardable (public, cfg0) priv in (* public -> private *)
  let prm1 = listen ~is_forwardable (priv, cfg1) public in (* private -> public *)
  Miou.await_all [ prm0; prm1 ]
  |> List.iter (function Ok () -> () | Error exn -> raise exn)

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let setup_logs utf_8 style_renderer level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stdout);
  Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let term =
  let open Term in
  const run $ setup_logs

let cmd =
  let info = Cmd.info "simple-fw" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
