let log = Logs.Src.create "rules" ~doc:"FW rules management"
module Log = (val Logs.src_log log : Logs.LOG)

type decision = 
  | ACCEPT
  | DROP
  (* TODO:
  | ESTABLISHED
  | RELATED will need more work... *)

type rule = {
  src : Ipaddr.V4.t ;
  dst : Ipaddr.V4.t ;
  proto : Ipv4_packet.protocol option ;
  action : decision
}

type cb = Ipaddr.V4.t * Cstruct.t -> unit Lwt.t

type t = {
  public_ipv4 : Ipaddr.V4.t ;
  private_ipv4 : Ipaddr.V4.t ;
  mutable l : rule list ;
  last_ressort : cb -> (Ipaddr.V4.t * Cstruct.t) -> unit Lwt.t
}

let init public_ipv4 private_ipv4 default_cb =
  { public_ipv4 ; private_ipv4 ;  l = [] ; last_ressort = default_cb }

(* Here we apply, for easy testing, a default to accept last ressort rule :x *)
let default_accept cb (dest, packet) =
  cb (dest, packet)

(* The default to drop last ressort rule would be *)
let default_drop _cb (dest, _packet) =
  Log.debug (fun f -> f "Filter out a packet to %a..." Ipaddr.V4.pp dest);
  Lwt.return_unit

(* Takes an ipv4 header [ipv4_hdr] and the whole IPv4 [packet] (containing the header).
   We want to filter out any packet matching the [filters] list, and if not filtered,
   transfer it (unchanged) to [forward_to].
   NOTE: We know the packet is not for us. *)
let filter t forward_to (ipv4_hdr, packet) =
  let rec apply_rules_and_forward
  : cb -> (cb -> (Ipaddr.V4.t * Cstruct.t) -> unit Lwt.t) -> (Ipv4_packet.t * Cstruct.t) -> rule list -> unit Lwt.t
  = fun forward_to default_cb (ipv4_hdr, packet) filter_rules ->
    match filter_rules with
    (* If the list is empty -> apply default action *)
    | [] -> default_cb forward_to (ipv4_hdr.dst, packet)
    (* If the packet matches the condition and has an accept action *)
    | {src; dst; proto; action=ACCEPT}::_ when
        ipv4_hdr.src = src && ipv4_hdr.dst = dst &&
        Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto = proto
      -> forward_to (ipv4_hdr.dst, packet)
    (* Otherwise the packet matches and the action is drop *)
    | {src; dst; proto; action=DROP}::_ when
        ipv4_hdr.src = src && ipv4_hdr.dst = dst &&
        Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto = proto
      ->
        Log.debug (fun f -> f "Filter out a packet from %a to %a..." Ipaddr.V4.pp src Ipaddr.V4.pp dst);
        Lwt.return_unit
    (* Or finally the packet does not matche the condition *)
    | _::tail -> apply_rules_and_forward forward_to default_cb (ipv4_hdr, packet) tail
  in
  apply_rules_and_forward forward_to t.last_ressort (ipv4_hdr, packet) t.l
