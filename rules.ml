let log = Logs.Src.create "rules" ~doc:"FW rules management"
module Log = (val Logs.src_log log : Logs.LOG)

type decision = 
  | ACCEPT
  | DROP
  (* TODO:
  | ESTABLISHED
  | RELATED will need more work... *)

type rule = {
  src : Ipaddr.V4.Prefix.t ;
  dst : Ipaddr.V4.Prefix.t ;
  proto : Ipv4_packet.protocol option ;
  action : decision option ;
}

let rule_to_string r =
  String.concat " " [
    Ipaddr.V4.Prefix.to_string r.src ;
    "->" ;
    Ipaddr.V4.Prefix.to_string r.dst ;
    "(" ;
    begin match r.proto with
    | Some `TCP -> "TCP"
    | Some `UDP -> "UDP"
    | _ -> "?"
    end ;
    ") :" ;
    begin match r.action with
    | Some ACCEPT -> "ACCEPT"
    | Some DROP -> "DROP"
    | None -> "XXX"
    end ;
  ]

(* A match is either the target is joker or the ip is in the target network *)
let match_ip ip target =
  target = Ipaddr.V4.Prefix.global || Ipaddr.V4.Prefix.mem ip target

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

(* The update packet is (as a UDP packet):
   2B port source + 2B port dest (our port must be 1234)+ 2B len + 2B crc
   + 4B magic string "1234"
   + 4B IP source + 1B netmask
   + 4B IP dest + 1B netmask
   + 1B insert or append (INSERT=0, APPEND=1)
   + 1B the protocol code (TCP=6, UDP=17)
   + 1B the decision (0=DROP, 1=ACCEPT)
   + 1B '\n'
*)
let magic_hdr = Int32.of_string "0x31323334"

let magic_is_present payload =
Cstruct.hexdump payload ;
  Cstruct.length payload = 26 &&
  Cstruct.BE.get_uint16 payload 2 = 1234 &&
  Cstruct.BE.get_uint32 payload 8 = magic_hdr &&
  Cstruct.get_uint8 payload 25 = 10 (* final '\n' *)
  (* I currently don't bother to check the crc... *)

let update t payload =
  let src = Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 payload 12) in
  let src_mask = Cstruct.get_uint8 payload 16 in
  let dst = Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 payload 17) in
  let dst_mask = Cstruct.get_uint8 payload 21 in

  let src = Ipaddr.V4.Prefix.make src_mask src in
  let dst = Ipaddr.V4.Prefix.make dst_mask dst in

  let ins_or_app = Cstruct.get_uint8 payload 22 in
  let proto = match Cstruct.get_uint8 payload 23 with
    | 6 -> Some `TCP
    | 17 -> Some `UDP
    | _ -> None
  in
  let action = match Cstruct.get_uint8 payload 24 with
    | 0 -> Some DROP
    | 1 -> Some ACCEPT
    | _ -> None
  in
  let r = {src ; dst ; proto ; action} in
  match ins_or_app, proto, action with
  (* not recognized protocol *)
  | _, None, _
  (* not recognized action *)
  | _, _, None ->
    Log.err(fun f -> f "Cannot recognize rule: %s" (rule_to_string r))
  (* other cases *)
  | 0, _, _ ->
    Log.info(fun f -> f "Insert rule: %s" (rule_to_string r));
    t.l <- r::t.l
  | 1, _, _ ->
    let r = {src ; dst ; proto ; action} in
    Log.info(fun f -> f "Append rule: %s" (rule_to_string r));
    t.l <- List.append t.l [{src ; dst ; proto ; action}]
  (* fall case where the rule is neither to be inserted or appended *)
  | _, _, _ ->
    Log.err(fun f -> f "Don't know chat to do with rule: %s" (rule_to_string r))

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
    | {src; dst; proto; action=Some ACCEPT}::_ when
        match_ip ipv4_hdr.src src && match_ip ipv4_hdr.dst dst &&
        Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto = proto
      -> forward_to (ipv4_hdr.dst, packet)
    (* Otherwise the packet matches and the action is drop *)
    | {src; dst; proto; action=Some DROP}::_ when
        match_ip ipv4_hdr.src src && match_ip ipv4_hdr.dst dst &&
        Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto = proto
      ->
        Log.debug (fun f -> f "Filter out a packet from %a to %a..." Ipaddr.V4.Prefix.pp src Ipaddr.V4.Prefix.pp dst);
        Lwt.return_unit
    (* Or finally the packet does not matche the condition *)
    | _::tail -> apply_rules_and_forward forward_to default_cb (ipv4_hdr, packet) tail
  in
  apply_rules_and_forward forward_to t.last_ressort (ipv4_hdr, packet) t.l
