(* This unikernel is largely inspired by the example at https://github.com/mirage/mirage-nat/ *)

open Lwt.Infix

module Main
    (* our unikernel is functorized over the physical, ethernet, ARP, and IPv4
       modules for the public and private interfaces, so each one shows up as
       a module argument. *)
    (Public_net: Mirage_net.S) (Private_net: Mirage_net.S)
    (Public_ethernet : Ethernet.S) (Private_ethernet : Ethernet.S)
    (Public_arpv4 : Arp.S) (Private_arpv4 : Arp.S)
    (Public_ipv4 : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t and type prefix = Ipaddr.V4.Prefix.t)
    (Private_ipv4 : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t and type prefix = Ipaddr.V4.Prefix.t)
    (Random : Mirage_random.S) (Clock : Mirage_clock.MCLOCK)
  = struct

  (* configure logs, so we can use them later *)
  let log = Logs.Src.create "nat" ~doc:"FW device"
  module Log = (val Logs.src_log log : Logs.LOG)

  (* We'll need to make routing decisions on both the public and private
     interfaces. *)
  module Public_routing = Routing.Make(Log)(Public_arpv4)
  module Private_routing = Routing.Make(Log)(Private_arpv4)

  type decision = 
    | ACCEPT
    | DROP
  type rule = {
    src : Ipaddr.V4.t ;
    dst : Ipaddr.V4.t ;
    proto : Ipv4_packet.protocol option ;
    action : decision
  }

  (* This is a test with an hardcoded rule dropping TCP packets from 10.10.10.10 to 172.10.10.10 *)
  let filter_rules = [
    {src=Ipaddr.V4.of_string_exn "10.10.10.10"; dst=Ipaddr.V4.of_string_exn "172.10.10.10"; proto=Some `TCP; action=ACCEPT};
  ]

  (* the specific impls we're using show up as arguments to start. *)
  let start public_netif private_netif
            public_ethernet private_ethernet
            public_arpv4 private_arpv4
            public_ipv4 private_ipv4 _rng () =

    (* Forward the (eth_hdr, ipv4_hdr, payload) packet to the public interface *)
    let output_public :
    (Ethernet.Packet.t * Ipv4_packet.t * Cstruct.t) -> unit Lwt.t
     = fun (_eth_hdr, ipv4_hdr, payload) ->
      (* For IPv4 only one prefix can be configured so the list is always of length 1 *)
      let network = List.hd (Public_ipv4.configured_ips public_ipv4) in

      Public_routing.destination_mac network None public_arpv4 ipv4_hdr.dst >>= function
      | Error _ ->
        Log.debug (fun f -> f "Could not send a packet from the public interface to the local network,\
                                as a failure occurred on the ARP layer");
        Lwt.return_unit
      | Ok destination ->
        Public_ethernet.write public_ethernet destination `IPv4 (fun b ->
          let len = Cstruct.length payload in
          Cstruct.blit payload 0 b 0 len ;
          len) >>= function
          | Error e ->
            Log.err (fun f -> f "Failed to send packet from public interface: %a"
                          Public_ethernet.pp_error e);
            Lwt.return_unit
          | Ok () -> Lwt.return_unit
    in

    (* Forward the (eth_hdr, ipv4_hdr, payload) packet to the private interface *)
    let output_private :
    (Ethernet.Packet.t * Ipv4_packet.t * Cstruct.t) -> unit Lwt.t
     = fun (_eth_hdr, ipv4_hdr, payload) ->
      (* For IPv4 only one prefix can be configured so the list is always of length 1 *)
      let network = List.hd (Private_ipv4.configured_ips private_ipv4) in

      Private_routing.destination_mac network None private_arpv4 ipv4_hdr.dst >>= function
      | Error _ ->
        Log.debug (fun f -> f "Could not send a packet from the private interface to the local network,\
                                as a failure occurred on the ARP layer");
        Lwt.return_unit
      | Ok destination ->
        Private_ethernet.write private_ethernet destination `IPv4 (fun b ->
          let len = Cstruct.length payload in
          Cstruct.blit payload 0 b 0 len ;
          len) >>= function
          | Error e ->
            Log.err (fun f -> f "Failed to send packet from private interface: %a"
                          Private_ethernet.pp_error e);
            Lwt.return_unit
          | Ok () -> Lwt.return_unit
    in

    (* Takes an ethernet packet and unmarshal it into an IPv4 packet.
       We want to filter out any packet matching the [filters] list,
       and if not filtered, transfer it to [forward_to]. *)
    let filter filter_rules forward_to packet =

      let rec apply_rules_and_forward :
      (Ethernet.Packet.t * Ipv4_packet.t * Cstruct.t -> unit Lwt.t) -> (Ethernet.Packet.t * Ipv4_packet.t * Cstruct.t) -> rule list -> unit Lwt.t
       = fun forward_to (eth_hdr, ipv4_hdr, payload) filter_rules  ->
        match filter_rules with
        (* If the list is empty -> apply default action, here accept *)
        | [] -> forward_to (eth_hdr, ipv4_hdr, payload)
        (* If the packet matches the condition and has an accept action *)
        | {src; dst; proto; action=ACCEPT}::_ when
            ipv4_hdr.src = src && ipv4_hdr.dst = dst &&
            Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto = proto
          -> forward_to (eth_hdr, ipv4_hdr, payload)
        (* Otherwise the packet matches and the action is drop *)
        | {src; dst; proto=_; action=DROP}::_ when
            ipv4_hdr.src = src && ipv4_hdr.dst = dst
          ->
            Log.debug (fun f -> f "Filter out a packet...");
            Lwt.return_unit
        (* Or finally the packet does not matche the condition *)
        | _::tail -> apply_rules_and_forward forward_to (eth_hdr, ipv4_hdr, payload) tail
      in

      (* Handle ethernet *)
      match Ethernet.Packet.of_cstruct packet with
      | Result.Error s ->
          Logs.err (fun m -> m "Can't parse packet: %s" s);
          Lwt.return_unit
      | Result.Ok (eth_hdr, payload) ->
        match eth_hdr.Ethernet.Packet.ethertype with
        | `ARP | `IPv6 ->
          Log.err (fun f -> f "packet is not ipv4");
          Lwt.return_unit
        | `IPv4 ->
          (* Handle IPv4 *)
          match Ipv4_packet.Unmarshal.of_cstruct payload with
          | Result.Error s ->
              Logs.err (fun m -> m "Can't parse IPv4 packet: %s" s);
              Lwt.return_unit
          | Result.Ok (ipv4_hdr, payload) ->
            apply_rules_and_forward forward_to (eth_hdr, ipv4_hdr, payload) filter_rules
    in

    (* we need to establish listeners for the private and public interfaces *)
    (* we're interested in all traffic to the physical interface; we'd like to
       send ARP traffic to the normal ARP listener and responder,
       handle ipv4 traffic with the functions we've defined above for NATting,
       and ignore all ipv6 traffic (ipv6 has no need for NAT!). *)
    let listen_public =
      let header_size = Ethernet.Packet.sizeof_ethernet
      and input = (* Takes an ethernet packet and send it to the relevant callback *)
        Public_ethernet.input
          ~arpv4:(Public_arpv4.input public_arpv4)
          ~ipv4:(filter filter_rules output_private)
          ~ipv6:(fun _ -> Lwt.return_unit) (* IPv6 is not relevant so far -> DROP *)
          public_ethernet
      in
      Public_net.listen ~header_size public_netif input >>= function
      | Error e -> Log.debug (fun f -> f "public interface stopped: %a"
                                 Public_net.pp_error e); Lwt.return_unit
      | Ok () -> Log.debug (fun f -> f "public interface terminated normally");
        Lwt.return_unit
    in

    let listen_private =
      let header_size = Ethernet.Packet.sizeof_ethernet
      and input = (* Takes an ethernet packet and send it to the relevant callback *)
        Private_ethernet.input
          ~arpv4:(Private_arpv4.input private_arpv4)
          ~ipv4:(filter filter_rules output_public)
          ~ipv6:(fun _ -> Lwt.return_unit) (* IPv6 is not relevant so far -> DROP *)
          private_ethernet
      in
      Private_net.listen ~header_size private_netif input >>= function
      | Error e -> Log.debug (fun f -> f "private interface stopped: %a"
                                 Private_net.pp_error e); Lwt.return_unit
      | Ok () -> Log.debug (fun f -> f "private interface terminated normally");
        Lwt.return_unit
    in

    (* Notice how we haven't said anything about ICMP anywhere.  The unikernel
       doesn't know anything about it, so pinging this host on either interface
       will just be ignored -- the only way this unikernel can be easily seen,
       without sending traffic through it, is via ARP.  The `arping` command
       line utility might be useful in trying to see whether your unikernel is
       up.  *)

    (* start both listeners, and continue as long as both are working. *)
    Lwt.pick [
      listen_public;
      listen_private;
    ]
end
