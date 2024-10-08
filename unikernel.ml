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
  let log = Logs.Src.create "fw" ~doc:"FW device"
  module Log = (val Logs.src_log log : Logs.LOG)

  (* We'll need to make routing decisions on both the public and private
     interfaces. *)
  module Public_routing = Routing.Make(Log)(Public_arpv4)
  module Private_routing = Routing.Make(Log)(Private_arpv4)

  (* the specific impls we're using show up as arguments to start. *)
  let start public_netif private_netif
            public_ethernet private_ethernet
            public_arpv4 private_arpv4
            public_ipv4 private_ipv4 _rng () =

    (* Creates a set of rules (empty) and a default condition (accept) *)
    let filter_rules =
      let public_network = List.hd (Public_ipv4.configured_ips public_ipv4) in
      let public_ipv4 = Ipaddr.V4.Prefix.address public_network in
      let private_network = List.hd (Private_ipv4.configured_ips private_ipv4) in
      let private_ipv4 = Ipaddr.V4.Prefix.address private_network in

      Rules.init public_ipv4 private_ipv4 Rules.default_accept
    in

    (* Takes an IPv4 [packet], unmarshal it, check if we're the destination and
       the payload is some sort of rule update, apply that, and if we're not the
       destination, use the filter_rules to [out] the packet or not. *)
    let filter out packet =
      (* Handle IPv4 only... *)
      match Ipv4_packet.Unmarshal.of_cstruct packet with
      | Result.Error s ->
        Logs.err (fun m -> m "Can't parse IPv4 packet: %s" s);
        Lwt.return_unit

      (* If the packet is a UDP packet to us, decode *)
      | Result.Ok (ipv4_hdr, payload) when
        Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto =
 Some `UDP &&
        (ipv4_hdr.dst = filter_rules.public_ipv4 || ipv4_hdr.dst = filter_rules.private_ipv4) &&
        Rules.magic_is_present payload ->
        Logs.info (fun m -> m "Got an update message from %a" Ipaddr.V4.pp ipv4_hdr.src);
        Rules.update filter_rules payload ;
        Lwt.return_unit

      (* If the packet is a UDP packet to us but not an update packet, ignore *)
      | Result.Ok (ipv4_hdr, _payload) when
        (ipv4_hdr.dst = filter_rules.public_ipv4 || ipv4_hdr.dst = filter_rules.private_ipv4) ->
        Logs.debug (fun m -> m "Got an message from %a but not an update packet" Ipaddr.V4.pp ipv4_hdr.src);
        Lwt.return_unit

      (* Otherwise try to forward (or not) the packet *)
      | Result.Ok (ipv4_hdr, _payload) ->
        Rules.filter filter_rules out (ipv4_hdr, packet)
    in

    (* Forward the (dest, packet) [packet] to the public interface, using [dest] to understand how to route *)
    let output_public :
    (Ipaddr.V4.t * Cstruct.t) -> unit Lwt.t
     = fun (dest, packet) ->
      (* For IPv4 only one prefix can be configured so the list is always of length 1 *)
      let network = List.hd (Public_ipv4.configured_ips public_ipv4) in

      Public_routing.destination_mac network None public_arpv4 dest >>= function
      | Error _ ->
        Log.debug (fun f -> f "Could not send a packet from the public interface to the local network,\
                                as a failure occurred on the ARP layer");
        Lwt.return_unit
      | Ok destination ->
        Public_ethernet.write public_ethernet destination `IPv4 (fun b ->
          let len = Cstruct.length packet in
          Cstruct.blit packet 0 b 0 len ;
          len) >>= function
          | Error e ->
            Log.err (fun f -> f "Failed to send packet from public interface: %a"
                          Public_ethernet.pp_error e);
            Lwt.return_unit
          | Ok () ->
            Lwt.return_unit
    in

    (* Forward the (dest, packet) [packet] to the private interface, using [dest] to understand how to route *)
    let output_private :
    (Ipaddr.V4.t * Cstruct.t) -> unit Lwt.t
     = fun (dest, packet) ->
      (* For IPv4 only one prefix can be configured so the list is always of length 1 *)
      let network = List.hd (Private_ipv4.configured_ips private_ipv4) in

      Private_routing.destination_mac network None private_arpv4 dest >>= function
      | Error _ ->
        Log.debug (fun f -> f "Could not send a packet from the private interface to the local network,\
                                as a failure occurred on the ARP layer");
        Lwt.return_unit
      | Ok destination ->
        Private_ethernet.write private_ethernet destination `IPv4 (fun b ->
          let len = Cstruct.length packet in
          Cstruct.blit packet 0 b 0 len ;
          len) >>= function
          | Error e ->
            Log.err (fun f -> f "Failed to send packet from private interface: %a"
                          Private_ethernet.pp_error e);
            Lwt.return_unit
          | Ok () -> Lwt.return_unit
    in

    (* we need to establish listeners for the private and public interfaces *)
    (* we're interested in all traffic to the physical interface; we'd like to
       send ARP traffic to the normal ARP listener and responder,
       handle ipv4 traffic with the functions we've defined above for filtering,
       and ignore all ipv6 traffic. *)
    let listen_public =
      let header_size = Ethernet.Packet.sizeof_ethernet
      and input = (* Takes an ethernet packet and send it to the relevant callback *)
        Public_ethernet.input
          ~arpv4:(Public_arpv4.input public_arpv4)
          ~ipv4:(filter output_private)
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
          ~ipv4:(filter output_public)
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
