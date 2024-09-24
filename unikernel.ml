(* This unikernel is largely inspired by the example at https://github.com/mirage/mirage-nat/ *)

open Lwt.Infix

module Main
    (* our unikernel is functorized over the physical, ethernet, ARP, and IPv4
       modules for the public and private interfaces, so each one shows up as
       a module argument. *)
    (Public_net: Mirage_net.S) (Private_net: Mirage_net.S)
    (Public_ethernet : Ethernet.S) (Private_ethernet : Ethernet.S)
    (Random : Mirage_random.S) (Clock : Mirage_clock.MCLOCK)
  = struct

  (* configure logs, so we can use them later *)
  let log = Logs.Src.create "fw" ~doc:"FW device"
  module Log = (val Logs.src_log log : Logs.LOG)

  (* the specific impls we're using show up as arguments to start. *)
  let start public_netif private_netif
            _public_ethernet _private_ethernet
            _rng () =

    (* Creates a set of rules (empty) and a default condition (accept) *)
    let filter_rules =
      Rules.init true
    in

    (* let output_arp_public :
    (Cstruct.t * Macaddr.t * Macaddr.t) -> unit Lwt.t
    = fun (packet, src, dest) ->
      let size = Arp_packet.size in
      Public_ethernet.write public_ethernet dest `ARP ~size ~src
        (fun b -> let len = Cstruct.length packet in
                  Cstruct.blit packet 0 b 0 len ;
                  len) >>= function
        | Error e ->
          Log.err (fun f -> f "error %a while outputting public ARP packet"
                        Public_ethernet.pp_error e);
          Lwt.return_unit
        | Ok () ->
          Lwt.return_unit
    in

    let output_arp_private :
    (Cstruct.t * Macaddr.t * Macaddr.t) -> unit Lwt.t
    = fun (packet, src, dest) ->
      let size = Arp_packet.size in
      Private_ethernet.write private_ethernet dest `ARP ~size ~src
        (fun b -> let len = Cstruct.length packet in
                  Cstruct.blit packet 0 b 0 len ;
                  len) >>= function
        | Error e ->
          Log.err (fun f -> f "error %a while outputting private ARP packet"
                        Private_ethernet.pp_error e);
          Lwt.return_unit
        | Ok () ->
          Lwt.return_unit
    in *)

    (* Takes an arp packet, and if we are concerned reply to it, otherwise forward it
    on the other link *)
    (* let handle_arp thisout otherout my_mac my_ip packet =
      match Arp_packet.decode packet with
      | Result.Error s ->
        Logs.err (fun m -> m "Can't parse Arp packet: %a" Arp_packet.pp_error s);
        Lwt.return_unit

      | Ok arp ->
        match arp.operation, arp.target_ip with
        | Request, target_ip when Ipaddr.V4.compare target_ip my_ip = 0 ->
          let reply:Arp_packet.t = {operation=Arp_packet.Reply; source_mac=my_mac; source_ip=my_ip; target_mac=arp.source_mac; target_ip=arp.source_ip;} in
          thisout (Arp_packet.encode reply, my_mac, arp.source_mac)
          (* reply to it *)
        | Reply, target_ip when Ipaddr.V4.compare target_ip my_ip = 0 ->
          Lwt.return_unit
          (* got our reply, deal with it *)
        | _, _ ->
          otherout (packet, arp.source_mac, arp.target_mac)
          (* any other case forward on the other interface *)
    in *)
    (* let handle_arp thisout my_ip packet =
      thisout(my_ip, packet)
    in *)

    (* Takes an IPv4 [packet], unmarshal it, check if we're the destination and
       the payload is some sort of rule update, apply that, and if we're not the
       destination, use the filter_rules to [out] the packet or not. *)
    let is_forwardable packet =
      (* Handle IPv4 only... *)
      match Ipv4_packet.Unmarshal.of_cstruct packet with
      | Result.Error s ->
        Logs.err (fun m -> m "Can't parse IPv4 packet: %s" s);
        false

      (* If the packet is a UDP packet to us, decode *)
      (* | Result.Ok (ipv4_hdr, payload) when
        Ipv4_packet.Unmarshal.int_to_protocol ipv4_hdr.Ipv4_packet.proto =
 Some `UDP &&
        (ipv4_hdr.dst = filter_rules.public_ipv4 || ipv4_hdr.dst = filter_rules.private_ipv4) &&
        Rules.magic_is_present payload ->
        Logs.info (fun m -> m "Got an update message from %a" Ipaddr.V4.pp ipv4_hdr.src);
        Rules.update filter_rules payload ;
        false

      (* If the packet is a UDP packet to us but not an update packet, ignore *)
      | Result.Ok (ipv4_hdr, _payload) when
        (ipv4_hdr.dst = filter_rules.public_ipv4 || ipv4_hdr.dst = filter_rules.private_ipv4) ->
        Logs.debug (fun m -> m "Got an message from %a but not an update packet" Ipaddr.V4.pp ipv4_hdr.src);
        false *)

      (* Otherwise try to forward (or not) the packet *)
      | Result.Ok (ipv4_hdr, payload) ->
        Rules.filter filter_rules (ipv4_hdr, payload)
    in

    (* Forward the (dest, packet) [packet] to the public interface, using [dest] to understand how to route *)
    (* let output_public :
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
    in *)

    (* Forward the (dest, packet) [packet] to the private interface, using [dest] to understand how to route *)
    let output_private :
    Cstruct.t -> unit Lwt.t
     = fun packet ->
      (* For IPv4 only one prefix can be configured so the list is always of length 1 *)
        let len = Cstruct.length packet in
        Private_net.write private_netif ~size:len (fun b -> Cstruct.blit packet 0 b 0 len ; len) >|= function
        | Ok () -> ()
        | Error e ->
          Log.warn (fun f -> f "netif write errored %a" Private_net.pp_error e) ;
          ()
    in

    (* we need to establish listeners for the private and public interfaces *)
    (* we're interested in all traffic to the physical interface; we'd like to
       send ARP traffic to the normal ARP listener and responder,
       handle ipv4 traffic with the functions we've defined above for filtering,
       and ignore all ipv6 traffic. *)
    let listen_public =
      let header_size = Ethernet.Packet.sizeof_ethernet
      and input frame= (* Takes an ethernet packet and send it to the relevant callback *)
        (* Public_ethernet.input *)
          (* ~arpv4:(handle_arp output_arp_public output_arp_private  (Public_ethernet.mac public_ethernet) filter_rules.public_ipv4)
          ~arpv4:(fun _ -> handle_arp output_private filter_rules.public_ipv4 frame)
          (* ~ipv4:(filter output_private frame) *)
          ~ipv4:(fun _ -> handle_arp output_private filter_rules.public_ipv4 frame)
          ~ipv6:(fun _ -> Lwt.return_unit) (* IPv6 is not relevant so far -> DROP *)
          _public_ethernet frame *)
          match Ethernet.Packet.of_cstruct frame with
          | Ok (header, payload) ->
            begin
              match header.Ethernet.Packet.ethertype with
              | `ARP -> output_private frame
              | `IPv4 when is_forwardable payload -> output_private frame
              | _ -> Lwt.return_unit
            end
          | Error s ->
            Log.debug (fun f -> f "dropping Ethernet frame: %s" s);
            Lwt.return_unit

        (* let len = Cstruct.length frame in
        Private_net.write private_netif ~size:len (fun b -> Cstruct.blit frame 0 b 0 len ; len) >|= function
        | Ok () -> ()
        | Error e ->
          Log.warn (fun f -> f "netif write errored %a" Private_net.pp_error e) ;
          ()*)
        (* output_private (filter_rules.public_ipv4, frame) *)
      in
      Public_net.listen ~header_size public_netif input >>= function
      | Error e -> Log.debug (fun f -> f "public interface stopped: %a"
                                 Public_net.pp_error e); Lwt.return_unit
      | Ok () -> Log.debug (fun f -> f "public interface terminated normally");
        Lwt.return_unit
    in

    let listen_private =
      let header_size = Ethernet.Packet.sizeof_ethernet
      and input frame = (* Takes an ethernet packet and send it to the relevant callback *)
        (* Private_ethernet.input
          ~arpv4:(handle_arp output_arp_private output_arp_public (Private_ethernet.mac private_ethernet) filter_rules.private_ipv4)
          ~ipv4:(filter output_public)
          ~ipv6:(fun _ -> Lwt.return_unit) (* IPv6 is not relevant so far -> DROP *)
          private_ethernet *)
        let len = Cstruct.length frame in
        Public_net.write public_netif ~size:len (fun b -> Cstruct.blit frame 0 b 0 len ; len) >|= function
        | Ok () -> ()
        | Error e ->
          Log.warn (fun f -> f "netif write errored %a" Public_net.pp_error e) ;
          ()
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