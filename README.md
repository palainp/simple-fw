# A simple bridge firewall

This is a test unikernel that filter or forward packets from one interface to the other.

## Network setup
You also need to set a network environment (as root or with `sudo`):
```bash
# we need to set up two interfaces on the same subnet
$ sudo ip tuntap add input mode tap
$ sudo ip addr add 10.10.0.100/24 dev input
$ sudo ip link set dev input up

$ sudo ip tuntap add output mode tap
$ sudo ip addr add 10.10.0.200/24 dev output
$ sudo ip link set dev output up

# we don't need ip_forward as our unikernel will do it :)
$ sudo sysctl -w net.ipv4.ip_forward=0
# but we need to allow local ARP resolution
$ sudo sysctl -w net.ipv4.conf.all.arp_announce=1
$ sudo sysctl -w net.ipv4.conf.all.arp_ignore=1
$ sudo sysctl -w net.ipv4.conf.all.rp_filter=0
$ sudo sysctl -w net.ipv4.conf.all.arp_filter=0
$ sudo sysctl -w net.ipv4.conf.all.accept_local=1
```

A simple script (created for my fedora Qube, please adapt to your needs :) ) do that: `sudo sh setup_network.sh`. With Qubes you'll also need to change the input policy to accept during the tests :)

## Compile and run
After cloning this repository, you can run the following to build the unikernel:
```bash
$ mirage configure -t hvt && make depend && dune build
...
```

Start the unikernel:
```bash
$ solo5-hvt --mem=32 --net:private=input --net:public=output dist/simple-fw.hvt
            |      ___|
  __|  _ \  |  _ \ __ \
\__ \ (   | | (   |  ) |
____/\___/ _|\___/____/
Solo5: Bindings version v0.9.0
Solo5: Memory map: 32 MB addressable:
Solo5:   reserved @ (0x0 - 0xfffff)
Solo5:       text @ (0x100000 - 0x212fff)
Solo5:     rodata @ (0x213000 - 0x254fff)
Solo5:       data @ (0x255000 - 0x326fff)
Solo5:       heap >= 0x327000 < stack < 0x2000000
2024-11-02T14:33:43-00:00: [INFO] [netif] Plugging into public with mac 42:96:de:49:a0:58 mtu 1500
2024-11-02T14:33:43-00:00: [INFO] [netif] Plugging into private with mac fa:70:42:31:60:fe mtu 1500
...
```

## Tests
Verify the filtering (ICMP, TCP 7070<->ANY, UDP ANY<->ANY are accepted everything else is dropped).
I failed to write packets and specify an interface with `nc` (both the openbsd and the traditional debian version).
Therefore I conducted tests with packeth but it's a bit less convenient:
```bash
$ ping -I 10.10.0.100 10.10.0.200   # ICMP is accepted
...
$ nc -l -s 10.10.0.100 -p 7070      # TCP 7070 is accepted
...
$ nc -l -u -s 10.10.0.100 -p 8080   # UDP ANY is accepted
...
$ nc -l -s 10.10.0.100 -p 1234      # anything else is dropped
...
```

And you can even run `ipef3` (with 3 different terminals, one for `solo5-hvt`, one for `iperf3 -s ...`, and the last one for `iperf3 -c`):
```bash
$ iperf3 -s -p 7070 --bind-dev input
...
$ iperf3 -c 10.10.0.100 --bind-dev output -p 7070
Connecting to host 10.10.0.100, port 7070
[  5] local 10.10.0.200 port 36992 connected to 10.10.0.100 port 7070
[ ID] Interval           Transfer     Bitrate         Retr  Cwnd
[  5]   0.00-1.00   sec   150 MBytes  1.25 Gbits/sec   45   1.15 MBytes
[  5]   1.00-2.00   sec  72.4 MBytes   607 Mbits/sec    0   1.26 MBytes
[  5]   2.00-3.00   sec  72.6 MBytes   609 Mbits/sec    0   1.34 MBytes
[  5]   3.00-4.00   sec  71.5 MBytes   600 Mbits/sec    4   1.00 MBytes
[  5]   4.00-5.00   sec  77.5 MBytes   650 Mbits/sec    0   1.07 MBytes
[  5]   5.00-6.00   sec  73.1 MBytes   613 Mbits/sec    0   1.11 MBytes
[  5]   6.00-7.00   sec  77.6 MBytes   651 Mbits/sec    0   1.14 MBytes
[  5]   7.00-8.00   sec  72.6 MBytes   609 Mbits/sec    0   1.19 MBytes
[  5]   8.00-9.00   sec  73.9 MBytes   620 Mbits/sec    0   1.24 MBytes
[  5]   9.00-10.00  sec  77.2 MBytes   648 Mbits/sec    0   1.28 MBytes
- - - - - - - - - - - - - - - - - - - - - - - - -
[ ID] Interval           Transfer     Bitrate         Retr
[  5]   0.00-10.00  sec   820 MBytes   688 Mbits/sec   49             sender
[  5]   0.00-10.02  sec   818 MBytes   685 Mbits/sec                  receiver

iperf Done.
```

## Get it Bigarray-free

Bigarrays are not managed by the Ocaml runtime, here we want to try to have Cstruct.t backed by bytes instead of bigarrays. In order to do that the most convenient way is to pin a specific branch for `ocaml-cstruct`, adapt `mirage-net-solo5` to that new low-layer of Cstruct, and do not modify any other existing code, refetch dependencies, and recompile:
```bash
$ opam pin https://github.com/palainp/mirage-net-solo5.git#no-bigarray -n
$ opam pin https://github.com/hannesm/ocaml-cstruct.git#no-bigarray -y
...
$ mirage clean && rm -rf duniverse/ && mirage configure -t hvt && make depend && dune build
...
```

## Performance measurement

Now we can show the performance results (the receiver line in the client `iperfd3` output, which correspond to the server's output, anyway, both are similar) in following table (tested with spt target inside a single Qube, please note that this can reduce the overall performances compared to a barmetal distribution), using iperf3 (with TCP as default), running for 30s (option `-t 30`), restarted between each run, 32MB memory for the unikernel, other configuration are welcome :). Ocaml 5 is provided by https://github.com/mirage/ocaml-solo5/pull/134 :
| Av. bandwitdh (Gbits/sec) |     Ocaml 4.14.1   |     Ocaml 5.2   |
|---------------------------|--------------------|-----------------|
| Cstruct as BA             |      1.16          |      1.12       |
| Cstruct as bytes          |      1.42          |      1.41       |

| Tot. transfer (GB/30s) |     Ocaml 4.14.1   |     Ocaml 5.2   |
|------------------------|--------------------|-----------------|
| Cstruct as BA          |       4.07         |      3.90       |
| Cstruct as bytes       |       4.95         |      4.92       |
