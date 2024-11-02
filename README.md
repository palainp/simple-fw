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
$ sysctl -w net.ipv4.ip_forward=0
# but we need to allow local ARP resolution
$ sysctl -w net.ipv4.conf.all.arp_announce=1
$ sysctl -w net.ipv4.conf.all.arp_ignore=2
$ sysctl -w net.ipv4.conf.all.rp_filter=0
$ sysctl -w net.ipv4.conf.all.arp_filter=0
$ sysctl -w net.ipv4.conf.all.accept_local=1
```


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

And you can even run `ipef3`:
```bash
$ iperf3 -s -p 7070 --bind-dev input
...
$ iperf3 -c 10.10.0.100 --bind-dev output -p 7070
Connecting to host 10.10.0.100, port 7070
[  5] local 10.10.0.200 port 60650 connected to 10.10.0.100 port 7070
[ ID] Interval           Transfer     Bitrate         Retr  Cwnd
[  5]   0.00-1.00   sec  42.1 MBytes   353 Mbits/sec   25   1.06 MBytes       
[  5]   1.00-2.00   sec  42.5 MBytes   357 Mbits/sec    0   1.18 MBytes       
[  5]   2.00-3.00   sec  40.0 MBytes   336 Mbits/sec    0   1.28 MBytes       
[  5]   3.00-4.00   sec  40.0 MBytes   336 Mbits/sec    0   1.35 MBytes       
[  5]   4.00-5.00   sec  37.5 MBytes   315 Mbits/sec    2   1.01 MBytes       
[  5]   5.00-6.00   sec  37.5 MBytes   315 Mbits/sec    0   1.07 MBytes       
[  5]   6.00-7.00   sec  43.8 MBytes   367 Mbits/sec    0   1.11 MBytes       
[  5]   7.00-8.00   sec  43.8 MBytes   367 Mbits/sec    0   1.14 MBytes       
[  5]   8.00-9.00   sec  43.8 MBytes   367 Mbits/sec    0   1.15 MBytes       
[  5]   9.00-10.00  sec  41.2 MBytes   346 Mbits/sec    0   1.16 MBytes       
- - - - - - - - - - - - - - - - - - - - - - - - -
[ ID] Interval           Transfer     Bitrate         Retr
[  5]   0.00-10.00  sec   412 MBytes   346 Mbits/sec   27             sender
[  5]   0.00-10.01  sec   410 MBytes   344 Mbits/sec                  receiver

iperf Done.
```

## Get it Cstruct-free

This unikernel is simple enough to try to make it Cstruct free and check if/wether there is better performances.
First thing is to find where `Cstruct.t` lives:
```bash
$ grep -R Cstruct duniverse/ | cut -d/ -f 2 | sort -u
arp
eqaf
ethernet
mirage
mirage-crypto
mirage-flow
mirage-net
mirage-net-solo5
mirage-solo5
mirage-tcpip
ocaml-cstruct
ocaml-ipaddr
randomconv
```

Some of the dependencies (eqaf, mirage, mirage-crypto, mirage-solo5, ocaml-ipaddr, randomconv) have references to Cstruct only in test files, documentation, or Cstruct-related part of the library (and block for mirage, eio for mirage-crypto but we don't use them in this unikernel):
```bash
$ grep -Ri Cstruct duniverse/eqaf/ | cut -d: -f1 | sort -u
duniverse/eqaf/CHANGES.md
duniverse/eqaf/.cirrus.yml
duniverse/eqaf/eqaf-cstruct.opam
duniverse/eqaf/lib/dune
duniverse/eqaf/lib/eqaf_cstruct.ml
duniverse/eqaf/lib/eqaf_cstruct.mli
duniverse/eqaf/README.md
$ grep -Ri Cstruct duniverse/mirage/ | cut -d: -f1 | sort -u
duniverse/mirage/CHANGES.md
duniverse/mirage/lib/devices/block.ml
$ grep -Ri Cstruct duniverse/mirage-crypto/ | cut -d: -f1 | sort -u
duniverse/mirage-crypto/CHANGES.md
duniverse/mirage-crypto/rng/eio/mirage_crypto_rng_eio.ml
duniverse/mirage-crypto/rng/eio/mirage_crypto_rng_eio.mli
duniverse/mirage-crypto/tests/test_rng.ml
$ grep -Ri Cstruct duniverse/mirage-solo5/ | cut -d: -f1 | sort -u
duniverse/mirage-solo5/CHANGES.md
duniverse/mirage-solo5/lib/dune
duniverse/mirage-solo5/mirage-solo5.opam
$ grep -Ri Cstruct duniverse/ocaml-ipaddr/ | cut -d: -f1 | sort -u
duniverse/ocaml-ipaddr/CHANGES.md
duniverse/ocaml-ipaddr/ipaddr-cstruct.opam
duniverse/ocaml-ipaddr/ipaddr-sexp.opam
duniverse/ocaml-ipaddr/lib/dune
duniverse/ocaml-ipaddr/lib/ipaddr_cstruct.ml
duniverse/ocaml-ipaddr/lib/ipaddr_cstruct.mli
duniverse/ocaml-ipaddr/lib/macaddr_cstruct.ml
duniverse/ocaml-ipaddr/lib/macaddr_cstruct.mli
duniverse/ocaml-ipaddr/lib_test/dune
duniverse/ocaml-ipaddr/lib_test/test_ipaddr.ml
duniverse/ocaml-ipaddr/lib_test/test_macaddr.ml
duniverse/ocaml-ipaddr/macaddr-cstruct.opam
duniverse/ocaml-ipaddr/macaddr-sexp.opam
duniverse/ocaml-ipaddr/README.md
$ grep -Ri Cstruct duniverse/randomconv | cut -d: -f1 | sort -u
duniverse/randomconv/CHANGES.md
```

For the others we need to pin specific branches and update the opam environment, get the new dependencies and rebuild the unikernel:
```bash
$ opam pin https://github.com/palainp/arp.git#remove-cstruct -n
$ opam pin https://github.com/palainp/ethernet.git#remove-cstruct -n
$ opam pin https://github.com/palainp/mirage-flow.git#remove-cstruct -n
$ opam pin https://github.com/palainp/mirage-net.git#remove-cstruct -n
$ opam pin https://github.com/palainp/mirage-net-solo5.git#remove-cstruct -n
$ opam pin https://github.com/palainp/mirage-tcpip.git#remove-cstruct -n
$ opam update
$ mirage configure -t hvt && make depend && dune build
```

Currently the work in mirage-tcip is still in progress....
