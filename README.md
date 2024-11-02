# A simple bridge firewall

This is a test unikernel that filter or forward packets from one interface to the other.

After cloning this repository, you can run the following to build the unikernel:
```bash
mirage configure -t hvt && make depend && dune build
```

You also need to set a network environment (as root or with `sudo`):
```bash
# we need to set up two interfaces on the same subnet
sudo ip tuntap add input mode tap
sudo ip addr add 10.10.0.100/24 dev input
sudo ip link set dev input up

sudo ip tuntap add output mode tap
sudo ip addr add 10.10.0.200/24 dev output
sudo ip link set dev output up

# we don't need ip_forward as our unikernel will do it :)
sysctl -w net.ipv4.ip_forward=0
# but we need to allow local ARP resolution
sysctl -w net.ipv4.conf.all.arp_announce=1
sysctl -w net.ipv4.conf.all.arp_ignore=2
sysctl -w net.ipv4.conf.all.rp_filter=0
sysctl -w net.ipv4.conf.all.arp_filter=0
```

Start the unikernel:
```bash
solo5-hvt --mem=32 --net:private=input --net:public=output dist/simple-fw.hvt
```

Verify the filtering (ICMP, TCP 7070<->ANY, UDP ANY<->ANY are accepted everything else is dropped).
I failed to write packets and specify an interface with `nc` (both the openbsd and the traditional debian version).
Therefore I conducted tests with packeth but it's a bit less convenient:
```bash
ping -I 10.10.0.100 10.10.0.200   # ICMP is accepted

nc -l -s 10.10.0.100 -p 7070      # TCP 7070 is accepted

nc -l -u -s 10.10.0.100 -p 8080   # UDP ANY is accepted

nc -l -s 10.10.0.100 -p 1234      # anything else is dropped
```
