#!/bin/env sh

set -e
export PATH=$PATH:/usr/sbin

ip tuntap add input mode tap
ip addr add 10.10.0.100/24 dev input
ip link set dev input up

ip tuntap add output mode tap
ip addr add 10.10.0.200/24 dev output
ip link set dev output up

sysctl -w net.ipv4.ip_forward=0
sysctl -w net.ipv4.conf.all.arp_announce=1
sysctl -w net.ipv4.conf.all.arp_ignore=1
sysctl -w net.ipv4.conf.all.rp_filter=0
sysctl -w net.ipv4.conf.all.arp_filter=0
sysctl -w net.ipv4.conf.all.accept_local=1
