#!/bin/bash

ip="192.168.20.1/24"
delay=${1:-20}ms
set -x
tc qdisc del dev eth1 root
#tc qdisc del dev lo root
tc qdisc add dev eth1 root handle 1: prio
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.2 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.3 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.4 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.5 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.6 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.7 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.8 flowid 2:1
tc filter add dev eth1 parent 1:0 protocol ip prio 1 u32 match ip dst 192.168.20.9 flowid 2:1
tc qdisc add dev eth1 parent 1:1 handle 2: netem delay $delay