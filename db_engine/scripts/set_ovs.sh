#!/bin/bash
set -e

if [ $EUID -ne 0 ]; then
	echo "This script must be run as root!"
	exit 1
fi

N=${1:-4}
PREFIX="icnode"
NET_PREFIX="192.168.20"
NET_BACKUP="192.168.30"

ovs-vsctl add-br ovs-br1
ip link set ovs-br1 up
ip addr add $NET_PREFIX.1/24 dev ovs-br1
#ip addr add $NET_BACKUP.1/24 dev ovs-br1
ip addr add 192.168.1.1/24 dev ovs-br1

for idx in `seq 1 $N`; do
	idx2=$(($idx+1))
	#if [ $idx -eq $N ]; then
	  #ovs-docker add-port ovs-br1 eth1 $PREFIX$idx --ipaddress=$NET_BACKUP.2/24
	#else
	  ovs-docker add-port ovs-br1 eth1 $PREFIX$idx --ipaddress=$NET_PREFIX.${idx2}/24
	#fi
done
