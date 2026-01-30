#!/bin/bash
nodes=${1:-4}
PREFIX="192.168.20."

num=$((nodes))
for (( nodeId=0; nodeId<num; nodeId++ )); do
  addr=$PREFIX$((nodeId + 2))
  ssh -o StrictHostKeyChecking=no root@${addr} "/root/ShardDB/scripts/add_latency.sh"
done
