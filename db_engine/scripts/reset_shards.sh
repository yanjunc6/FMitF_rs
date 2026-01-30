#!/bin/bash
nodes=${1:-8}
mode=${2:-0}
verbose=${3:-""}
preVerification=${4:-""}
PREFIX="192.168.20."
godir="/usr/local/go/bin/go"

for (( nodeId=0; nodeId<nodes; nodeId++ )); do
  addr=$PREFIX$((nodeId + 2))
  ssh -o StrictHostKeyChecking=no root@${addr} "pkill -f startServer"
  ssh -o StrictHostKeyChecking=no root@${addr} "cd /root/ShardDB; \
          cp data/*.db server/db/;"

  for (( serverId=0; serverId<10; serverId++ )); do
    id=$((nodeId*10 + serverId))
    logFile="shardLog${id}.txt"
    ssh -o StrictHostKeyChecking=no root@${addr} "cd /root/ShardDB; \
          rm -f ${logFile}; touch ${logFile}; \
          ${godir} run startServer.go --config=node --id=${id} --mode=${mode} ${verbose} ${preVerification} &>> ${logFile} &"
  done
done
