#!/bin/bash

num=${1}
path1=${2}
path2=${3}

PREFIX="192.168.20."
for (( regionId=0; regionId<num; regionId++ )); do
  addr=$PREFIX$((regionId + 2))
  scp -o StrictHostKeyChecking=no "${path1}" root@${addr}:"/root/ShardDB/${path2}"
done