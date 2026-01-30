#!/bin/bash

nodes=${1:-4}
Re=${2:-2} # If Re == 1, stop containers instead

num=$((nodes)) # Add one for client node
sudo ./unset_ovs.sh $num
./kill_containers.sh
sleep 2
if [ $Re -eq 1 ]; then
	exit 0
fi

./start_containers.sh $num
sleep 2
sudo ./set_ovs.sh $num

PREFIX="192.168.20."
dir=$(dirname "$0")

ClientAddr=${PREFIX}$((nodes+2))

tomlFile="${dir}/../node.toml"
rm -f ${tomlFile}
touch ${tomlFile}

echo "shard-number = ${nodes}" >> ${tomlFile}

echo '[[clients]]' >> ${tomlFile}
echo "client-id = 0" >> ${tomlFile}
echo "client-addr = \"${ClientAddr}:10000\"" >> ${tomlFile}

for (( nodeId=0; nodeId<nodes; nodeId++ )); do
  addr=$PREFIX$((nodeId + 2))

  for (( districtId=0; districtId < 10; districtId++ )); do
    did=$((nodeId * 10 + districtId))
    port=$((10000 + districtId))
    echo '[[shards]]' >> ${tomlFile}
    echo "shard-id = ${did}" >> ${tomlFile}
    echo "shard-addr = \"${addr}:${port}\"" >> ${tomlFile}
  done
done

for (( nodeId=0; nodeId<nodes; nodeId++ )); do
  addr=$PREFIX$((nodeId + 2))
  ssh -o StrictHostKeyChecking=no root@${addr} "cp -r /root/ShardDB1/* /root/ShardDB/"
  scp -o StrictHostKeyChecking=no ${tomlFile} root@${addr}:/root/ShardDB/node.toml
done

./add_latency_all.sh "$nodes"
