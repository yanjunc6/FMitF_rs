#!/bin/bash

# Number of Shards
nodes=${1:-4}
# Concurrency control Mode
# 1=2PC, 2=IC3
mode=${2:-0}
# Number of concurrent requests
con=${3:-10}
# Number of items, default 100000
numI=${4:-100000}
filePrefix=${5:-""}
verbose="-v"
PREFIX="192.168.20."
ClientAddr=${PREFIX}$((nodes+2))
godir="/usr/local/go/bin/go"

./reset_shards.sh ${nodes} ${mode} ${verbose}

sleep 1
logFile="log/${filePrefix}result.txt"
go run ../runTPCC.go --config=../node --mode=${mode} --id=0 --rate=${con} --item=${numI} ${verbose}