#!/bin/bash

# Number of Shards
nodes=${1:-4}
# Concurrency control Mode
# 1=2PC, 2=IC3
mode=${2:-0}
# Number of concurrent requests
con=${3:-10}

verbose="-v"

./reset_shards.sh ${nodes} ${mode} ${verbose} -p

sleep 1
go run ../runRubis.go --config=../node --mode=${mode} --id=0 --rate=${con} ${verbose}