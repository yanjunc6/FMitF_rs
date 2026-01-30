#!/bin/bash
# Number of Shards
nodes=${1:-2}
# Concurrency control Mode
# 1=2PC, 2=IC3, 3=DIC3, 4=IC3+Ordered2PL, 5 = IC3+MessagePulling
mode=${2:-1}
logFileName=${3:-"log"}
rm -f "${logFileName}.txt"
touch "${logFileName}.txt"

contention="10 100 500 1000 10000"
for SH in $contention; do
  echo "Running on ${SH} items"
  ./exp.sh "${nodes}" "${mode}" 64 "${SH}" | tee -a "${logFileName}.txt"
  sleep 1
done