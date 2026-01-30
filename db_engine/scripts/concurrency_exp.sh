#!/bin/bash
# Number of Shards
nodes=${1:-2}
# Concurrency control Mode
# 1=2PC, 2=IC3, 3=DIC3, 4=IC3+Ordered2PL, 5 = IC3+MessagePulling
mode=${2:-1}
logFileName=${3:-"log"}
rm -f "${logFileName}.txt"
touch "${logFileName}.txt"

concurrency="1 4 8 16 32 64 128"
for SH in $concurrency; do
  echo "Running on ${SH} concurrency"
  ./expRubis.sh "${nodes}" "${mode}" "${SH}" | tee -a "${logFileName}.txt"
  sleep 1
done