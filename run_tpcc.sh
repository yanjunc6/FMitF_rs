rm -r ./tpcc_out
# cargo run --release -- ./examples/tpcc.transact ./tpcc_out --timeout 61920 # (17.2 hours) 8 hours / 20 cores * 43 jobs
# cargo run --release -- ./examples/tpcc.transact ./tpcc_out --timeout 600 # (10 min)
cargo run --release -- ./examples/tpcc.transact ./tpcc_out --timeout 10 # (10 sec)