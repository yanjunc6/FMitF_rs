rm -r ./tpcc_out
cargo run --release -- ./examples/tpcc.transact ./tpcc_out --timeout 61920 # 8 hours / 20 cores * 43 jobs