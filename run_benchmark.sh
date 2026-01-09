# rm -r ./tpcc_out
cargo run --release -- ./examples/tpcc.transact ./tpcc_out/61920 --timeout 61920 # (17.2 hours) 8 hours / 20 cores * 43 jobs
cargo run --release -- ./examples/tpcc.transact ./tpcc_out/600 --timeout 600 # (10 min)
cargo run --release -- ./examples/tpcc.transact ./tpcc_out/10 --timeout 10 # (10 sec)
cargo run --release -- ./examples/social.transact ./social_out
cargo run --release -- ./examples/rubis.transact ./rubis_out