# rm -r ./tpcc_out
cargo run --release -- ./examples/tpcc.transact ./data_out/tpcc_out/61920 --timeout 61920 --disable-split --clear-cache # (17.2 hours) 8 hours / 20 cores * 43 jobs
cargo run --release -- ./examples/tpcc.transact ./data_out/tpcc_out/600 --timeout 600 --disable-split --clear-cache # (10 min)
cargo run --release -- ./examples/tpcc.transact ./data_out/tpcc_out/10 --timeout 10 --disable-split --clear-cache # (10 sec)
cargo run --release -- ./examples/social.transact ./data_out/social_out --disable-split --clear-cache 
cargo run --release -- ./examples/rubis.transact ./data_out/rubis_out --disable-split --clear-cache 