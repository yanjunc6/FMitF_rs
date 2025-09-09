use FMitF_rs::cli;

fn main() {
    if let Err(e) = cli::run_cli() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
