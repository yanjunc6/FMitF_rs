use FMitF_rs::cli;

fn main() {
    if let Err(_) = cli::run_cli() {
        std::process::exit(1);
    }
}
