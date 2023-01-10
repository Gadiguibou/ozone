use std::cmp::Ordering;

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();

    match args.len().cmp(&2) {
        Ordering::Equal => ozone::run_file(&args[1]),
        Ordering::Less => ozone::run_prompt(),
        Ordering::Greater => {
            println!("Usage: nox [file]");
            std::process::exit(64);
        }
    }
}
