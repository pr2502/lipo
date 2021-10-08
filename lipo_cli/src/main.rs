use anyhow::Context;
use lipo::builtins;
use lipo::error::Report;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opts {
    /// Input file
    #[structopt(parse(from_os_str), default_value = "-")]
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::from_args();
    pretty_env_logger::init();

    let input = if opts.file == Path::new("-") {
        let mut buf = String::new();
        let stdin = io::stdin();
        stdin.lock().read_to_string(&mut buf)
            .context("reading stdin")?;
        buf
    } else {
        let mut file = File::open(&opts.file)
            .with_context(|| format!("opening file {:?}", &opts.file))?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)
            .with_context(|| format!("reading file {:?}", &opts.file))?;
        buf
    };

    let alloc = lipo::Alloc::new();
    let src = builtins::String::new_owned(input.into(), &alloc);

    let ast = match lipo::parse(src) {
        Ok(ast) => ast,
        Err(err) => {
            err.report(&src);
            return Ok(());
        }
    };

    let script = match lipo::compile(ast, &alloc) {
        Ok(script) => script,
        Err(errs) => {
            for err in errs {
                err.report(&src);
            }
            return Ok(());
        }
    };

    let vm = lipo::VM::new(script, &alloc);

    if let Err(err) = vm.run() {
        err.report(&src);
    }

    Ok(())
}
