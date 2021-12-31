use std::collections::HashSet;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use anyhow::Context;
use lipo::error::Report;
use lipo::{builtins, ObjectRef};
use structopt::StructOpt;


fn init_tracing() {
    use tracing::Level;
    use tracing_subscriber::{EnvFilter, FmtSubscriber};

    FmtSubscriber::builder()
        .without_time()
        .with_max_level(Level::TRACE)
        .with_env_filter(EnvFilter::from_default_env())
        .init();
}

#[derive(StructOpt)]
struct Opts {
    /// Input file
    #[structopt(parse(from_os_str), default_value = "-")]
    file: PathBuf,

    // Debugging options
    /// Pretty print successfully parsed AST
    #[structopt(long)]
    dbg_ast: bool,

    /// Pretty print successfully compiled bytecode
    #[structopt(long)]
    dbg_bytecode: bool,
}

fn pretty_print_bytecode<'alloc>(fun: ObjectRef<'alloc, builtins::Function<'alloc>>) {
    fn recur<'alloc>(
        fun: ObjectRef<'alloc, builtins::Function<'alloc>>,
        printed: &mut HashSet<*const builtins::Function<'alloc>>,
    ) {
        eprintln!("{} = {:?}", fun.name, fun.chunk);
        for constant in fun.chunk.constants() {
            if let Some(fun) = constant.downcast::<builtins::Function>() {
                if printed.insert(&*fun as *const _) {
                    recur(fun, printed);
                }
            }
        }
    }
    recur(fun, &mut HashSet::default());
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::from_args();
    init_tracing();

    let input = if opts.file == Path::new("-") {
        let mut buf = String::new();
        let stdin = io::stdin();
        stdin
            .lock()
            .read_to_string(&mut buf)
            .context("reading stdin")?;
        buf
    } else {
        let mut file =
            File::open(&opts.file).with_context(|| format!("opening file {:?}", &opts.file))?;
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
        },
    };

    if opts.dbg_ast {
        eprintln!("\nast = {:#?}\n", &ast);
    }

    let script = match lipo::compile(ast, &alloc) {
        Ok(script) => script,
        Err(errs) => {
            for err in errs {
                err.report(&src);
            }
            return Ok(());
        },
    };

    if opts.dbg_bytecode {
        pretty_print_bytecode(script);
    }

    let vm = lipo::VM::new(script, &alloc);

    if let Err(err) = vm.run() {
        err.report(&src);
    }

    Ok(())
}
