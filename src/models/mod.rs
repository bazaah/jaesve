use {
    crate::{
        cli::ProgramArgs,
        match_with_log,
        models::{
            assets::{IdentifyFirstLast, ReadFrom, ReadKind, RegexOptions},
            builder::{Builder, Output},
            error::{ErrorKind, Result},
            pointer::{Pointer, PointerKind},
            scan::JsonScan,
            field::Field
        },
        CLI,
    },
    simplelog::*,
    std::{
        fs::{File, OpenOptions},
        io::{stdin as cin, stdout as cout, Result as ioResult, Write as ioWrite},
        path::PathBuf,
        str::from_utf8,
        sync::mpsc::SyncSender,
    },
};

pub mod assets;
pub mod builder;
pub mod error;
pub mod field;
pub mod pointer;
pub mod scan;

/// Type def for the reader -> builder channel
pub type ToBuilder = (Option<usize>, Option<PointerKind>, Option<Vec<u8>>);
/// Type def for the builder -> writer channel
pub type ToWriter = Output;

/// Determines write destination from runtime args
// w: (_, bool), true => append, false => create
pub fn get_writer(w: &(Option<String>, bool)) -> Box<dyn ioWrite> {
    match w {
        (Some(file_name), false) => match_with_log!(
            match File::create(file_name).ok() {
                Some(file) => match_with_log!(Box::new(file), info!("Success!")),
                None => match_with_log!(Box::new(cout()), warn!("Failed! Switching to stdout...")),
            },
            info!("Attempting to create {}...", file_name)
        ),
        (Some(file_name), true) => match_with_log!(
            match OpenOptions::new().append(true).open(file_name) {
                Ok(file) => match_with_log!(Box::new(file), info!("Success!")),
                Err(e) => match_with_log!(
                    Box::new(cout()),
                    warn!("Unable to open file: {}, switching to stdout...", e)
                ),
            },
            info!("Attempting to append to {}...", file_name)
        ),
        (None, _) => match_with_log!(
            Box::new(cout()),
            info!("No output file detected, defaulting to stdout...")
        ),
    }
}

/// Helper function for generating a list of read sources at runtime
pub fn get_reader(r: Option<&str>) -> Option<ReadFrom> {
    match r {
        Some("-") => Some(ReadFrom::Stdin),
        Some(file_name) => {
            let path = PathBuf::from(file_name);
            if path.is_file() {
                Some(ReadFrom::File(path))
            } else {
                None
            }
        }
        None => Some(ReadFrom::Stdin),
    }
}

/// Opens a read source, defaults to stdin if source errors
pub fn set_reader(src: &Option<ReadFrom>) -> ReadKind {
    match src {
        Some(s) => match s {
            ReadFrom::File(path) => match_with_log!(
                match File::open(path) {
                    Ok(f) => match_with_log!(ReadKind::File(f), info!("Success! ({:?})", path)),
                    Err(e) => match_with_log!(
                        ReadKind::Stdin(cin()),
                        warn!("Failed! {}, switching to stdin... ({:?})", e, path)
                    ),
                },
                info!("Attempting to read from {:?}...", path)
            ),
            ReadFrom::Stdin => {
                match_with_log!(ReadKind::Stdin(cin()), info!("Reading from stdin..."))
            }
        },
        None => match_with_log!(
            ReadKind::Stdin(cin()),
            info!("No input source found, defaulting to stdin...")
        ),
    }
}

/// Handles crate log initialization, potentially including
/// user defined writer sources, if any source is unreachable
/// it is ignored
pub fn initialize_logging(opts: &ProgramArgs) {
    // Collects any failures for logging
    let mut skipped: Vec<String> = Vec::new();
    match opts.logger() {
        Some(set) if set.contains("-") && set.len() == 1 => {
            TermLogger::init(opts.debug_level(), Config::default(), TerminalMode::Stderr)
                .unwrap_or_else(|_| {
                    skipped.push(format!(
                        "No TERM variable found, defaulting to fallback logger"
                    ));
                    SimpleLogger::init(opts.debug_level(), Config::default()).unwrap()
                })
        }
        Some(set) => CombinedLogger::init(
            set.iter()
                .filter_map(|path| match path.as_str() {
                    "-" => Some(None),
                    path => OpenOptions::new()
                        .append(true)
                        .create(true)
                        .open(path)
                        .map_err(|e| {
                            skipped.push(format!(
                                "Failed to open '{}' reason: {}... ignoring",
                                path, e
                            ))
                        })
                        .ok()
                        .map(|f| Some(f)),
                })
                .map(|path| -> Box<dyn SharedLogger> {
                    match path {
                        Some(file) => WriteLogger::new(opts.debug_level(), Config::default(), file),
                        None => TermLogger::new(
                            opts.debug_level(),
                            Config::default(),
                            TerminalMode::Stderr,
                        )
                        .unwrap(),
                    }
                })
                .collect(),
        )
        .unwrap_or_else(|_| {
            skipped.push(format!("Failed to start logger(s), using fallback"));
            SimpleLogger::init(opts.debug_level(), Config::default()).unwrap()
        }),
        None => TermLogger::init(opts.debug_level(), Config::default(), TerminalMode::Stderr)
            .unwrap_or_else(|_| {
                skipped.push(format!(
                    "No TERM variable found, defaulting to fallback logger"
                ));
                SimpleLogger::init(opts.debug_level(), Config::default()).unwrap()
            }),
    }
    info!("<---- PROGRAM START ---->");
    if !skipped.is_empty() {
        for failed in skipped {
            warn!("{}", failed)
        }
    }
}

/// Entry function for the JSON stream parser,
/// handles both recursive and single item docs
pub fn unwind_json<I>(
    opts: &ProgramArgs,
    ident: Option<usize>,
    source: Option<I>,
    channel: SyncSender<ToBuilder>,
) -> Result<()>
where
    I: Iterator<Item = ioResult<u8>>,
{
    debug!("Started parsing a JSON doc");
    let pointer = eval_raw(|opts, _| PointerKind::new(opts), ());
    let mut maybe_scanner = source.map(|src| JsonScan::new(src));
    match maybe_scanner.as_mut() {
        Some(scanner) => loop {
            match scanner.next() {
                Some(Ok(b @ b'[')) => {
                    unwind_recursive(opts, ident, scanner, pointer.clone(), channel.clone(), b)?;
                    continue;
                }
                Some(Ok(b @ b'{')) => {
                    unwind_recursive(opts, ident, scanner, pointer.clone(), channel.clone(), b)?;
                    continue;
                }
                Some(Ok(b @ b'-')) | Some(Ok(b @ b'0'..=b'9')) => {
                    unwind_single(opts, ident, scanner, b, channel.clone())?
                }
                Some(Ok(b @ b't')) | Some(Ok(b @ b'f')) => {
                    unwind_single(opts, ident, scanner, b, channel.clone())?
                }
                Some(Ok(b @ b'n')) => unwind_single(opts, ident, scanner, b, channel.clone())?,
                Some(Ok(b @ b'"')) => unwind_single(opts, ident, scanner, b, channel.clone())?,
                Some(Ok(_)) => continue,
                Some(Err(e)) => return Err(e.into()),
                None => break,
            }
        },
        None => {
            channel
                .send((ident, PointerKind::new(opts), None))
                .map_err(|_| {
                    ErrorKind::UnexpectedChannelClose(format!(
                        "builder in |reader -> builder| channel has hung up"
                    ))
                })?;

            drop(channel);
        }
    }

    debug!("Finished parsing a JSON doc");
    Ok(())
}
/// Recursively unwinds a JSON data stream
/// sending the pieces to a builder thread
// This function does not check if the stream is valid JSON,
// if it isn't the deserializer will catch it,
// but the error it emits might be cryptic depending on
// how badly this function mangled it
pub fn unwind_recursive<I>(
    opts: &ProgramArgs,
    ident: Option<usize>,
    scanner: &mut JsonScan<I>,
    jptr: Option<PointerKind>,
    channel: SyncSender<ToBuilder>,
    prefix_byte: u8,
) -> Result<()>
where
    I: Iterator<Item = ioResult<u8>>,
{
    // Handle the '{' or '[' byte that the outside function might have
    let mut buffer: Vec<u8> = vec![prefix_byte];
    let mut array_count = 0usize;
    trace!("BEFORE: ({:?}, {:?})", &jptr, from_utf8(&buffer));
    loop {
        match scanner.next() {
            Some(Ok(b @ b'[')) if scanner.outside_quotes() => {
                unwind_recursive(
                    opts,
                    ident,
                    scanner,
                    match prefix_byte {
                        b'[' => jptr.as_ref().map(|ptr| ptr.clone_extend(array_count)),
                        b'{' => jptr
                            .as_ref()
                            .map(|ptr| {
                                from_utf8(&calculate_key(&buffer, scanner.offsets()))
                                    .map(|s| ptr.clone_extend(s))
                            })
                            .transpose()?,
                        _ => unreachable!(),
                    },
                    channel.clone(),
                    b,
                )?;
                buffer.push(b);
                // Recursive call above eats the corresponding ']' replace it, creating an empty array
                buffer.push(b']');
            }
            Some(Ok(b @ b'{')) if scanner.outside_quotes() => {
                unwind_recursive(
                    opts,
                    ident,
                    scanner,
                    match prefix_byte {
                        b'[' => jptr.as_ref().map(|ptr| ptr.clone_extend(array_count)),
                        b'{' => jptr
                            .as_ref()
                            .map(|ptr| {
                                from_utf8(&calculate_key(&buffer, scanner.offsets()))
                                    .map(|s| ptr.clone_extend(s))
                            })
                            .transpose()?,
                        _ => unreachable!(),
                    },
                    channel.clone(),
                    b,
                )?;
                buffer.push(b);
                // Recursive call above eats the corresponding '}' replace it, creating an empty map
                buffer.push(b'}');
            }
            Some(Ok(b @ b']')) | Some(Ok(b @ b'}')) if scanner.outside_quotes() => {
                buffer.push(b);
                break;
            }
            Some(Ok(b @ b',')) => {
                array_count += 1;
                buffer.push(b)
            }
            Some(Ok(b)) => buffer.push(b),
            Some(Err(e)) => return Err(e.into()),
            None => break,
        }
    }

    channel.send((ident, jptr, Some(buffer))).map_err(|_| {
        ErrorKind::UnexpectedChannelClose(format!(
            "builder in |reader -> builder| channel has hung up"
        ))
    })?;

    drop(channel);
    Ok(())
}

/// Sends the entire read stream to the builder
/// Only called if the doc is not a object or array
pub fn unwind_single<I>(
    opts: &ProgramArgs,
    ident: Option<usize>,
    scanner: &mut JsonScan<I>,
    prefix_byte: u8,
    channel: SyncSender<ToBuilder>,
) -> Result<()>
where
    I: Iterator<Item = ioResult<u8>>,
{
    let buffer: Result<Vec<u8>> = [prefix_byte]
        .iter()
        .map(|b| Ok(*b))
        .chain(scanner)
        .map(|res| res.map_err(|e| e.into()))
        .collect();

    channel
        .send((ident, PointerKind::new(opts), Some(buffer?)))
        .map_err(|_| {
            ErrorKind::UnexpectedChannelClose(format!(
                "builder in |reader -> builder| channel has hung up"
            ))
        })?;

    Ok(())
}

/// Helper function for finding the key
/// of the JSON object being unwound
fn calculate_key(buffer: &[u8], offsets: (usize, usize)) -> Vec<u8> {
    let (in_quotes, out_quotes) = offsets;
    let mut key: Vec<_> = buffer
        .iter()
        .rev()
        .skip(out_quotes)
        .take(in_quotes)
        .copied()
        .collect();
    key.reverse();

    trace!("KEY: {:?}", from_utf8(&key));

    key
}

/// Specialized writer function for Output using Builder
pub fn write_formatted_output<B, W>(w: &mut W, blocks: B, blueprint: &[Field]) -> Result<()>
where
    B: Builder<Field>,
    W: ioWrite,
    ErrorKind: From<<B as Builder<Field>>::Error>,
{
    let iter = blueprint.iter().identify_first_last();
    for (_, last, field) in iter {
        write!(
            w,
            "{}{}{}",
            blocks.guard()?,
            blocks.build_with(*field)?,
            blocks.guard()?
        )?;
        if !last {
            write!(w, "{}", blocks.delimiter()?)?;
        }
    }
    writeln!(w, "")?;

    Ok(())
}

/// Helper function for early parse skipping, based on input ident
pub fn check_index(regex: Option<&RegexOptions>, ident: Option<usize>) -> bool {
    ident.map_or(true, |i| match regex {
        Some(regex) if regex.is_ident() => regex.pattern().is_match(&i.to_string()),
        _ => true,
    })
}

/// Helper function for abstracting the dependency check out of
/// the individual business logic
pub fn eval<T, C, F, A>(check: &C, f: F, arg: A) -> T
where
    F: Fn(bool, A) -> T,
    C: AsRef<Field>,
{
    f(CLI.should_calculate(check.as_ref()), arg)
}

pub fn eval_raw<T, F, A>(f: F, arg: A) -> T
where
    F: Fn(&ProgramArgs, A) -> T,
{
    f(&CLI, arg)
}
