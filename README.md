# Jaesve

A CLI utility written in pure Rust for stream converting JSON objects to a series of CSV values, from stdin and/or file(s) to stdout or a file.

## Installation

- Install [rust](https://www.rust-lang.org/tools/install)
  1. Install from cargo
      - `cargo install jaesve`
  2. Install from the repo
      - Run `rustup default stable`
      - Run `git clone https://github.com/bazaah/jaesve.git; cd jaesve`
      - Run `cargo run --release`
      - The binary can be found in `target/release/`

### CLI

Jaesve comes with a CLI, courtesy of [clap.rs](https://github.com/clap-rs/clap). You can type `-h` or `--help` to see the available settings, or browse a complete listing below.

#### Flags

- `-h` `--help` Displays help (use `--help` for more detailed messages)
- `-V` `--version` Displays version information
- `-a` `--append` Append to output file, instead of overwriting
- `-l` `--line` Set stdin to read a JSON doc from each line
- `-q` `--quiet` Silences error messages
- `-v` Verbosity of debug information
  - Max: 3

#### Options

- `-o` `--output` Set output file to write
  - Default: writes to stdout
- `-E` `--regex` Set a regex to filter output
- `-c` `--column` Sets field to match regex on
  - Possible: `ident, jptr, type, value, jmes`
- `-f` `--format` A dot '.' separated list of fields describing how output is formatted
  - Default: `ident.jptr.type.value`
  - Possible: `ident, jptr, type, value, jmes`
- `-d` `--delim` Sets delimiter between output fields
  - Default: `,`
- `-g` `--guard` Set field quote character
  - Default: `"`

#### SubCommands

- `config` Configure various program intrinsics
  - These parameters are **unstable** and may change in the future
  - `config --help` for the current list

#### Args

- A space separated list of valid file paths, with a `-` representing stdin. If you wish to add flags and options after this Arg, you must end its values with a `:`.
  - For example:
    - `jaesve --quiet --append -o output.csv input1 input2 - input4 // This is happy`
    - `jaesve input1 input2 - input4 --quiet --append -o output.csv // This is sad`
    - `jaesve input1 input2 - input4 : --quiet --append -o output.csv // This is happy`

### Performance

#### Speed

In preliminary tests it parsed 2G of JSON in 3 minutes.

#### Memory

Jaesve is written to minimize memory usage. It uses a stream based approach to parsing JSON, and attempts to unroll nested objects. Its maximum memory footprint can be described as follows:

- `sizeof largest object/array` +
- `sizeof combined elements NOT including any object/array from doc start to largest object/array` +
- `program overhead`

TLDR: the more deeply nested and larger the objects/arrays, the larger the memory footprint.

### Example Usage

For a simple example, let's use the following JSON:

```json
//sample.json
{
  "aliceblue": "#f0f8ff",
  "antiquewhite": "#faebd7",
  "azure": "#f0ffff",
  "beige": "#f5f5dc",
  "black": "#000000",
  "blanchedalmond": "#ffebcd",
  "gradient": {
    "blues": ["#0000f0", "#0000f1", "#0000f2"],
    "green": "#00ff00"
  }
}
```

Running `jaesve sample.json` prints out

```csv
"1","/gradient/blues/0","String","#0000f0"
"1","/gradient/blues/1","String","#0000f1"
"1","/gradient/blues/2","String","#0000f2"
"1","/gradient/green","String","#00ff00"
"1","/gradient/blues","Array",""
"1","/aliceblue","String","#f0f8ff"
"1","/antiquewhite","String","#faebd7"
"1","/azure","String","#f0ffff"
"1","/beige","String","#f5f5dc"
"1","/black","String","#000000"
"1","/blanchedalmond","String","#ffebcd"
"1","/gradient","Object",""
```

Where:

- `"INTEGER"` is which input source the value came from
- `"/.../..."` is the [json pointer](https://tools.ietf.org/html/rfc6901) of that record
- `"JSON TYPE"` is the record type
- `VALUE` is the value associated with that record, if it is an endpoint i.e not a Object or Array

### Errors

Jaesve prints any errors to stderr unless `--quiet` is set with escalating information on `-v`, `-vv` and `-vvv`.

It will error for the following conditions:

1. JSON object is malformed / contains invalid unicode points
2. Could not read from input file(s)
3. Could not create output file (It will not create directories)
4. Could not write to output

#### Bugs

The above errors are expected to be handled by the user, if other errors occur they are considered bugs and I'd appreciate it if you'd open an issue with a description of what went wrong and the error message you received.
