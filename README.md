# Jaesve

A CLI utility written in pure Rust for converting JSON objects to a series of CSV values, from stdin and/or file(s) to stdout or a file.

## Installation

- Install rust - minimum version: 2018 Stable (v1.30)
- Clone this repository
- Run `cargo run --release -- -h` for usage information

### CLI

Jaesve comes with a CLI, courtesy of clap.rs. You can type `-h` or `--help` to see the available settings.

#### Flags

- `-h` `--help` Brings up the cli
- `-V` `--version` Displays version information
- `-t` `--type` Don't display the type of each JSON value
- `-v` Verbosity of debug information (max: 3)

#### Options

- `-i` `--input` Set input file(s) to be read from (NOTE: Files are processed in the order you enter them)
- `-o` `--output` Set output file to write (NOTE: Jaesve defaults to stdout)
- `-s` Sets the separator: either a comma (","), comma-space (", ") or a tab ("ACSII: \09"), or an arbitrary, user designated separator

#### Experimental

These features are currently undergoing testing and should be used with care.

- `-l` `--line` Sets stdin to read one JSON object per line
- `-x` `--regex` Set a regex for filtering output

### Example Usage

For a simple example, let's use the following JSON:

```json
//sample.json
{
  "aliceblue": "#f0f8ff",
  "antiquewhite": "#faebd7",
  "aqua": "#00ffff",
  "aquamarine": "#7fffd4",
  "azure": "#f0ffff",
  "beige": "#f5f5dc",
  "bisque": "#ffe4c4",
  "black": "#000000",
  "blanchedalmond": "#ffebcd",
  "blue": "#0000ff",
  "blueviolet": "#8a2be2",
  "brown": "#a52a2a",
  "phone numbers": ["+44 1234567", "+44 2345678"],
  "gradient": {
    "blues": ["#0000f0", "#0000f1", "#0000f2"],
    "green": "#00ff00"
  },
  "a number": 42,
  "a bool": true,
  "weird": "NaN"
}
```

Typing `cargo run --release -- -ti sample.json` will output:

```bash
"/phone number", ""
"/gradient", ""
"/a bool", true
"/a number", 42
"/aliceblue", "#f0f8ff"
"/antiquewhite", "#faebd7"
"/aqua", "#00ffff"
"/aquamarine", "#7fffd4"
"/azure", "#f0ffff"
"/beige", "#f5f5dc"
"/bisque", "#ffe4c4"
"/black", "#000000"
"/blanchedalmond", "#ffebcd"
"/blue", "#0000ff"
"/blueviolet", "#8a2be2"
"/brown", "#a52a2a"
"/gradient/blues", ""
"/weird", "NaN"
"/phone number/0", "+44 1234567"
"/phone number/1", "+44 2345678"
"/gradient/green", "#00ff00"
"/gradient/blues/0", "#0000f0"
"/gradient/blues/1", "#0000f1"
"/gradient/blues/2", "#0000f2"
```

Why are some entries like 'phone number' or 'gradient' empty? Because they are not endpoints, they could contain other k:v pairs. Note that by default jaesve will include type information, which we removed with `-t`.

For another example, let's say that you're only interested in finding numbers, and you want to read from stdin. After some thought you type: `cat some/path/sample.json | cargo run --release | grep Number` which outputs:

```bash
"/a number", Number, 42
```

What happened here? `cat` sent to jaesve's stdin which was auto detected due to no `-i` and because we were only looking for JSON numbers (not numbers inside strings) we send it off to `grep` to filter out everything that is not a `Number`.

### Errors

Jaesve prints any errors to stderr if `-v` is set (with escalating information on `-vv` or `-vvv`), otherwise it will fail silently to avoid messing with pipes and handles.

It will error for the following conditions:

1. JSON object is malformed
2. Could not read from input file(s)
3. Could not create output file (It will not create directories)
4. Could not write to output

If any of these occur it will log an error and attempt to continue

#### Bugs

The above errors are expected to be handled by the user, if other errors occur they are considered bugs and I'd appreciate it if you'd open an issue with a description of what went wrong and the error message you received.
