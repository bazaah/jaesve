use failure::Error as fError;
use serde_json::{
    json, Value as JsonValue,
    Value::{
        Array as jArray, Bool as jBool, Null as jNull, Number as jNumber, Object as jObject,
        String as jString,
    },
};
use std::{
    collections::VecDeque,
    fs::File,
    io::{Read, Write},
    result,
};

type FailureResult<T> = result::Result<T, fError>;

// Opens a write stream to either stdout or a file, depending on the user
// If it can't open a file it will attempt to create it
// If it can't create it, will default to stdout
pub fn get_writer(w: Option<&str>, options: &Options) -> Box<Write> {
    let debug_level = options.get_debug_level();
    match w {
        Some(file_name) => match File::create(file_name).ok() {
            Some(file) => Box::new(file),
            None => {
                if *debug_level >= 1 {
                    eprintln!(
                        "Error! Failed to create: {}, switching to stdout...",
                        &file_name
                    )
                }
                Box::new(std::io::stdout())
            }
        },
        None => Box::new(std::io::stdout()),
    }
}

// Either opens from a file or stdin if the "filename" is "-"
pub fn get_reader(r: Option<&str>) -> Result<Box<Read>, String> {
    match r {
        Some("-") => Ok(Box::new(std::io::stdin())),
        Some(file_name) => match File::open(file_name).ok() {
            Some(file) => Ok(Box::new(file)),
            None => Err(format!("Can't open file: {}", &file_name)),
        },
        None => Ok(Box::new(std::io::stdin())),
    }
}

// Puts all the pieces together
pub fn to_csv<R: Read, W: Write>(
    options: &Options,
    input: R,
    output: W,
) -> FailureResult<JsonValue> {
    let data: JsonValue = serde_json::from_reader(input)?;
    let packet = JsonPacket::new(data);
    packet.print(options, output);

    Ok(json!(0))
}

// Function that writes the formatted output to the writer
// The work-horse of the rebel fleet
// If something goes wrong, writes the error to stderr and moves on
fn write<W: Write>(options: &Options, mut output: W, entry: &str, val: Option<&JsonValue>) {
    let separator = options.get_separator();
    let show_type = options.type_status();
    let value = match val {
        Some(jObject(_)) => jString("".to_string()),
        Some(jArray(_)) => jString("".to_string()),
        Some(jString(s)) => jString(s.to_string()),
        Some(jNumber(n)) => jString(n.to_string()),
        Some(jBool(b)) => jString(b.to_string()),
        Some(jNull) => jString("NULL".to_string()),
        None => jString("NO_VALUE".to_string()),
    };
    let mut formated_output = String::new();

    if *show_type {
        let type_of = match val {
            Some(val) => match val {
                jObject(_) => "Map",
                jArray(_) => "Array",
                jString(_) => "String",
                jNumber(_) => "Number",
                jBool(_) => "Bool",
                jNull => "Null",
            },
            None => "NO_TYPE",
        };
        let fmt = format!(
            "\"{}\"{}\"{}\"{}{}",
            entry, separator, type_of, separator, value
        );
        formated_output.push_str(&fmt);
    } else {
        let fmt = format!("\"{}\"{}{}", entry, separator, value);
        formated_output.push_str(&fmt);
    }
    writeln!(output.by_ref(), "{}", formated_output.as_str())
        .map_err(|e| eprintln!("An error occurred while writing: {}", e))
        .unwrap_or(())
}

// Small function for formatting any error (chains) failureResult catches
pub fn formated_error(err: &::failure::Error) -> String {
    let mut format = err.to_string();
    let mut prev = err.as_fail();
    while let Some(next) = prev.cause() {
        format.push_str(": ");
        format.push_str(&next.to_string());
        prev = next;
    }
    format
}

// Struct for holding the options that affect program logic
// Only passes out references
pub struct Options {
    show_type: bool,
    separator: String,
    debug_level: i32,
}

impl Options {
    pub fn new(show_type: bool, separator: String, debug_level: i32) -> Self {
        Options {
            show_type,
            separator,
            debug_level,
        }
    }

    pub fn get_separator(&self) -> &str {
        &self.separator
    }

    pub fn type_status(&self) -> &bool {
        &self.show_type
    }

    pub fn get_debug_level(&self) -> &i32 {
        &self.debug_level
    }
}

// Struct for creating and holding a list of json pointers
// for arbitrary JsonValues
struct JsonPacket {
    object: JsonValue,
    plist: Vec<String>,
}

impl JsonPacket {
    pub fn new(object: JsonValue) -> Self {
        let plist = JsonPacket::parse_json(&object);
        JsonPacket { object, plist }
    }

    // Convenience function around write that allows for clearer flow
    pub fn print<W: Write>(&self, options: &Options, mut output: W) {
        for entry in &self.plist {
            let data = self.object.pointer(&entry);
            write(options, output.by_ref(), entry, data);
        }
    }

    // Unwinds the JsonValue, growing a Vec for every endpoint it finds
    // While queueing any maps or arrays for unwinding
    fn parse_json(json_value: &JsonValue) -> Vec<String> {
        let mut list: Vec<String> = Vec::new();
        let mut jqueue: VecDeque<(&JsonValue, String)> = VecDeque::new();
        jqueue.push_back((json_value, String::default()));

        loop {
            let value = jqueue.pop_front();
            match value {
                Some((jObject(map), ref s)) => {
                    for (k, v) in map.iter() {
                        let new_path = s.clone() + "/" + k;
                        if v.is_object() {
                            list.push(new_path.clone());
                        }
                        if v.is_array() {
                            list.push(new_path.clone());
                        }
                        jqueue.push_back((v, new_path));
                    }
                }
                Some((jArray(a), ref s)) => {
                    for (i, v) in a.iter().enumerate() {
                        let new_path = s.clone() + "/" + &i.to_string();
                        jqueue.push_back((v, new_path));
                    }
                }
                Some((jString(_), s)) => list.push(s),
                Some((jNumber(_), s)) => list.push(s),
                Some((jBool(_), s)) => list.push(s),
                Some((jNull, s)) => list.push(s),
                None => break,
            }
        }
        list
    }
}
