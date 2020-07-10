#![forbid(unsafe_code)]
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;
use std::{char, u16};

pub type JSONResult = Result<JSONValue, ParseError>;
pub type JSONArray = Vec<JSONValue>;
pub type JSONMap = HashMap<String, JSONValue>;

#[derive(Clone, Debug, PartialEq)]
pub enum JSONValue {
    Object(HashMap<String, JSONValue>),
    Array(Vec<JSONValue>),
    String(String),
    Number(f64),
    True,
    False,
    Null,
}

impl JSONValue {
    pub fn unwrap<T: TryFrom<JSONValue>>(self) -> T {
        match T::try_from(self) {
            Ok(val) => val,
            Err(_) => panic!("Tried to unwrap an empty value"),
        }
    }
}

impl TryFrom<JSONValue> for JSONMap {
    type Error = &'static str;
    fn try_from(v: JSONValue) -> Result<Self, Self::Error> {
        match v {
            JSONValue::Object(o) => Ok(o),
            _ => Err("Invalid type conversion"),
        }
    }
}

impl TryFrom<JSONValue> for JSONArray {
    type Error = &'static str;
    fn try_from(v: JSONValue) -> Result<Self, Self::Error> {
        match v {
            JSONValue::Array(a) => Ok(a),
            _ => Err("Invalid type conversion"),
        }
    }
}

impl TryFrom<JSONValue> for f64 {
    type Error = &'static str;
    fn try_from(v: JSONValue) -> Result<Self, Self::Error> {
        match v {
            JSONValue::Number(n) => Ok(n),
            _ => Err("Invalid type conversion"),
        }
    }
}

impl TryFrom<JSONValue> for String {
    type Error = &'static str;
    fn try_from(v: JSONValue) -> Result<Self, Self::Error> {
        match v {
            JSONValue::String(s) => Ok(s),
            _ => Err("Invalid type conversion"),
        }
    }
}

impl TryFrom<JSONValue> for bool {
    type Error = &'static str;
    fn try_from(v: JSONValue) -> Result<Self, Self::Error> {
        match v {
            JSONValue::True => Ok(true),
            JSONValue::False => Ok(false),
            _ => Err("Invalid type conversion"),
        }
    }
}

impl From<JSONValue> for () {
    fn from(_: JSONValue) -> () {
        ()
    }
}

impl From<JSONMap> for JSONValue {
    fn from(val: JSONMap) -> JSONValue {
        Self::Object(val)
    }
}

impl From<JSONArray> for JSONValue {
    fn from(val: JSONArray) -> JSONValue {
        Self::Array(val)
    }
}

impl From<f64> for JSONValue {
    fn from(n: f64) -> Self {
        Self::Number(n)
    }
}

impl From<String> for JSONValue {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<&str> for JSONValue {
    fn from(s: &str) -> Self {
        Self::String(String::from(s))
    }
}

impl From<bool> for JSONValue {
    fn from(val: bool) -> Self {
        match val {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl From<()> for JSONValue {
    fn from(_s: ()) -> Self {
        Self::Null
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEndOfInput(String),
    ExpectedEndOfInput(String),
    ExpectedObjectKey(String),
    ExpectedToken(String),
    UnexpectedToken(String),
    ExpectedDigit(String),
    ExpectedEscapeChar(String),
    ExpectedUnicodeEscape(String),
}

#[derive(Debug, PartialEq)]
pub struct JSON {
    chars: Vec<char>,
    i: usize,
}

macro_rules! try_parse {
    ($( $e:expr ),* ) => {
        $(
            if let Some(v) = $e? {
                return Ok(v);
            }
        )*
    };
}

impl JSON {
    fn new(json: &str) -> Self {
        JSON {
            chars: json.chars().collect(),
            i: 0,
        }
    }

    fn parse_value(&mut self) -> JSONResult {
        self.skip_whitespace();
        try_parse!(
            self.parse_string(),
            self.parse_number(),
            self.parse_object(),
            self.parse_array(),
            self.parse_keyword("true", JSONValue::True),
            self.parse_keyword("false", JSONValue::False),
            self.parse_keyword("null", JSONValue::Null)
        );
        Err(ParseError::UnexpectedEndOfInput(format!(
            "Doesn't seem to be valid JSON"
        )))
    }

    fn parse_object(&mut self) -> Result<Option<JSONValue>, ParseError> {
        if self.chars[self.i] != '{' {
            return Ok(None);
        }
        self.increment(1);
        self.skip_whitespace();
        let mut result: JSONMap = HashMap::new();
        let mut initial = true;
        while self.chars[self.i] != '}' {
            self.skip_whitespace();
            if initial == false {
                self.eat(',')?;
                self.skip_whitespace();
            } else {
                self.skip_whitespace();
            }
            let maybe_key = self.parse_string()?;
            if maybe_key.is_none() {
                return Err(ParseError::ExpectedObjectKey(format!(
                    "Expected an object key. Does the object have a trailing comma?"
                )));
            }
            self.skip_whitespace();
            self.eat(':')?;
            let key = maybe_key.unwrap().unwrap();
            let value = self.parse_value()?;
            result.insert(key, value);
            initial = false;
            self.skip_whitespace();
        }
        self.expect_not_end('}')?;
        self.increment(1);
        Ok(Some(JSONValue::from(result)))
    }

    fn parse_array(&mut self) -> Result<Option<JSONValue>, ParseError> {
        if self.chars[self.i] != '[' {
            return Ok(None);
        }
        self.increment(1);
        self.skip_whitespace();
        let mut result: Vec<JSONValue> = vec![];
        let mut initial = true;
        while self.chars[self.i] != ']' {
            self.skip_whitespace();
            if initial == false {
                self.eat(',')?;
            }
            let value = self.parse_value()?;
            result.push(value);
            initial = false;
        }
        self.expect_not_end(']')?;
        self.increment(1);
        Ok(Some(JSONValue::from(result)))
    }

    fn parse_string(&mut self) -> Result<Option<JSONValue>, ParseError> {
        if self.chars[self.i] != '"' {
            return Ok(None);
        }
        self.increment(1);
        let mut result = String::new();
        while self.chars[self.i] != '"' && self.i < self.chars.len() - 1 {
            if self.chars[self.i] == '\\' {
                let ch = self.chars[self.i + 1];
                if ch == '"' {
                    result.push_str("\"");
                    self.increment(1);
                } else if ['\\', '/'].contains(&ch) {
                    let escaped = ch.escape_default().next().unwrap_or(ch);
                    result.push(escaped);
                    self.increment(1);
                } else if ['b', 'f', 'n', 'r', 't'].contains(&ch) {
                    let ch = match ch {
                        'b' => '\u{8}',
                        'f' => '\x0C',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => unreachable!(),
                    };
                    result.push(ch);
                    self.increment(1);
                } else if ch == 'u' {
                    if self.chars[self.i + 2].is_ascii_hexdigit()
                        && self.chars[self.i + 3].is_ascii_hexdigit()
                        && self.chars[self.i + 4].is_ascii_hexdigit()
                        && self.chars[self.i + 5].is_ascii_hexdigit()
                    {
                        let char_str = String::from_iter(&self.chars[self.i + 2..=self.i + 5]);
                        let code = u16::from_str_radix(&char_str, 16)
                            .expect("Failed to parse unicode escape number");
                        let string = String::from_utf16_lossy(&[code]);
                        result.push_str(&string);
                        self.increment(5);
                    } else {
                        return Err(ParseError::ExpectedUnicodeEscape(format!(
                            "Expected a unicode escape sequence"
                        )));
                    }
                } else {
                    return Err(ParseError::ExpectedEscapeChar(format!(
                        "Expected an escape sequence"
                    )));
                }
            } else {
                result.push(self.chars[self.i]);
            }
            self.increment(1);
        }
        self.expect_not_end('"')?;
        self.increment(1);
        Ok(Some(JSONValue::from(result)))
    }

    fn parse_number(&mut self) -> Result<Option<JSONValue>, ParseError> {
        let start = self.i;
        if !(self.chars[start].is_ascii_digit() || self.chars[start] == '-') {
            return Ok(None);
        }
        let max = self.chars.len() - 1;
        let mut n = start;
        if self.chars[n] == '-' && n < max {
            n += 1;
            self.expect_digit(start, n)?;
        }
        while self.chars[n].is_ascii_digit() && n < max {
            n += 1;
        }
        if self.chars[n] == '.' && n < max {
            n += 1;
            self.expect_digit(start, n)?;
            while self.chars[n].is_ascii_digit() && n < max {
                n += 1;
            }
        }
        if self.chars[n] == 'e' || self.chars[n] == 'E' && n < max {
            n += 1;
            if self.chars[n] == '-' || self.chars[n] == '+' && n < max {
                n += 1;
            }
            self.expect_digit(start, n)?;
            while self.chars[n].is_ascii_digit() && n < max {
                n += 1;
            }
        }
        if n > start {
            let mut end = if n < self.chars.len() { n } else { max };
            if !self.chars[end].is_ascii_digit() {
                end -= 1;
            }
            let str = String::from_iter(&self.chars[start..=end]);
            match str.parse::<f64>() {
                Ok(number) => {
                    self.increment(str.len());
                    return Ok(Some(JSONValue::from(number)));
                }
                Err(e) => Err(ParseError::ExpectedDigit(format!("'{}', {:#?}", str, e))),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_keyword(
        &mut self,
        search: &str,
        value: JSONValue,
    ) -> Result<Option<JSONValue>, ParseError> {
        let start = self.i;
        let end = if self.i + search.len() > self.chars.len() {
            self.chars.len()
        } else {
            self.i + search.len()
        };
        let slice = &String::from_iter(&self.chars[start..end]);
        if slice == search {
            self.i += search.len();
            return Ok(Some(value));
        }
        Ok(None)
    }

    fn skip_whitespace(&mut self) {
        while self.chars[self.i].is_ascii_whitespace() {
            self.increment(1);
        }
    }

    fn eat(&mut self, ch: char) -> Result<(), ParseError> {
        if self.chars[self.i] != ch {
            let msg = format!("Expected {}.", ch);
            return Err(ParseError::ExpectedToken(msg));
        }
        self.increment(1);
        Ok(())
    }

    fn increment(&mut self, amount: usize) {
        let current = self.i;
        if current + amount >= self.chars.len() {
            self.i = self.chars.len() - 1;
        } else {
            self.i += amount;
        }
    }

    fn expect_digit(&mut self, start: usize, end: usize) -> Result<(), ParseError> {
        let current = String::from_iter(&self.chars[start..end]);
        if !self.chars[end].is_ascii_digit() {
            Err(ParseError::ExpectedDigit(format!(
                "Expected a digit, received '{}' after numeric '{}'",
                self.chars[end], current
            )))
        } else {
            Ok(())
        }
    }

    fn expect_not_end(&mut self, ch: char) -> Result<(), ParseError> {
        if self.i == self.chars.len() {
            Err(ParseError::UnexpectedEndOfInput(format!(
                "Unexpected end of input. Expected '{}'",
                ch
            )))
        } else {
            Ok(())
        }
    }

    pub fn parse(json: &str) -> JSONResult {
        JSON::new(json).parse_value()
    }
}
