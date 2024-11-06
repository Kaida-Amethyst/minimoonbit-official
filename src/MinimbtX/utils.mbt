fn is_whitespace(c : Char) -> Bool {
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

fn is_alpha(c : Char) -> Bool {
  let i = c.to_int()
  (i >= 'a'.to_int() && i <= 'z'.to_int()) ||
  (i >= 'A'.to_int() && i <= 'Z'.to_int())
}

fn is_lowercase(c : Char) -> Bool {
  let i = c.to_int()
  i >= 'a'.to_int() && i <= 'z'.to_int()
}

fn is_uppercase(c : Char) -> Bool {
  let i = c.to_int()
  i >= 'A'.to_int() && i <= 'Z'.to_int()
}

fn is_digit(c : Char) -> Bool {
  let i = c.to_int()
  i >= '0'.to_int() && i <= '9'.to_int()
}

fn is_hex_digit(c : Char) -> Bool {
  let i = c.to_int()
  (i >= '0'.to_int() && i <= '9'.to_int()) ||
  (i >= 'a'.to_int() && i <= 'f'.to_int()) ||
  (i >= 'A'.to_int() && i <= 'F'.to_int())
}

fn is_oct_digit(c : Char) -> Bool {
  let i = c.to_int()
  i >= '0'.to_int() && i <= '7'.to_int()
}

fn is_parenthesis(c : Char) -> Bool {
  let parenthesis = ['(', ')', '[', ']', '{', '}']
  parenthesis.iter().any(fn(p) { c == p })
}

fn to_lowercase(c: Char) -> Char {
  match c {
    'A' => 'a'
    'B' => 'b'
    'C' => 'c'
    'D' => 'd'
    'E' => 'e'
    'F' => 'f'
    'G' => 'g'
    'H' => 'h'
    'I' => 'i'
    'J' => 'j'
    'K' => 'k'
    'L' => 'l'
    'M' => 'm'
    'N' => 'n'
    'O' => 'o'
    'P' => 'p'
    'Q' => 'q'
    'R' => 'r'
    'S' => 's'
    'T' => 't'
    'U' => 'u'
    'V' => 'v'
    'W' => 'w'
    'X' => 'x'
    'Y' => 'y'
    'Z' => 'z'
    _ => c
  }
}

type! NonDigitCharError String derive(Show)

fn char_to_int64(c : Char) -> Int64 {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    _ => {
      println("\{c} Is Not a digit")
      0
    }
  }
}

fn char_to_int64_hex(c : Char) -> Int64 {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    'a' => 10
    'b' => 11
    'c' => 12
    'd' => 13
    'e' => 14
    'f' => 15
    'A' => 10
    'B' => 11
    'C' => 12
    'D' => 13
    'E' => 14
    'F' => 15
    _ => {
      println("\{c} Is Not a hex digit")
      0
    }
  }
}

fn char_to_int64_oct(c : Char) -> Int64 {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    _ => {
      println("\{c} Is Not an oct digit")
      0
    }
  }
}

fn char_to_int32(c : Char) -> Int {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    _ => {
      println("\{c} Is Not a digit")
      0
    }
  }
}

fn char_to_int32_hex(c : Char) -> Int {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    'a' => 10
    'b' => 11
    'c' => 12
    'd' => 13
    'e' => 14
    'f' => 15
    'A' => 10
    'B' => 11
    'C' => 12
    'D' => 13
    'E' => 14
    'F' => 15
    _ => {
      println("\{c} Is Not a hex digit")
      0
    }
  }
}

fn char_to_int32_oct(c : Char) -> Int {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    _ => {
      println("\{c} Is Not an oct digit")
      0
    }
  }
}

fn char_to_uint32(c : Char) -> UInt {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    _ => {
      println("\{c} Is Not a digit")
      0
    }
  }
}

fn char_to_uint32_hex(c : Char) -> UInt {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    'a' => 10
    'b' => 11
    'c' => 12
    'd' => 13
    'e' => 14
    'f' => 15
    'A' => 10
    'B' => 11
    'C' => 12
    'D' => 13
    'E' => 14
    'F' => 15
    _ => {
      println("\{c} Is Not a hex digit")
      0
    }
  }
}

fn char_to_uint32_oct(c : Char) -> UInt {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    _ => {
      println("\{c} Is Not an oct digit")
      0
    }
  }
}

fn char_to_uint64(c : Char) -> UInt64 {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    _ => {
      println("\{c} Is Not a digit")
      0
    }
  }
}

fn char_to_uint64_hex(c : Char) -> UInt64 {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    'a' => 10
    'b' => 11
    'c' => 12
    'd' => 13
    'e' => 14
    'f' => 15
    'A' => 10
    'B' => 11
    'C' => 12
    'D' => 13
    'E' => 14
    'F' => 15
    _ => {
      println("\{c} Is Not a hex digit")
      0
    }
  }
}

fn char_to_uint64_oct(c : Char) -> UInt64 {
  match c {
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    _ => {
      println("\{c} Is Not an oct digit")
      0
    }
  }
}

fn is_keyword(s : String) -> Bool {
  let keywords = [
    "if", "else", "while", "for", "in", "loop", "fn", "let", "mut", "match", "return",
    "break", "continue", "derive", "impl", "trait", "enum", "struct", "type", "pub",
    "priv", "readonly", "as",
  ]
  keywords.iter().any(fn(k) { s == k })
}

fn stoi(s : String) -> Int {
  let (f, base, i) = if s.starts_with("0x") || s.starts_with("0X") {
    (char_to_int32_hex, 16, 2)
  } else if s.starts_with("0o") || s.starts_with("0O") {
    (char_to_int32_oct, 8, 2)
  } else {
    (char_to_int32, 10, 0)
  }
  s.to_array()[i:]
  .iter()
  .fold(
    fn(p, c) {
      let m = f(c)
      p * base + m
    },
    init=0,
  )
}

fn stol(s : String) -> Int64 {
  let (f, base, i) = if s.starts_with("0x") || s.starts_with("0X") {
    (char_to_int64_hex, 16L, 2)
  } else if s.starts_with("0o") || s.starts_with("0O") {
    (char_to_int64_oct, 8L, 2)
  } else {
    (char_to_int64, 10L, 0)
  }
  s.to_array()[i:]
  .iter()
  .fold(
    fn(p, c) {
      let m = f(c)
      p * base + m
    },
    init=0,
  )
}

fn stou(s : String) -> UInt {
  let (f, base, i) = if s.starts_with("0x") || s.starts_with("0X") {
    (char_to_uint32_hex, 16U, 2)
  } else if s.starts_with("0o") || s.starts_with("0O") {
    (char_to_uint32_oct, 8U, 2)
  } else {
    (char_to_uint32, 10U, 0)
  }
  s.to_array()[i:]
  .iter()
  .fold(
    fn(p, c) {
      let m = f(c)
      p * base + m
    },
    init=0,
  )
}

fn stoul(s : String) -> UInt64 {
  let (f, base, i) = if s.starts_with("0x") || s.starts_with("0X") {
    (char_to_uint64_hex, 16UL, 2)
  } else if s.starts_with("0o") || s.starts_with("0O") {
    (char_to_uint64_oct, 8UL, 2)
  } else {
    (char_to_uint64, 10UL, 0)
  }
  s.to_array()[i:]
  .iter()
  .fold(
    fn(p, c) {
      let m = f(c)
      p * base + m
    },
    init=0,
  )
}

fn stod(s : String) -> Double {
  if s.length() == 0 {
    return 0.0
  }
  let mut idx = 0
  let mut negation = false

  // skip leading whitespaces
  while idx < s.length() && s[idx] == ' ' {
    idx += 1
  }

  // check for sign
  if idx < s.length() && (s[idx] == '-' || s[idx] == '+') {
    negation = s[idx] == '-'
    idx += 1
  }

  // Parse integer part
  let mut integer_part = 0.0
  while idx < s.length() && is_digit(s[idx]) {
    integer_part = integer_part * 10 +
      (s[idx].to_int() - '0'.to_int()).to_double()
    idx += 1
  }

  // Parse fractional part
  let mut fractional_part = 0.0
  let mut fractional_divisor = 1.0
  if idx < s.length() && s[idx] == '.' {
    idx += 1
    while idx < s.length() && is_digit(s[idx]) {
      fractional_divisor /= 10.0
      fractional_part += (s[idx].to_int() - '0'.to_int()).to_double() *
        fractional_divisor
      idx += 1
    }
  }

  // Parse exponent part
  let mut exponent_part : Int = 0
  let mut exponent_negation = false
  if idx < s.length() && (s[idx] == 'e' || s[idx] == 'E') {
    idx += 1
    if idx < s.length() && (s[idx] == '-' || s[idx] == '+') {
      exponent_negation = s[idx] == '-'
      idx += 1
    }
    while idx < s.length() && is_digit(s[idx]) {
      exponent_part = exponent_part * 10 + (s[idx].to_int() - '0'.to_int())
      idx += 1
    }
  }

  // Handle trailing whitespaces
  while idx < s.length() && s[idx] == ' ' {
    idx += 1
  }

  // if idx < s.length() {
  //   raise NonNumericError("Invalid character")
  // }

  // Combine all parts
  let mut result = integer_part + fractional_part
  let mut pow10 = 1.0
  let mut cnt = 0
  while cnt < exponent_part {
    pow10 *= 10.0
    cnt += 1
  }
  if exponent_negation {
    pow10 = 1.0 / pow10
  }
  result *= pow10
  if negation {
    result = -result
  }
  result
}

fn repeat(s : String, n : Int) -> String {
  loop "", n {
    res, 0 => break res
    res, n => continue res + s, n - 1
  }
}

let symbol_chars = [
  '+', '-', '*', '/', '=', '|', '>', '<', '&', '^', '%', '!', '$', '@', '?', '!',
  '.', ':',
]

fn is_symbol(c : Char) -> Bool {
  symbol_chars.iter().any(fn(s) { c == s })
}

fn zip[T, U](left : Array[T], right: Array[U]) -> Array[(T, U)] {
  let result = []
  let mut i = 0
  while i < left.length() && i < right.length() {
    result.push((left[i], right[i]))
    i += 1
  }
  result
}
