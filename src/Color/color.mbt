enum Color {
  None
  Red
  Green
  Yellow
  Blue
  Magenta
  Cyan
  StrongRed
  StrongGreen
  StrongYellow
  StrongBlue
  StrongMagenta
  StrongCyan
}

struct Pigment {
  color : Color
  background : Color
  bold : Bool
  underline : Bool
  italic : Bool
}

pub fn pigment(
  ~color : Color = Color::None,
  ~background : Color = Color::None,
  ~bold : Bool = false,
  ~underline : Bool = false,
  ~italic : Bool = false
) -> Pigment {
  Pigment::{ color, background, bold, underline, italic }
}

pub fn apply(self : Pigment, s : String) -> String {
  let mut new_str = ""
  new_str += match self.color {
    Color::Red => "\x1b[31m"
    Color::Green => "\x1b[32m"
    Color::Yellow => "\x1b[33m"
    Color::Blue => "\x1b[34m"
    Color::Magenta => "\x1b[35m"
    Color::Cyan => "\x1b[36m"
    Color::StrongRed => "\x1b[91m"
    Color::StrongGreen => "\x1b[92m"
    Color::StrongYellow => "\x1b[93m"
    Color::StrongBlue => "\x1b[94m"
    Color::StrongMagenta => "\x1b[95m"
    Color::StrongCyan => "\x1b[96m"
    Color::None => ""
  }
  new_str += match self.background {
    Color::Red => "\x1b[41m"
    Color::Green => "\x1b[42m"
    Color::Yellow => "\x1b[43m"
    Color::Blue => "\x1b[44m"
    Color::Magenta => "\x1b[45m"
    Color::Cyan => "\x1b[46m"
    Color::StrongRed => "\x1b[101m"
    Color::StrongGreen => "\x1b[102m"
    Color::StrongYellow => "\x1b[103m"
    Color::StrongBlue => "\x1b[104m"
    Color::StrongMagenta => "\x1b[105m"
    Color::StrongCyan => "\x1b[106m"
    Color::None => ""
  }
  new_str += if self.bold { "\x1b[1m" } else { "" }
  new_str += if self.underline { "\x1b[4m" } else { "" }
  new_str += if self.italic { "\x1b[3m" } else { "" }
  if new_str != "" {
    new_str + s + "\x1b[0m"
  } else {
    s
  }
}

pub fn red_font(s : String) -> String {
  apply(pigment(color=Color::Red), s)
}

pub fn green_font(s : String) -> String {
  apply(pigment(color=Color::Green), s)
}

pub fn yellow_font(s : String) -> String {
  apply(pigment(color=Color::Yellow), s)
}

pub fn blue_font(s : String) -> String {
  apply(pigment(color=Color::Blue), s)
}

pub fn magenta_font(s : String) -> String {
  apply(pigment(color=Color::Magenta), s)
}

pub fn cyan_font(s : String) -> String {
  apply(pigment(color=Color::Cyan), s)
}

pub fn strong_red_font(s : String) -> String {
  apply(pigment(color=Color::StrongRed), s)
}

pub fn strong_green_font(s : String) -> String {
  apply(pigment(color=Color::StrongGreen), s)
}

pub fn strong_yellow_font(s : String) -> String {
  apply(pigment(color=Color::StrongYellow), s)
}

pub fn strong_blue_font(s : String) -> String {
  apply(pigment(color=Color::StrongBlue), s)
}

pub fn strong_magenta_font(s : String) -> String {
  apply(pigment(color=Color::StrongMagenta), s)
}

pub fn strong_cyan_font(s : String) -> String {
  apply(pigment(color=Color::StrongCyan), s)
}

pub fn red_background(s : String) -> String {
  apply(pigment(background=Color::Red), s)
}

pub fn green_background(s : String) -> String {
  apply(pigment(background=Color::Green), s)
}

pub fn yellow_background(s : String) -> String {
  apply(pigment(background=Color::Yellow), s)
}

pub fn blue_background(s : String) -> String {
  apply(pigment(background=Color::Blue), s)
}

pub fn magenta_background(s : String) -> String {
  apply(pigment(background=Color::Magenta), s)
}

pub fn cyan_background(s : String) -> String {
  apply(pigment(background=Color::Cyan), s)
}

pub fn strong_red_background(s : String) -> String {
  apply(pigment(background=Color::StrongRed), s)
}

pub fn strong_green_background(s : String) -> String {
  apply(pigment(background=Color::StrongGreen), s)
}

pub fn strong_yellow_background(s : String) -> String {
  apply(pigment(background=Color::StrongYellow), s)
}

pub fn strong_blue_background(s : String) -> String {
  apply(pigment(background=Color::StrongBlue), s)
}

pub fn strong_magenta_background(s : String) -> String {
  apply(pigment(background=Color::StrongMagenta), s)
}

pub fn strong_cyan_background(s : String) -> String {
  apply(pigment(background=Color::StrongCyan), s)
}

pub fn bold(s : String) -> String {
  apply(pigment(bold=true), s)
}

pub fn bold_red(s : String) -> String {
  apply(pigment(color=Color::Red, bold=true), s)
}

pub fn bold_green(s : String) -> String {
  apply(pigment(color=Color::Green, bold=true), s)
}

pub fn bold_yellow(s : String) -> String {
  apply(pigment(color=Color::Yellow, bold=true), s)
}

pub fn bold_blue(s : String) -> String {
  apply(pigment(color=Color::Blue, bold=true), s)
}

pub fn bold_magenta(s : String) -> String {
  apply(pigment(color=Color::Magenta, bold=true), s)
}

pub fn bold_strong_red(s : String) -> String {
  apply(pigment(color=Color::StrongRed, bold=true), s)
}

pub fn bold_strong_green(s : String) -> String {
  apply(pigment(color=Color::StrongGreen, bold=true), s)
}

pub fn bold_strong_yellow(s : String) -> String {
  apply(pigment(color=Color::StrongYellow, bold=true), s)
}

pub fn bold_strong_blue(s : String) -> String {
  apply(pigment(color=Color::StrongBlue, bold=true), s)
}

pub fn bold_strong_magenta(s : String) -> String {
  apply(pigment(color=Color::StrongMagenta, bold=true), s)
}

pub fn underline(s : String) -> String {
  apply(pigment(underline=true), s)
}

pub fn italic(s : String) -> String {
  apply(pigment(italic=true), s)
}
