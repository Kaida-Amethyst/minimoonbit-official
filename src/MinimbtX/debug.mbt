pub fn debug_one_line(
  head_token : Token,
  end_token : Token,
  msg : String
) -> Unit {
  let context = head_token.context
  let head_line = head_token.line
  let end_line = end_token.line
  let head_column = head_token.column
  let end_column = end_token.column + (end_token.end_loc - end_token.start_loc)
  let code_line = context.get_line(head_line)
  let normal_head = code_line.substring(start=0, end=head_column)
  let colored = code_line.substring(start=head_column, end=end_column)
  let colored = @Color.underline(colored)
  let normal_tail = code_line.substring(
    start=end_column,
    end=code_line.length(),
  )
  let pre_line = "   |\n"
  let content_line = " " +
    (head_line + 1).to_string() +
    " |" +
    normal_head +
    colored +
    normal_tail +
    "  ---  " +
    msg +
    "\n"
  let post_line = "   |"
  let whole = pre_line + content_line + post_line
  println(whole)
}

pub fn debug(head_token : Token, end_token : Token, msg : String) -> Unit {
  let context = head_token.context
  let head_line = head_token.line
  let end_line = end_token.line
  let head_column = head_token.column
  let end_column = end_token.column + (end_token.end_loc - end_token.start_loc)
  if head_line == end_line {
    debug_one_line(head_token, end_token, msg)
    return
  }
  let code_lines : Array[String] = []
  let first_hl = context.get_line(head_line)
  let first_end = first_hl.length()
  let first_hl_normal = first_hl.substring(start=0, end=head_column)
  let first_hl_colored = first_hl.substring(start=head_column, end=first_end)
  let first_hl_colored = @Color.magenta_font(first_hl_colored)
  let first_hl = first_hl_normal + first_hl_colored
  code_lines.push(first_hl)
  for i = head_line + 1; i < end_line - 1; i = i + 1 {
    let colored_line = @Color.green_background(context.get_line(i))
    code_lines.push(colored_line)
  }
  let last_hl = context.get_line(end_line)
  let last_end = last_hl.length()
  let last_hl_colored = last_hl.substring(start=0, end=end_column)
  let last_hl_colored = @Color.magenta_font(last_hl_colored)
  let last_hl_normal = last_hl.substring(start=end_column, end=last_end)
  let last_hl = last_hl_colored +
    last_hl_normal +
    "  ---  " +
    @Color.green_font(msg)
  code_lines.push(last_hl)
  let mut whole = "  |\n"
  let mut line_no = head_line + 1
  for code_line in code_lines {
    whole = whole + line_no.to_string() + " |" + code_line + "\n"
    line_no = line_no + 1
  }
  whole = whole + "  |"
  println(whole)
}
