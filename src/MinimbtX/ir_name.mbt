/// ------------------------------------------------------------------
/// # Name
///
/// eg:
///   %tmp.1 // mark `tmp`, slot 1
///   %a.2   // mark `a`, slot 2
///   %42    // id: 42
/// ------------------------------------------------------------------
struct Name {
  mark : String
  slot : Int?
}

fn Name::new(
  mark : String,
  ~slot : Int? = None,
) -> Name {
  Name::{ mark, slot }
}

fn createSibling(self: Name, ~slot: Int? = None) -> Name {
  match slot {
    None => match self.slot {
      None => Name::new(self.mark, slot = Some(1))
      Some(i) => Name::new(self.mark, slot = Some(i + 1))
    }
    Some(_) => Name::new(self.mark, slot = slot)
  }
}

pub fn to_string(self : Name) -> String {
  match self.mark {
    "_" => "_"
    _ => match self.slot {
      None => self.mark
      Some(i) => self.mark + "." + i.to_string()
    }
  }
}

fn output(self : Name, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

fn op_equal(self : Name, other : Name) -> Bool {
  self.mark == other.mark && self.slot == other.slot
}

fn hash_combine(self : Name, hasher : Hasher) -> Unit {
  hasher.combine_string(self.to_string())
}
