let match_regexp str re =
  Str.string_match (Str.regexp re) str 0

let raise_exception f e =
  try
    ignore (f ());
    false
  with e' ->
    Printexc.to_string e = Printexc.to_string e'

let raise_an_exception f =
  try
    ignore (f ());
    false
  with _ -> true
