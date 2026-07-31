type 'a parse_result =
  | Success of int * 'a
  | Failure
