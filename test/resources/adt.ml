type data =
  | Cons1 of int
  | Cons2 of bool
  | Cons3 of data
[@@deriving elpi]
