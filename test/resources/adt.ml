type data =
  | Cons1 of int
  | Cons2 of int list
  | Cons3 of data
[@@deriving elpi]
