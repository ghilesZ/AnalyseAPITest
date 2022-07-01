let x = 50
let seven_square x = 7 * x * x
let is_even x = x mod 2 = 0

(* Test des patterns et tuples : *)
let y = (5, 0)
let y1, y2 = (5, 0)

let y1 = 5
and y2 = 0

let matching n =
  match n with
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | _ -> "many"

