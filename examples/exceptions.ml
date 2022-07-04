let () =
  try
    let x = 4 / 0 in
    Printf.printf "%d\n" x
  with
  | Division_by_zero -> print_endline "Cannot divide by zero"
  | _ -> print_endline "Something went wrong"

let a = 1
let b = 2

exception Foo of string

let x = "bonjour"
let y = "monde"
let i_will_fail () = raise (Foo "Oh no!")
