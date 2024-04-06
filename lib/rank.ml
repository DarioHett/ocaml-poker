open Core

type t =
  | Ace
  | King
  | Queen
  | Jake
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
[@@deriving sexp]

let to_string t =
  match t with
  | Ace -> "A"
  | King -> "K"
  | Queen -> "Q"
  | Jake -> "J"
  | Ten -> "T"
  | Nine -> "9"
  | Eight -> "8"
  | Seven -> "7"
  | Six -> "6"
  | Five -> "5"
  | Four -> "4"
  | Three -> "3"
  | Two -> "2"

let from_char_exn char =
  match char with
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Jake
  | 'T' -> Ten
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Two
  | _ -> failwith @@ Char.to_string char

let to_int t =
  match t with
  | Ace -> 12
  | King -> 11
  | Queen -> 10
  | Jake -> 9
  | Ten -> 8
  | Nine -> 7
  | Eight -> 6
  | Seven -> 5
  | Six -> 4
  | Five -> 3
  | Four -> 2
  | Three -> 1
  | Two -> 0

let of_int t =
  match t with
  | 12 -> Some Ace
  | 11 -> Some King
  | 10 -> Some Queen
  | 9 -> Some Jake
  | 8 -> Some Ten
  | 7 -> Some Nine
  | 6 -> Some Eight
  | 5 -> Some Seven
  | 4 -> Some Six
  | 3 -> Some Five
  | 2 -> Some Four
  | 1 -> Some Three
  | 0 -> Some Two
  | _ -> None

let of_int_exn t =
  match of_int t with Some rank -> rank | _ -> failwith @@ Int.to_string t

let succ t = to_int t |> Int.( + ) 1 |> (Fn.flip Int.rem) 13 |> of_int_exn
let compare a b = compare (to_int a) (to_int b)

let%expect_test "rank-compare" =
  print_s [%sexp (compare Ace King : int)];
  [%expect {| 1 |}]

let%expect_test "rank-succ" =
  print_s [%sexp (succ Ace |> to_int : int)];
  [%expect {| 0 |}]

let%expect_test "rank-of-int-exn-too-large" =
  Printexc.record_backtrace false;
  print_s [%sexp (of_int_exn 13 |> to_int : int)];
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure 13) |}]

let%expect_test "rank-of-int-exn-too-low" =
  Printexc.record_backtrace false;
  print_s [%sexp (of_int_exn (-1) |> to_int : int)];
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure -1) |}]

let%expect_test "rank-of-int-exn-sup" =
  print_s [%sexp (of_int_exn 12 |> to_int : int)];
  [%expect {| 12 |}]

let%expect_test "rank-of-int-exn-inf" =
  print_s [%sexp (of_int_exn 0 |> to_int : int)];
  [%expect {| 0 |}]
