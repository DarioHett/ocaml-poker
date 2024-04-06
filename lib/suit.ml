open Core

module T = struct
  type t = Spades | Hearts | Diamonds | Clubs

  let sexp_of_t t =
    match t with
    | Spades -> Sexp.Atom "Spades"
    | Hearts -> Sexp.Atom "Hearts"
    | Diamonds -> Sexp.Atom "Diamonds"
    | Clubs -> Sexp.Atom "Clubs"

  let t_of_sexp t =
    match t with
    | Sexp.Atom "Spades" -> Spades
    | Sexp.Atom "Hearts" -> Hearts
    | Sexp.Atom "Diamonds" -> Diamonds
    | Sexp.Atom "Clubs" -> Clubs
    | _ -> failwith @@ Sexp.to_string t

  let to_string t =
    match t with
    | Spades -> "s"
    | Hearts -> "h"
    | Diamonds -> "d"
    | Clubs -> "c"

  let from_char_exn char =
    match char with
    | 's' -> Spades
    | 'h' -> Hearts
    | 'd' -> Diamonds
    | 'c' -> Clubs
    | 'S' -> Spades
    | 'H' -> Hearts
    | 'D' -> Diamonds
    | 'C' -> Clubs
    | _ -> failwith @@ Char.to_string char

  let to_int t =
    match t with Spades -> 3 | Hearts -> 2 | Diamonds -> 1 | Clubs -> 0

  let of_int t =
    match t with
    | 3 -> Some Spades
    | 2 -> Some Hearts
    | 1 -> Some Diamonds
    | 0 -> Some Clubs
    | _ -> None

  let of_int_exn t =
    match of_int t with Some suit -> suit | _ -> failwith @@ Int.to_string t

  let compare a b = compare (to_int a) (to_int b)
end

include T
include Comparator.Make (T)

let%expect_test "suit-compare" =
  print_s [%sexp (compare Spades Clubs : int)];
  [%expect {| 1 |}]

let%expect_test "suit-of-int-exn-too-large" =
  Printexc.record_backtrace false;
  print_s [%sexp (of_int_exn 4 |> to_int : int)];
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure 4) |}]

let%expect_test "suit-of-int-exn-too-low" =
  Printexc.record_backtrace false;
  print_s [%sexp (of_int_exn (-1) |> to_int : int)];
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure -1) |}]

let%expect_test "suit-of-int-exn-sup" =
  print_s [%sexp (of_int_exn 3 |> to_int : int)];
  [%expect {| 3 |}]

let%expect_test "suit-of-int-exn-inf" =
  print_s [%sexp (of_int_exn 0 |> to_int : int)];
  [%expect {| 0 |}]
