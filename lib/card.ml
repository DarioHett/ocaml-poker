open Core

type t = Suit.t * Rank.t [@@deriving sexp]

let to_string (suit, rank) =
  String.( ^ ) (Rank.to_string rank) (Suit.to_string suit)

let from_string_exn str =
  match (String.get str 0, String.get str 1) with
  | rank, suit -> (Suit.from_char_exn suit, Rank.from_char_exn rank)

let to_int (suit, rank) = (Suit.to_int suit * 13) + Rank.to_int rank

let of_int t =
  match (Suit.of_int (Int.( / ) t 13), Rank.of_int (Int.rem t 13)) with
  | Some suit, Some rank -> Some (suit, rank)
  | _ -> None

let of_int_exn t =
  match of_int t with Some card -> card | _ -> failwith @@ Int.to_string t

let compare (suit_a, rank_a) (suit_b, rank_b) =
  match Rank.compare rank_a rank_b with
  | 0 -> Suit.compare suit_a suit_b
  | x -> x

let%expect_test "card-compare" =
  print_s
    [%sexp (compare (Suit.Spades, Rank.Ace) (Suit.Hearts, Rank.Ace) : int)];
  [%expect {| 1 |}]

let%expect_test "card-of-int-exn-too-large" =
  Printexc.record_backtrace false;
  print_s [%sexp (of_int_exn 52 |> to_int : int)];
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure 52) |}]

let%expect_test "card-of-int-exn-too-low" =
  Printexc.record_backtrace false;
  print_s [%sexp (of_int_exn (-1) |> to_int : int)];
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure -1) |}]

let%expect_test "card-of-int-exn-sup" =
  print_s [%sexp (of_int_exn 51 |> to_int : int)];
  [%expect {| 51 |}]

let%expect_test "card-of-int-exn-inf" =
  print_s [%sexp (of_int_exn 0 |> to_int : int)];
  [%expect {| 0 |}]
