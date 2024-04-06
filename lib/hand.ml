open Core
open Sexplib.Std

type t =
  | HighCard of int
  | Pair of Rank.t * int
  | TwoPair of Rank.t * Rank.t * int
  | ThreeOfAKind of Rank.t
  | Straight of Rank.t
  | Flush of Suit.t
  | FullHouse of Rank.t * Rank.t
  | FourOfAKind of Rank.t
  | StraightFlush of Rank.t
[@@deriving sexp]

let to_int t =
  match t with
  | HighCard _ -> 0
  | Pair _ -> 1
  | TwoPair _ -> 2
  | ThreeOfAKind _ -> 3
  | Straight _ -> 4
  | Flush _ -> 5
  | FullHouse _ -> 6
  | FourOfAKind _ -> 7
  | StraightFlush _ -> 8

let four_of_a_kind t =
  Option.(
    Cardsummary.has_n_of_same_rank 4 t >>| fun x ->
    FourOfAKind (Rank.of_int_exn x))

let three_of_a_kind t =
  Option.(
    Cardsummary.has_n_of_same_rank 3 t >>| fun x ->
    ThreeOfAKind (Rank.of_int_exn x))

let flush t =
  (* Should flush return Suit? *)
  Option.(Cardsummary.has_5_same_suit t >>| fun x -> Flush x)

let full_house t =
  let open Option in
  Cardsummary.has_n_of_same_rank 3 t >>= fun trip_rank ->
  Cardsummary.has_n_of_same_rank ~rank_to_ignore:trip_rank 2 t >>| fun x ->
  FullHouse (Rank.of_int_exn trip_rank, Rank.of_int_exn x)

let straight (t : Cardsummary.t) =
  (* `straight` IS BUGGY FOR SURE. SOMETHING DOES NOT TRANSLATE WELL.
     PROBABLY SELECT HIGH CARD SEPARATELY? *)
  let open Option in
  Array.findi Cardsummary.straight_bitmaps ~f:(fun _ straight_bitmap ->
      Int.( land ) t.rank_bitmap straight_bitmap |> Int.equal straight_bitmap)
  >>| fun (index, _) ->
  match 12 - index with
  (* | 3 -> Straight (Rank.of_int_exn 12) *)
  | _ -> Straight (Rank.of_int_exn (12 - index))

let straight_flush (t : Cardsummary.t) =
  let has_flush = flush t in
  match has_flush with
  | Some (Flush suit) -> (
      let crds =
        List.filter
          ~f:(fun (csuit, _) -> Suit.compare csuit suit |> Int.equal 0)
          t.cards
      in
      let has_straight = Cardsummary.of_cards crds |> straight in
      match has_straight with
      | Some (Straight rank) -> Some (StraightFlush rank)
      | _ -> None)
  | _ -> None

let high_card (t : Cardsummary.t) =
  Some (HighCard (Cardsummary.get_kickers 5 t.rank_bitmap))

let two_pair (t : Cardsummary.t) =
  let open Option in
  let top_pair_rank = Cardsummary.has_n_of_same_rank 2 t in
  let add_bottom_pair_rank top_pair_rank =
    Cardsummary.has_n_of_same_rank ~rank_to_ignore:top_pair_rank 2 t
    >>| fun bottom_pair_rank -> (top_pair_rank, bottom_pair_rank)
  in
  let to_two_pair (top_pair_rank, bottom_pair_rank) =
    let kicker =
      Cardsummary.get_kickers
        ~ranks_to_ignore:[| top_pair_rank; bottom_pair_rank |]
        1 t.rank_bitmap
    in
    TwoPair
      (Rank.of_int_exn top_pair_rank, Rank.of_int_exn bottom_pair_rank, kicker)
  in
  top_pair_rank >>= add_bottom_pair_rank >>| to_two_pair

let pair (t : Cardsummary.t) =
  let open Option in
  let pair_rank = Cardsummary.has_n_of_same_rank 2 t in
  let add_kicker pair_rank =
    Cardsummary.get_kickers ~ranks_to_ignore:[| pair_rank |] 3 t.rank_bitmap
    |> fun kicker -> (pair_rank, kicker)
  in
  let to_pair (rank, kicker) = Pair (Rank.of_int_exn rank, kicker) in
  pair_rank >>| add_kicker >>| to_pair

let%expect_test "hand-two_pair-4-2-fh" =
  let example_fh =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Two);
        (Suit.Spades, Rank.Two);
        (Suit.Hearts, Rank.Four);
        (Suit.Spades, Rank.Four);
        (Suit.Clubs, Rank.Four);
      ]
  in
  print_s [%sexp (two_pair example_fh : t option)];
  [%expect {| ((TwoPair Four Two 0)) |}]

let%expect_test "hand-pair-4-2-fh" =
  let example_fh =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Two);
        (Suit.Spades, Rank.Two);
        (Suit.Hearts, Rank.Four);
        (Suit.Spades, Rank.Four);
        (Suit.Clubs, Rank.Four);
      ]
  in
  print_s [%sexp (pair example_fh : t option)];
  [%expect {| ((Pair Four 1)) |}]

let%expect_test "hand-two_pair-4-2" =
  let example_fh =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Two);
        (Suit.Spades, Rank.Two);
        (Suit.Hearts, Rank.Four);
        (Suit.Spades, Rank.Four);
        (Suit.Clubs, Rank.Five);
      ]
  in
  print_s [%sexp (two_pair example_fh : t option)];
  [%expect {| ((TwoPair Four Two 8)) |}]

let%expect_test "hand-pair-4-2" =
  let example_fh =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Two);
        (Suit.Spades, Rank.Two);
        (Suit.Hearts, Rank.Four);
        (Suit.Spades, Rank.Four);
        (Suit.Clubs, Rank.Five);
      ]
  in
  print_s [%sexp (pair example_fh : t option)];
  [%expect {| ((Pair Four 9)) |}]

let%expect_test "hand-full_house-4-2" =
  let example_fh =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Two);
        (Suit.Spades, Rank.Two);
        (Suit.Hearts, Rank.Four);
        (Suit.Spades, Rank.Four);
        (Suit.Clubs, Rank.Four);
      ]
  in
  print_s [%sexp (full_house example_fh : t option)];
  [%expect {| ((FullHouse Four Two)) |}]

let%expect_test "hand-full_house-not" =
  let example_fh =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Three);
        (Suit.Spades, Rank.Two);
        (Suit.Hearts, Rank.Four);
        (Suit.Spades, Rank.Four);
        (Suit.Clubs, Rank.Four);
      ]
  in
  print_s [%sexp (full_house example_fh : t option)];
  [%expect {| () |}]

let%expect_test "hand-straight-flush-6" =
  let example_strt =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Nine);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (straight_flush example_strt : t option)];
  [%expect {| ((StraightFlush Ten)) |}]

let%expect_test "hand-straight-6" =
  let example_strt =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Nine);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (straight example_strt : t option)];
  [%expect {| ((Straight Ten)) |}]

let%expect_test "hand-straight-not" =
  let example_strt =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Jake);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (straight example_strt : t option)];
  [%expect {| () |}]

let%expect_test "hand-flush-hearts" =
  let example_strtflush =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Nine);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (flush example_strtflush : t option)];
  [%expect {| ((Flush Hearts)) |}]

let%expect_test "hand-straight-flush-6" =
  let example_strtflush =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Nine);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (flush example_strtflush : t option)];
  [%expect {| ((Flush Hearts)) |}]

let compare a b = compare (to_int a) (to_int b)

let compare_variants a b =
  match (a, b) with
  | HighCard x, HighCard y -> Int.compare x y
  | Pair (x1, x2), Pair (y1, y2) -> (
      match Rank.compare x1 y1 with 0 -> Int.compare x2 y2 | x -> x)
  | TwoPair (x1, x2, x3), TwoPair (y1, y2, y3) -> (
      match Rank.compare x1 y1 with
      | 0 -> ( match Rank.compare x2 y2 with 0 -> Int.compare x3 y3 | x -> x)
      | x -> x)
  | ThreeOfAKind x, ThreeOfAKind y -> Rank.compare x y
  | Straight x, Straight y -> Rank.compare x y
  | Flush x, Flush y -> Suit.compare x y
  | FullHouse (x1, x2), FullHouse (y1, y2) -> (
      match Rank.compare x1 y1 with 0 -> Rank.compare x2 y2 | x -> x)
  | FourOfAKind x, FourOfAKind y -> Rank.compare x y
  | StraightFlush x, StraightFlush y -> Rank.compare x y
  | a, b -> compare a b

let analyse_hand (t : Cardsummary.t) =
  List.find_map
    [
      straight_flush;
      four_of_a_kind;
      full_house;
      flush;
      straight;
      three_of_a_kind;
      two_pair;
      pair;
      high_card;
    ] ~f:(fun f -> f t)

let%expect_test "hand-analyse-straight-flush" =
  let example_strtflush =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Nine);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (analyse_hand example_strtflush : t option)];
  [%expect {| ((StraightFlush Ten)) |}]

let%expect_test "hand-analyse-straight" =
  let example_strtflush =
    Cardsummary.of_cards
      [
        (Suit.Spades, Rank.Nine);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (analyse_hand example_strtflush : t option)];
  [%expect {| ((Straight Ten)) |}]

let%expect_test "hand-analyse-flush" =
  let example_strtflush =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.Three);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Six);
        (Suit.Hearts, Rank.Seven);
        (Suit.Hearts, Rank.Eight);
      ]
  in
  print_s [%sexp (analyse_hand example_strtflush : t option)];
  [%expect {| ((Flush Hearts)) |}]

let%expect_test "King high-1" =
  let cards =
    Cardsummary.of_cards
      [
        (Suit.Hearts, Rank.King);
        (Suit.Hearts, Rank.Queen);
        (Suit.Hearts, Rank.Jake);
        (Suit.Hearts, Rank.Ten);
        (Suit.Hearts, Rank.Nine);
        (Suit.Clubs, Rank.Ace);
        (Suit.Diamonds, Rank.Ace);
      ]
  in
  print_s [%sexp (straight_flush cards : t option)];
  [%expect {| ((StraightFlush King))|}]

let%expect_test "King high" =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "KH"; "QH"; "JH"; "TH"; "9H"; "AC"; "AD" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((StraightFlush King))|}]

let%expect_test "Ace high " =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "KH"; "QH"; "JH"; "TH"; "9H"; "AH"; "AD" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((StraightFlush Ace)) |}]

let%expect_test "Five high " =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "5H"; "4H"; "3H"; "2H"; "AH"; "AC"; "AD" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((StraightFlush Five))|}]

let%expect_test "9 high with more of same suit" =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "5C"; "6C"; "7C"; "8C"; "9C"; "AC"; "7S" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((StraightFlush Nine)) |}]

let%expect_test "Ts on 3s" =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "3S"; "3C"; "TH"; "TC"; "TS"; "AD"; "KD" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((FullHouse Ten Three))|}]

let%expect_test "3s on Ts" =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "3S"; "3C"; "3D"; "TC"; "TS"; "AD"; "KD" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((FullHouse Three Ten)) |}]

let%expect_test "Queen high 1" =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "QH"; "JH"; "8H"; "7H"; "6H"; "TC"; "9C" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((Flush Hearts)) |}]

let%expect_test "Queen high 1" =
  let cards =
    Cardsummary.of_cards
      (List.map ~f:Card.from_string_exn
         [ "QH"; "JH"; "9H"; "7H"; "6H"; "TC"; "9C" ])
  in
  print_s [%sexp (analyse_hand cards : t option)];
  [%expect {| ((Flush Hearts)) |}]
