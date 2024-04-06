open Core
open Sexplib.Std

type t = {
  suit_rank_bitmaps : int array; (* Length 4; Suit-indexed. *)
  rank_counts : int array; (* Length 13; Rank-indexed. *)
  rank_bitmap : int; (* Unsuited Rank Bitmap of Hand *)
  cards : Card.t list (* Orig. input *);
}
[@@deriving sexp]

let bitmap_of_rank t = Rank.to_int t |> Int.pow 2

let _has_n_of_same_rank ?(rank_to_ignore = 13) n (t : int array) =
  let open Option in
  let flip_index ix = 12 - ix in
  Array.rev t
  |> Array.findi ~f:(fun revix cnt ->
         match Int.equal (flip_index revix) rank_to_ignore with
         | true -> false
         | false -> Int.( >= ) cnt n)
  >>| fun (index, _) -> index |> flip_index

let%expect_test "has_n_of_same_rank-find" =
  print_s
    [%sexp
      (_has_n_of_same_rank 4
       @@ Array.of_list [ 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4 ]
        : int option)];
  [%expect {| (12) |}]

let%expect_test "has_n_of_same_rank-ignore-find" =
  print_s
    [%sexp
      (_has_n_of_same_rank ~rank_to_ignore:(Rank.to_int Rank.Ace) 4
       @@ Array.of_list [ 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4 ]
        : int option)];
  [%expect {| (1) |}]

let%expect_test "has_n_of_same_rank-not-find" =
  print_s
    [%sexp
      (_has_n_of_same_rank 5
       @@ Array.of_list [ 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4 ]
        : int option)];
  [%expect {| () |}]

let has_n_of_same_rank ?(rank_to_ignore = 13) n (t : t) =
  _has_n_of_same_rank ~rank_to_ignore n t.rank_counts

let has_5_same_suit t =
  Array.find_mapi
    ~f:(fun i x ->
      match Int.popcount x |> Int.( <= ) 5 with
      | true -> Suit.of_int i
      | false -> None)
    t.suit_rank_bitmaps

let top_bits bitmap ~n =
  let rec _top_bits bitmap acc =
    match Int.popcount bitmap <= n with
    | true -> Int.shift_left bitmap acc
    | false -> _top_bits (Int.shift_right bitmap 1) (1 + acc)
  in
  _top_bits bitmap 0

let%expect_test "top_bits-1" =
  print_s [%sexp (top_bits 4096 ~n:1 : int)];
  [%expect {| 4096 |}]

let%expect_test "top_bits-2" =
  print_s [%sexp (top_bits 4095 ~n:1 : int)];
  [%expect {| 2048 |}]

let get_kickers ?(ranks_to_ignore = [||]) n (t : int) =
  Array.fold ~init:t
    ~f:(fun bitmap rank -> Int.( lxor ) (Int.shift_left 1 rank) bitmap)
    ranks_to_ignore
  |> top_bits ~n

let%expect_test "get_kickers-4832-11" =
  print_s [%sexp (get_kickers ~ranks_to_ignore:[| 11 |] 1 4832 : int)];
  [%expect {| 4096 |}]

let%expect_test "get_kickers-4832-12" =
  print_s [%sexp (get_kickers ~ranks_to_ignore:[| 12 |] 1 4832 : int)];
  [%expect {| 512 |}]

let%expect_test "get_kickers-4832-12,11" =
  print_s [%sexp (get_kickers ~ranks_to_ignore:[| 12; 11 |] 1 4832 : int)];
  [%expect {| 2048 |}]

let empty_suit_rank_bitmaps () = Array.create ~len:4 0
let empty_rank_counts () = Array.create ~len:13 0
let combine bitmaps = Array.reduce_exn ~f:Int.bit_or bitmaps
let rank_bitmap (t : int array) suit = t.(Suit.to_int suit)

let suit_rank_bitmaps (cards : Card.t list) =
  let suit_rank_bitmap_array = empty_suit_rank_bitmaps () in
  List.iter cards ~f:(fun (suit, rank) ->
      suit_rank_bitmap_array.(Suit.to_int suit) <-
        suit_rank_bitmap_array.(Suit.to_int suit) + bitmap_of_rank rank);
  suit_rank_bitmap_array

let rank_counts (cards : Card.t list) =
  let counts = empty_rank_counts () in
  List.iter cards ~f:(fun (_, rank) ->
      counts.(Rank.to_int rank) <- counts.(Rank.to_int rank) + 1);
  counts

let of_cards (cards : Card.t list) =
  let first = suit_rank_bitmaps cards in
  {
    suit_rank_bitmaps = first;
    rank_counts = rank_counts cards;
    rank_bitmap = combine first;
    cards;
  }

let straight_bitmaps =
  Array.of_list_map
    ~f:(List.reduce_exn ~f:Int.( + ))
    [
      [ 4096; 2048; 1024; 512; 256 ];
      [ 2048; 1024; 512; 256; 128 ];
      [ 1024; 512; 256; 128; 64 ];
      [ 512; 256; 128; 64; 32 ];
      [ 256; 128; 64; 32; 16 ];
      [ 128; 64; 32; 16; 8 ];
      [ 64; 32; 16; 8; 4 ];
      [ 32; 16; 8; 4; 2 ];
      [ 16; 8; 4; 2; 1 ];
      [ 8; 4; 2; 1; 4096 ];
    ]

let%expect_test "hand-to-int-two" =
  print_s [%sexp (bitmap_of_rank Rank.Two : int)];
  [%expect {| 1 |}]

let%expect_test "hand-to-int-ace" =
  print_s [%sexp (bitmap_of_rank Rank.Ace : int)];
  [%expect {| 4096 |}]

let%expect_test "rank-bitmap-1" =
  print_s
    [%sexp
      (Array.map ~f:bitmap_of_rank [| Rank.Ace; Rank.Ace |] |> combine : int)];
  [%expect {| 4096 |}]

let%expect_test "rank-bitmap-2" =
  print_s
    [%sexp
      (Array.map ~f:bitmap_of_rank [| Rank.Two; Rank.Ace |] |> combine : int)];
  [%expect {| 4097 |}]

let%expect_test "bitmaps-of-t-1" =
  print_s
    [%sexp
      ([
         (Suit.Hearts, Rank.Two);
         (Suit.Spades, Rank.Two);
         (Suit.Hearts, Rank.Four);
         (Suit.Hearts, Rank.King);
         (Suit.Hearts, Rank.Ace);
       ]
       |> suit_rank_bitmaps
        : int array)];
  [%expect {| (0 0 6149 1) |}]

let%expect_test "bitmaps-of-hearts-1" =
  let bitmaps =
    [
      (Suit.Hearts, Rank.Two);
      (Suit.Spades, Rank.Two);
      (Suit.Hearts, Rank.Four);
      (Suit.Hearts, Rank.King);
      (Suit.Hearts, Rank.Ace);
    ]
    |> suit_rank_bitmaps
  in
  print_s [%sexp (rank_bitmap bitmaps Suit.Hearts : int)];
  [%expect {| 6149 |}]

let%expect_test "rank-counts-1" =
  print_s
    [%sexp
      ([
         (Suit.Hearts, Rank.Two);
         (Suit.Spades, Rank.Two);
         (Suit.Hearts, Rank.Four);
         (Suit.Hearts, Rank.King);
         (Suit.Hearts, Rank.Ace);
       ]
       |> rank_counts
        : int array)];
  [%expect {| (2 0 1 0 0 0 0 0 0 0 0 1 1) |}]
