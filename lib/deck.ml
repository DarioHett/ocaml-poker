open Core

let deck =
  let suit_ints = List.range 0 4 in
  let rank_ints = List.range 0 13 in
  let card_ints =
    List.map
      ~f:(fun a -> List.map ~f:(fun b -> (13 * a) + b) rank_ints)
      suit_ints
    |> List.concat
  in
  List.map ~f:Card.of_int_exn card_ints

(* let shuffle ?(seed=1) deck = List.permute ~random_state:(Random.State.make [|seed|]) deck *)
let shuffle ?(seed = Random.State.default) deck =
  List.permute ~random_state:seed deck

let%expect_test "deck-length" =
  print_s [%sexp (List.length deck : int)];
  [%expect {| 52 |}]
