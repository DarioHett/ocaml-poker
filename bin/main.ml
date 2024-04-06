open Core
open Ocaml_poker

let () =
  let deck = Deck.shuffle Ocaml_poker.Deck.deck in
  let cards = List.take deck 7 in
  let summary = Cardsummary.of_cards cards in
  let hand = Hand.analyse_hand summary in
  match hand with
  | Some hand -> Hand.sexp_of_t hand |> Sexp.to_string |> print_endline
  | _ -> failwith (Cardsummary.sexp_of_t summary |> Sexp.to_string)
