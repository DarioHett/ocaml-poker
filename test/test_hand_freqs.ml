open Core
open Ocaml_poker
open Sexplib.Std

let expected_freqs =
  [| 0.000311; 0.00168; 0.026; 0.0303; 0.0462; 0.0483; 0.235; 0.438; 0.174 |]
  |> Array.rev

let std_err n p =
  let variance = p *. (1.0 -. p) in
  variance /. n |> Float.sqrt

let%expect_test "freq 10_000" =
  let cnt = Array.init ~f:(fun _ -> 0) 9 in
  let n = 10000 in
  for i = 1 to n do
    let smry =
      Deck.shuffle ~seed:(Random.State.make [| i |]) Ocaml_poker.Deck.deck
      |> Fn.flip List.take 7 |> Cardsummary.of_cards
    in
    let hand = Hand.analyse_hand smry in
    match hand with
    | Some x ->
        let ix = Hand.to_int x in
        cnt.(ix) <- cnt.(ix) + 1
    | None -> Cardsummary.sexp_of_t smry |> Sexp.to_string |> failwith
  done;
  let observed_freqs =
    Array.map cnt ~f:(fun x ->
        Float.of_int x |> Fn.flip Float.( / ) @@ Float.of_int n)
  in
  let std_errs = Array.map expected_freqs ~f:(std_err (Float.of_int n)) in
  let errs =
    Array.zip_exn observed_freqs expected_freqs
    |> Array.mapi ~f:(fun i (x, y) -> (x -. y) /. std_errs.(i) |> Float.abs)
  in
  let in_bounds = Array.for_all ~f:(fun x -> Float.( < ) x 4.0) errs in
  print_s [%sexp (in_bounds : bool)];
  [%expect {| true |}]
