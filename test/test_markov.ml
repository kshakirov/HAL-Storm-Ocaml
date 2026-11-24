 open Eio.Std
open Eio.Buf_read
open Hal_storm_lib.Markov_parser
open List

let test_markov  =
  let (r, str, buf) = markov_parse (Cstruct.of_string "Tell me why") [(Cstruct.of_string "Te", Cstruct.of_string "AA")]  in
  assert(r);
  Printf.printf "result  is %b %s %s" r (Cstruct.to_string  (fst str) ) (Cstruct.to_string (snd str))



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
                  test_markov
