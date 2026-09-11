open Eio.Std
open Eio.Buf_read
open Hal_storm_lib.Markov_parser
open List

let test_markov  =
  let (r, str, buf) = markov_parse (Cstruct.of_string "Tell me why") [("Te", "AA")]  in
  assert(r);
  Printf.printf "reslut is %b %s %s" r (fst str ) (snd str)



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
                  test_markov
