 open Eio.Std
open Eio.Buf_read
open Hal_storm_lib.Markov_parser
open List

let buf = (Cstruct.of_string {|value with \" quotes \" and \\ double slashes, here goes the real quote "|})
let test_markov  =
  let (r, str, buf) = markov_parse buf  [(Cstruct.of_string {|\"|}, Cstruct.of_string "AA")]  0 in
  assert(r);
  Printf.printf "result  is %b %s %s %d\n" r (Cstruct.to_string  (fst3 str) ) (Cstruct.to_string (snd3 str)) (thd str);
  Printf.printf "%s \n" (Cstruct.to_string buf)
    



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
                  test_markov
