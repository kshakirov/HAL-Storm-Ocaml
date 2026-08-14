open Eio.Std
open Eio.Buf_read
open Hal_storm_lib.Parser
open List
let test_simple_parse () =
  let mock_request = "GET /index.html HTTP/1.1\r\nHost: localhost\r\n\r\n" in
  (* pure-буфер из строки, сетевых сокетов и фоновых задач нет *)
  let buf = Eio.Buf_read.of_string mock_request  in
  let r = wirth_parser buf {state=ReqMethod; offsets= [0]; index = 0} in
  (* let s = String.sub  mock_request (r.offsets | drop )  (\* подсмотрит 4 байта, буфер останется на месте *\) in  *)
  (* traceln "Method is  %s " s; *)
  (* Тут вызов твоего парсера *)
  (* let result = Parser.parse buf in *)
  iter (fun l -> Printf.printf "%d\n" l) r.offsets 


let () =
  (* Обязательно заворачиваем в Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
  test_simple_parse ()
