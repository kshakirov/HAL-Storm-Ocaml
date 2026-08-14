open Eio.Std
open Hal_storm_lib.Parser
let test_simple_parse () =
  let mock_request = "GET /index.html HTTP/1.1\r\nHost: localhost\r\n\r\n" in
  (* pure-буфер из строки, сетевых сокетов и фоновых задач нет *)
  let buf = Eio.Buf_read.of_string mock_request  in
  let r = wirth_parser buf {state=ReqMethod; offsets= [0]} in 
  (* Тут вызов твоего парсера *)
  (* let result = Parser.parse buf in *)
  
  traceln "Тест успешно пройден! %d" (List.length r.offsets)

let () =
  (* Обязательно заворачиваем в Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
  test_simple_parse ()
