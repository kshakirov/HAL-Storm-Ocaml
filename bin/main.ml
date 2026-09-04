open Eio.Std
open Hal_storm_lib.HttpRequestParser
open Hal_storm_lib.Parser
let handle_client flow  =
  let buf = Eio.Buf_read.of_flow flow ~max_size:4096 in
  let wirthState = {state=ReqUri; offsets=[]; index= 0} in
  let state = httpRequestParse buf buf  wirthState in
  let response =
  "HTTP/1.1 200 OK\r\n\
   Content-Length: 2\r\n\
   Connection: close\r\n\
   \r\n\
   OK" in
  match state with
  |Finished ->
    traceln "Finised";
    Eio.Flow.copy_string response flow
  |_ ->
    traceln "Other";
    Eio.Flow.copy_string response flow

        
  (* Тут вызовем наш парсер *)

let run_server net port =
  Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 addr in
  traceln "Сервер запущен на порту %d" port;


  traceln "Сервер запущен на порту %d" port;

  Eio.Net.run_server socket
    ~on_error:(fun exn ->
      traceln "Ошибка клиента: %a" Eio.Exn.pp exn)
    (fun flow _addr ->
      handle_client flow)


let () =
  Eio_main.run @@ fun env ->
  run_server (Eio.Stdenv.net env) 8081







