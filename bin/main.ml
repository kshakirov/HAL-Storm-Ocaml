open Eio.Std
open Hal_storm_lib.HttpRequestParser
open Hal_storm_lib.Parser
let rec handle_client flow (requestBuffer : Eio.Buf_read.t)  =
  let fragment = Eio.Buf_read.of_flow flow ~max_size:1024 in
  let wirthState = {state=ReqUri; offsets=[]; index= 0} in
  let (state, n_buf) = httpRequestParse requestBuffer fragment  wirthState in
  let response =
  "HTTP/1.1 200 OK\r\n\
   Content-Length: 2\r\n\
   Connection: close\r\n\
   \r\n\
   OK" in
  match state with
  |Finished ->
    traceln "Finished";
    Eio.Flow.copy_string response flow
  |Error ->
    traceln "Error";
    Eio.Flow.copy_string response flow
  |_ ->
    traceln "Other";
    handle_client flow requestBuffer

        
  (* Тут вызовем наш парсер *)

let run_server net port =
  Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 addr in
  let requestBuffer = Eio.Buf_read.of_string "" in 
  traceln "Сервер запущен на порту %d" port;


  traceln "Сервер запущен на порту %d" port;

  Eio.Net.run_server socket
    ~on_error:(fun exn ->
      traceln "Ошибка клиента: %a" Eio.Exn.pp exn)
    (fun flow _addr ->
      handle_client flow requestBuffer)


let () =
  Eio_main.run @@ fun env ->
  run_server (Eio.Stdenv.net env) 8081







