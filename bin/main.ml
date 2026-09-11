open Eio.Std
open Hal_storm_lib.HttpRequestParser
open Hal_storm_lib.Parser
let buffer_size = 1028
let rec handle_client flow (requestBuffer : Cstruct.t) (wirthState : parserState) =

  let read_bytes =Eio.Flow.single_read flow requestBuffer in
  match read_bytes with
  |0 ->
    traceln "Zero bytes read, finishing ";
  | _ -> 

     let fragment = Cstruct.sub requestBuffer 0 read_bytes in
     let (newState, n_buf, newWirthState) = httpRequestParse fragment  wirthState in
     let response =
       "HTTP/1.1 200 OK\r\n\r\n" in
     match newState with
     |Finished ->
       traceln "Finished";
       Eio.Flow.copy_string response flow
     |Error ->
       traceln "Error";
       Eio.Flow.copy_string response flow
     |_ ->
       traceln "Other";
       handle_client flow fragment newWirthState


(* Тут вызовем наш парсер *)

let run_server net port =
  Switch.run @@ fun sw ->
                let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
                let socket = Eio.Net.listen net ~sw ~backlog:128 addr in
                let _requestBuffer = Cstruct.create 1028 in (*this one we may not need at all but for the time being*)

                traceln "Сервер запущен на порту %d" port;

                Eio.Net.run_server socket
                  ~on_error:(fun exn ->
                    traceln "Ошибка клиента: %a" Eio.Exn.pp exn)
                  (fun flow _addr ->
                    handle_client flow (Cstruct.create buffer_size) {state=ReqUri; offsets=[]; index= 0} )


let () =
  Eio_main.run @@ fun env ->
                  run_server (Eio.Stdenv.net env) 8081







