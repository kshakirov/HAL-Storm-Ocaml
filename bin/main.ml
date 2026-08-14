open Eio.Std

let handle_client flow =
  let buf = Eio.Buf_read.of_flow flow ~max_size:4096 in
  traceln "Клиент подключен %d"  (Eio.Buf_read.buffered_bytes buf )
  (* Тут вызовем наш парсер *)

let run_server net port =
  Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 addr in
  traceln "Сервер запущен на порту %d" port;
  
  (* accept_fork сам работает как бесконечный цикл *)
      Eio.Net.accept_fork socket ~sw
  ~on_error:(fun exn -> traceln "Ошибка сокета: %a" Eio.Exn.pp exn)
  (fun flow _addr ->
    try handle_client flow
    with exn -> traceln "Ошибка клиента: %a" Eio.Exn.pp exn
  )

let () =
  Eio_main.run @@ fun env ->
  run_server (Eio.Stdenv.net env) 8081







