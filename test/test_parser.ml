open Eio.Std
open Eio.Buf_read
open Hal_storm_lib.Parser
open List


(* Функция нарезки слайса по двум оффсетам (start, stop) *)

let slice_between (str : string) (p1 : int) (p2 : int) : string =
  String.sub str p1 (p2 - p1)

(* Рекурсивная нарезка списка хедеров парами (Name, Value) *)
let rec print_headers str = function
  | p_name_start :: p_name_end :: p_val_start :: p_val_end :: rest ->
      let name  = slice_between str p_name_start p_name_end in
      let value = slice_between str p_val_start p_val_end in
      Printf.printf "Header: '%s' = '%s'\n" name value;
      print_headers str rest
  | _ -> () (* Конец хедеров *)

let print_full_request mock_request offsets =
  match List.rev offsets with
  | p0 :: p1 :: p2 :: p3 :: headers_offsets ->
      let req_method = slice_between mock_request p0 p1 in
      let uri        = slice_between mock_request (p1 + 1) p2 in
      let version    = slice_between mock_request (p2 + 1) p3 in
      Printf.printf "Method:  '%s'\n" req_method;
      Printf.printf "URI:     '%s'\n" uri;
      Printf.printf "Version: '%s'\n" version;
      print_headers mock_request headers_offsets
  | _ -> Printf.printf "Недостаточно оффсетов!\n"

let test_simple_parse () =
  Printf.printf "test started\n";
  (* let mock_request = "GET /index.html HTTP/1.1\r\nHost: localhost\r\n\r\n" in *)
  let mock_request = 
  "GET /index.html HTTP/1.1\r\n" ^
  "Host: localhost\r\n" ^
  "User-Agent: HalStorm/1.0\r\n" ^
  "Accept: */*\r\n" ^
  "\r\n" in 
  (* pure-буфер из строки, сетевых сокетов и фоновых задач нет *)
  let buf = Eio.Buf_read.of_string mock_request  in
  let r = wirth_parser buf {state=ReqMethod; offsets= [0]; index = 0} in
  (* let s = String.sub  mock_request (r.offsets | drop )  (\* подсмотрит 4 байта, буфер останется на месте *\) in  *)
  (* traceln "Method is  %s " s; *)
  (* Тут вызов твоего парсера *)
  (* let result = Parser.parse buf in *)
  (* iter (fun l -> Printf.printf "%d = %c\n" l mock_request.[if l > 0 then l - 1 else l]) (rev r.offsets ) *)
  print_full_request mock_request r.offsets
   

let () =
  (* Обязательно заворачиваем в Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
  test_simple_parse ()
