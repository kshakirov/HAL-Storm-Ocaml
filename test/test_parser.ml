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
  | p0 :: p1 :: p2 :: p3 ::p4::p5:: headers_offsets ->
      let req_method = slice_between mock_request p0 p1 in
      let uri        = slice_between mock_request  p2  p3  in
      let version    = slice_between mock_request p4 p5 in
      Printf.printf "Method:  '%s'\n" req_method;
      Printf.printf "URI:     '%s'\n" uri;
      Printf.printf "Version: '%s'\n" version;
      print_headers mock_request headers_offsets
  | _ -> Printf.printf "Недостаточно оффсетов!\n"

let mock_request = "GET /index.html HTTP/1.1\r\nHost: localhost\r\n\r\n" 
 
let rec test_recursive (origBuf: Cstruct.t) (state: parserState) =
   match Cstruct.length origBuf with
   |0   ->  state
   |_ ->                           
     let ch =Cstruct.sub   origBuf 0 1 in
     let buf = Cstruct.sub origBuf 1 (Cstruct.length origBuf - 1) in
     let n_state = wirth_parser ch state in
     test_recursive buf n_state

let rec cmp_offsets (one : int list) (another : int list) =
  match one, another with
  | [],[] -> true
  | x::xs, y::ys when  x = y -> cmp_offsets xs ys
  |_,_ -> false
  

let cmp_parsed_results(one: parserState) (another: parserState) =
  assert(one.index = another.index);
  assert(one.state = another.state);
  assert(List.length one.offsets = List.length another.offsets)
 

  
  
    



    
  
let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
                  (* test_simple_parse () *)
                  let state  = test_recursive ( Cstruct.of_string mock_request)  {state=ReqMethod; offsets= [0]; index = 0} in
                  let another_state = wirth_parser(Cstruct.of_string mock_request) {state=ReqMethod; offsets= [0]; index = 0}  in
                  let equal = cmp_offsets state.offsets another_state.offsets in
                  cmp_parsed_results state another_state ; assert(equal)







