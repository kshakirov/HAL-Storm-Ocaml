open Eio.Std(* Состояния ДКА по Вирту для разбора первой строки HTTP *)
open Eio.Buf_read
type http_state =
  | ParseMethod     (* Ищем метод: GET, POST... *)
  | ParseUri        (* Ищем путь: /api/v1/users... *)
  | ParseVersion    (* Ищем версию: HTTP/1.1 *)

type http_status =
  |ReqParse
  |ReqMethod
  |ReqUri
  |ExpectCRLF
  |Error
  |Success
  |ReqVersion[@@deriving show]



let  test_hello n  =
  n

type parserState = {state : http_status ; offsets: int list; index: int}

let check_method_3 str  =
  match  String.capitalize_ascii(String.sub str 0 3)  with
  |"get" -> true
  |"put" -> true
  |_-> false
  

(* let rec test_wirth str idx s:parserState = *)
(*   Printf.printf  "test_wirth: idx = %d\n" idx; *)
(*   (\* print_int idx; *\) *)
(*   match (String.get str idx), s.state with *)
(*   |_, ReqParse ->   test_wirth str (idx + 1) {state=ReqMethod;offsets=s.offsets} *)
(*   | x, ReqMethod when x <> ' '  -> test_wirth str (idx + 1) {state=ReqUri;offsets=s.offsets} *)
(*   | ' ', ReqMethod when idx= 3 &&  check_method_3 str    -> test_wirth str (idx + 1) {state=ReqUri;offsets=List.cons 4 (List.cons 2 s.offsets)} *)
(*   | ' ', ReqMethod when idx = 3 &&   not (check_method_3 str)    -> {state= Error; offsets=[-1]} *)
(*   | _, ReqUri when String.get str  idx  <> ' ' -> test_wirth str (idx + 1)  {state=ReqUri;offsets=s.offsets} *)
(*   | ' ',ReqUri -> test_wirth str  (idx + 1)  {state=ReqVersion;offsets=List.cons (idx + 1) (List.cons (idx -1) s.offsets)} *)
(*   |x, ReqVersion when x  <> '\r' -> test_wirth str (idx + 1)  {state=ReqVersion;offsets=s.offsets} *)
(*   | '\r',ReqUri -> test_wirth str  (idx + 1)  {state=ExpectCRLF;offsets=List.cons (idx - 1)  s.offsets} *)
(*   | '\n',ExpectCRLF -> test_wirth str  (idx + 1)  {state=Success;offsets=s.offsets} *)
(*   | x,ExpectCRLF  when x <> '\n'    -> {state= Error; offsets=[-1]} *)
(*   | _ -> *)
(*      Printf.printf "Successfully finishing parsing "; *)
(*      {state= Success; offsets=s.offsets} *)

let rec wirth_parser (buf: Eio.Buf_read.t )  (s: parserState) : parserState =
  let ch =any_char   buf in
  traceln "ch is %c" ch ;
  match ch, s.state with
      |_-> s 

  
