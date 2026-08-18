open Eio.Std(* Состояния ДКА по Вирту для разбора первой строки HTTP *)
open Eio.Buf_read
open List

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
  |HeaderName
  |HeaderValue
  |ReqVersion[@@deriving show]



let  test_hello n  =
  n

type parserState = {state : http_status ; offsets: int list; index: int}

let check_method_3 str  =
  match  String.capitalize_ascii(String.sub str 0 3)  with
  |"get" -> true
  |"put" -> true
  |_-> false
  


let rec wirth_parser (buf: Eio.Buf_read.t )  (s: parserState) : parserState =
  let ch =any_char   buf in
  traceln "ch is %c" ch ;
  match ch, s.state with
  | ' ', ReqMethod  -> wirth_parser buf {state=ReqUri; offsets= cons s.index  s.offsets; index= (s.index + 1)}
  | x , ReqMethod when  s.index < 9 -> wirth_parser buf {state=ReqMethod; offsets= s.offsets; index= (s.index + 1)}
  | x , ReqMethod when  s.index > 9 -> {state=Error; offsets= s.offsets; index= s.index }
  | ' ', ReqUri  -> wirth_parser buf {state=ReqVersion; offsets= cons s.index  s.offsets; index= (s.index + 1)}
  | x , ReqUri  -> wirth_parser buf {state=ReqUri; offsets= s.offsets; index= (s.index + 1)}
  | '\r', ReqVersion  -> wirth_parser buf {state=ExpectCRLF; offsets= cons s.index  s.offsets; index= s.index}
  | x, ReqVersion  -> wirth_parser buf {state=ReqVersion; offsets= s.offsets; index= s.index + 1}
  | '\n',ExpectCRLF   -> wirth_parser buf {state=HeaderName; offsets= cons (s.index + 1)  s.offsets; index= (s.index + 1)}
  | '\r',ExpectCRLF -> wirth_parser buf {state=Success; offsets=  s.offsets; index=(s.index + 1)}
  | ':', HeaderName -> wirth_parser buf {state=HeaderValue; offsets= cons (s.index - 1) s.offsets; index=(s.index + 1)}
  | '\r', HeaderName -> wirth_parser buf {state=Success; offsets=  s.offsets; index=(s.index + 1)}
  | x, HeaderName -> wirth_parser buf {state=HeaderName; offsets=  s.offsets; index=(s.index + 1)}
  | '\r', HeaderValue -> wirth_parser buf {state=ExpectCRLF; offsets= cons (s.index - 1) s.offsets; index=(s.index + 1)}
  | x, HeaderValue -> wirth_parser buf {state=HeaderValue; offsets=  s.offsets; index=(s.index + 1)}                        
  | _, Success -> s
  |_ -> s

  
