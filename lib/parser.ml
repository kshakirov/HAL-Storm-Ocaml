open Eio.Std(* Состояния ДКА по Вирту для разбора первой строки HTTP *)
open Eio
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

type parser_stage =
  |Finished
  |NeedMoreData
  |Malformed



let  test_hello n  =
  n

type parserState = {state : http_status ; offsets: int list; index: int}

let check_method_3 str  =
  match  String.capitalize_ascii(String.sub str 0 3)  with
  |"get" -> true
  |"put" -> true
  |_-> false
  


let rec wirth_parser (origBuf: Cstruct.t  )  (s: parserState) : parserState =
  match Cstruct.length origBuf with
  |0   ->  s 
  |_ ->                           
    let ch = Cstruct.get_uint8 origBuf 0 in
    let buf = Cstruct.sub origBuf 1 (Cstruct.length origBuf - 1) in
  (* traceln "ch is %c" ch ;     *)
  match ch, s.state with
  | 32, ReqMethod  -> wirth_parser buf {state=ReqUri; offsets= cons (s.index + 1) (cons s.index  s.offsets); index= (s.index + 1)}
  | x , ReqMethod when  s.index < 9 -> wirth_parser buf {state=ReqMethod; offsets= s.offsets; index= (s.index + 1)}
  | x , ReqMethod when  s.index > 9 -> {state=Error; offsets= s.offsets; index= s.index }
  | 32, ReqUri  -> wirth_parser buf {state=ReqVersion; offsets= cons (s.index + 1) (cons s.index  s.offsets); index= (s.index + 1)}
  | x , ReqUri  -> wirth_parser buf {state=ReqUri; offsets= s.offsets; index= (s.index + 1)}
  | 13, ReqVersion  -> wirth_parser buf {state=ExpectCRLF; offsets= cons s.index  s.offsets; index= s.index + 1}
  | x, ReqVersion  -> wirth_parser buf {state=ReqVersion; offsets= s.offsets; index= s.index + 1}
  | 10,ExpectCRLF   -> wirth_parser buf {state=HeaderName; offsets= cons (s.index + 1)  s.offsets; index= (s.index + 1)}
  | 13,ExpectCRLF -> wirth_parser buf {state=Success; offsets=  s.offsets; index=(s.index + 1)}
  | 58, HeaderName -> wirth_parser buf {state=HeaderValue; offsets= cons (s.index + 1) (cons s.index s.offsets); index=(s.index + 1)}
  | 13, HeaderName -> wirth_parser buf {state=Success; offsets=  s.offsets; index=(s.index + 1)}
  | x, HeaderName -> wirth_parser buf {state=HeaderName; offsets=  s.offsets; index=(s.index + 1)}
  | 13, HeaderValue -> wirth_parser buf {state=ExpectCRLF; offsets= cons s.index  s.offsets; index=(s.index + 1)}
  | x, HeaderValue -> wirth_parser buf {state=HeaderValue; offsets=  s.offsets; index=(s.index + 1)}                        
  | _, Success -> s
  |_ -> s

   
