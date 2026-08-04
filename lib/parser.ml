(* Состояния ДКА по Вирту для разбора первой строки HTTP *)
type http_state =
  | PanrseMethod     (* Ищем метод: GET, POST... *)
  | ParseUri        (* Ищем путь: /api/v1/users... *)
  | ParseVersion    (* Ищем версию: HTTP/1.1 *)

type http_status =
  |ReqParse
  |ReqMethod
  |ReqUriStart
  |ReqUri
  |ReqVersion[@@deriving show]


let  test_hello n  =
  n


let rec test_wirth str idx s =
  (* print_int idx; *)
  match idx, s with
  | 0, _ when String.get str 0 = 'G'  ->  test_wirth str (idx +1) ReqMethod
  | 2, _  when String.get str 2 = 'T'  -> test_wirth str (idx +1) ReqUriStart
  | _, ReqUriStart when String.get str  idx = ' ' -> test_wirth str (idx + 1)  ReqVersion
  | _ -> (idx, s )



  
