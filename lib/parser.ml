(* Состояния ДКА по Вирту для разбора первой строки HTTP *)
type http_state =
  | PanrseMethod     (* Ищем метод: GET, POST... *)
  | ParseUri        (* Ищем путь: /api/v1/users... *)
  | ParseVersion    (* Ищем версию: HTTP/1.1 *)

let  test_hello n  =
  n

let rec test_wirth str idx =
  (* print_int idx; *)
  let c = String.get str  idx in
  if c =' '  then idx else
                   if idx >= String.length str then  -1  else 
                     test_wirth str (idx + 1)

  
