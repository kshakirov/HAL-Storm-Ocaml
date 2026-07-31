(* Состояния ДКА по Вирту для разбора первой строки HTTP *)
type http_state =
  | ParseMethod     (* Ищем метод: GET, POST... *)
  | ParseUri        (* Ищем путь: /api/v1/users... *)
  | ParseVersion    (* Ищем версию: HTTP/1.1 *)
