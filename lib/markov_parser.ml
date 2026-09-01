open Eio.Std
open Eio.Buf_read
open List


let match_rule (r : Eio.Buf_read.t) (expected : string) : bool =
  let len = String.length expected in
  (* Получаем доступ к текущему окну буфера без копирования *)
  let got = peek r in
  if Cstruct.length got >= len then
    (* Сравниваем подстроку нужной длины с нашей строкой *)
    Cstruct.to_string ~len got = expected
  else
    false

let markov_step (buf: Eio.Buf_read.t) (rules : (string * string) list)  = 
  List.find_opt (fun rule -> 
      match_rule buf (fst rule )
    ) rules
    


let rec markov_parse (buf: Eio.Buf_read.t) (rules : (string * string) list) =
  match peek_char buf with
  |None   ->  (false, (" " , " "), buf) 
  |_ ->
    let rule = markov_step buf rules in
    match rule with
    |  Some x -> (true, x, buf)
    | None ->
       let _ch = any_char buf in
       markov_parse buf rules
                   

    

