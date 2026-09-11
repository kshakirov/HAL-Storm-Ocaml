open Eio.Std
open Eio.Buf_read
open List


let match_rule (r : Cstruct.t) (expected : string) : bool =
  let len = String.length expected in
  (* Получаем доступ к текущему окну буфера без копирования *)

  if Cstruct.length r >= len then
    (* Сравниваем подстроку нужной длины с нашей строкой *)
    Cstruct.to_string ~len r = expected
  else
    false

let markov_step (buf: Cstruct.t) (rules : (string * string) list)  = 
  let found = List.find_opt (fun rule -> 
      match_rule buf (fst rule )
                     ) rules in
  match found with
  |    Some(s1, s2) ->
        traceln "\nMatch %s\n" s1;
        Some((s1,s2) )
  |None -> None


    


let rec markov_parse (buf: Cstruct.t) (rules : (string * string) list) =
  match Cstruct.length buf with
  |0   ->  (false, (" " , " "), buf) 
  |_ ->
    let rule = markov_step buf rules in
    match rule with
    |  Some x -> (true, x, buf)
    | None ->
       markov_parse (Cstruct.sub  buf 1 (Cstruct.length buf - 1)) rules
                   

    

