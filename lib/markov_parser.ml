open Eio.Std
open Eio.Buf_read
open List



let rec cmp (r : Cstruct.t ) (expected: Cstruct.t)  =
  match Cstruct.length expected with
  | 0 -> true
  |_-> 
  match Cstruct.get_byte r 0 = Cstruct.get_byte expected 0 with
    |true ->  if Cstruct.length expected > 1 then
        cmp (Cstruct.sub r 1 (Cstruct.length r - 1))  (Cstruct.sub  expected 1 (Cstruct.length expected - 1) )
          else true
  |_ -> false 
  




let match_rule (r : Cstruct.t) (expected : Cstruct.t) : bool =
  let len = Cstruct.length expected in
  (* Получаем доступ к текущему окну буфера без копирования *)

  if Cstruct.length r >= len then
        cmp r expected
  else
    false
  
let markov_step (buf: Cstruct.t) (rules : (Cstruct.t * Cstruct.t) list)  = 
  let found = List.find_opt (fun rule -> 
      match_rule buf (fst rule )
                     ) rules in
  match found with
  |    Some(s1, s2) ->
       
        Some((s1,s2) )
  |None -> None


    


let rec markov_parse (buf: Cstruct.t) (rules : (Cstruct.t * Cstruct.t) list) =
  match Cstruct.length buf with
  |0   ->  (false, (Cstruct.empty , Cstruct.empty), buf) 
  |_ ->
    let rule = markov_step buf rules in
    match rule with
    |  Some x -> (true, x, buf)
    | None ->
       markov_parse (Cstruct.sub  buf 1 (Cstruct.length buf - 1)) rules
                   

    

