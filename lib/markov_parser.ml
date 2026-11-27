open Eio.Std
open Eio.Buf_read
open List


let fst3 (a, _,_) = a
let snd3 (_,b,_) = b
let thd (_,_,c) = c




let rec cmp (r : Cstruct.t ) (expected: Cstruct.t)  =
  match Cstruct.length expected with
  | 0 -> true
  |_-> 
  match Cstruct.get_byte r 0 = Cstruct.get_byte expected 0 with
    |true ->  if Cstruct.length expected > 1 then
        cmp (Cstruct.sub r 1 (Cstruct.length r - 1))  (Cstruct.sub  expected 1 (Cstruct.length expected - 1) )
          else true
  |_ -> false 
  

let rec replace_tmpl_internal (buf: Cstruct.t) (tmpl: Cstruct.t) (pos : int ) length =
  match pos =length with
  | true -> ()
  |_->
    Cstruct.set_char buf pos (Cstruct.get tmpl pos);
    replace_tmpl_internal buf tmpl (pos + 1) length
    
          
  

let replace_tmpl buf tmpl = replace_tmpl_internal buf tmpl 0 (Cstruct.length tmpl)

let match_rule (r : Cstruct.t) (expected : Cstruct.t) : bool =
  let len = Cstruct.length expected in
  (* Получаем доступ к текущему окну буфера без копирования *)

  if Cstruct.length r >= len then
        cmp r expected
  else
    false
  
let markov_step (buf: Cstruct.t) (rules : (Cstruct.t * Cstruct.t) list) (pos : int) = 
  let found = List.find_opt (fun rule -> 
      match_rule buf (fst rule )
                     ) rules in
  match found with
  |    Some(s1, s2) ->
       replace_tmpl buf s2;
        Some((s1,s2,pos) )
  |None -> None


    


let rec markov_parse (buf: Cstruct.t) (rules : (Cstruct.t * Cstruct.t) list) (pos : int) =
  match Cstruct.length buf with
  |0   ->  (false, (Cstruct.empty , Cstruct.empty, pos), buf) 
  |_ ->
    let rule = markov_step buf rules pos in
    match rule with
    |  Some x -> (true, x, buf)
    | None ->
       markov_parse (Cstruct.sub  buf 1 (Cstruct.length buf - 1)) rules (pos + 1)
                   

    

