open Eio.Std
open Eio.Buf_read

open List


let parse_protobuf seq = seq 
  

let  frame1 = "\x08\x01"

type proto_parser_state =
  |Tag
  |VarInt
  |Payload

type protobuf_value =
  |VarIntVal of int64
  |SliceVal of Cstruct.t


let add_little_endian b = b (*just stub*)
(*one more parameter needed field number*)
let rec parse_varint (t:Cstruct.t) values_tuple iv field_number shift=
  match Cstruct.length t with
  |0 -> (t, values_tuple, iv)
  |_ ->
    let b = Cstruct.get_byte t 0 in 
    match  b  with
    | x when  x land 0x80 = 0  -> (Cstruct.sub t 1 (Cstruct.length t - 1), (field_number, VarIntVal (Int64.of_int b)):: values_tuple , iv)
    | x when  x land 0X80 = 1  -> parse_varint (Cstruct.sub t 1 (Cstruct.length t - 1))  values_tuple iv field_number (shift + 1)
    | _ ->  (t,values_tuple,iv) (* just stub*)
     
     
(* iv  is a value which we parse from little endian *)
let parse_protobuf seq  values_tuple intermediate_value  =
  match Cstruct.length seq with
  |0 -> (seq, values_tuple, intermediate_value)
  |_ ->
    let b = Cstruct.get_byte seq 0 in 
    match b with
      (* get field number from varint and pass it further*)
    |x when  x land 7 = 0 ->  parse_varint (Cstruct.sub seq 1 (Cstruct.length seq - 1)) values_tuple [] (x lsr 3) 0
    |_ -> (seq, values_tuple, intermediate_value)
      
  



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->
                  Printf.printf "Working with Protobufs"
