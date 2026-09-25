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


let add_little_endian prev_b new_b  shift =
  let payload = new_b land 0x7f in
  let shifted_payload = payload lsl shift in
  shifted_payload + prev_b
(*one more parameter needed field number*)

let rec parse_delim_length (t:Cstruct.t) values_tuple field_number length iv currentIndex =
  match Cstruct.length t with
  |0 -> (t, values_tuple, iv)
  |_ ->
    let _b = Cstruct.get_byte t 0 in
    match currentIndex with
    |x when x < length-> parse_delim_length (Cstruct.sub t 1 (Cstruct.length t - 1)) values_tuple field_number length iv (currentIndex + 1)
    |_ -> (Cstruct.sub t 1 (Cstruct.length t - 1), (field_number, SliceVal iv ):: values_tuple , iv)



let rec parse_varint (t:Cstruct.t) values_tuple iv field_number shift=
  match Cstruct.length t with
  |0 -> (t, values_tuple, iv)
  |_ ->
    let b = Cstruct.get_byte t 0 in
    (* Printf.printf "%d\n" b; *)
    match  b  with
    | x when  x land 0x80 = 0  -> (Cstruct.sub t 1 (Cstruct.length t - 1), (field_number, VarIntVal (Int64.of_int (add_little_endian iv x shift))):: values_tuple , iv)
    | x  -> parse_varint (Cstruct.sub t 1 (Cstruct.length t - 1))  values_tuple (add_little_endian iv x shift) field_number (shift + 7) 

     
     
(* iv  is a value which we parse from little endian *)
let parse_protobuf seq  values_tuple intermediate_value  =
  match Cstruct.length seq with
  |0 -> (seq, values_tuple, intermediate_value)
  |_ ->

    let b = Cstruct.get_byte seq 0 in
    (* Printf.printf "%d\n" b; *)
    match b with
      (* get field number from varint and pass it further*)
    |x when  x land 7 = 0 ->  parse_varint (Cstruct.sub seq 1 (Cstruct.length seq - 1)) values_tuple 0 (x lsr 3) 0
    |x when  x land 7 = 2 -> 
             let (seq, tmp_values_tuple, intermediate_value) =
               parse_varint (Cstruct.sub seq 1 (Cstruct.length seq - 1)) values_tuple 0 (x lsr 3) 0  in
             let _varint_val =  
             match snd (List.hd tmp_values_tuple) with
             |VarIntVal v -> v
             |_-> Int64.of_int 0 in
             let _field_number = fst (List.hd tmp_values_tuple) in
             (seq, values_tuple, intermediate_value)
             (* parse_delim_length seq values_tuple varint_val *)

      
    |_ -> (seq, values_tuple, intermediate_value)
      
  



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->

                  let test_buffer = Cstruct.of_hex "089601" in 
                  let (_seq, values_tuple, _iv)  = parse_protobuf test_buffer [] 0 in
	          assert(1 = fst (List.hd values_tuple));
                  assert((VarIntVal 150L)  = snd (List.hd values_tuple));
                  Printf.printf "Working with Protobufs";
