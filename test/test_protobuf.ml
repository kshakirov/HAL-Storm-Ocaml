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
  |VarIntVal of int
  |SliceVal of Cstruct.t


let add_little_endian prev_b new_b  shift =
  let payload = new_b land 0x7f in
  let shifted_payload = payload lsl shift in
  shifted_payload + prev_b
(*one more parameter needed field number*)

let rec parse_delim_length (seq:Cstruct.t) values_tuple field_number length iv =
  match Cstruct.length seq - length with
  |l when l < 0  -> (seq, values_tuple, iv)
  |_ -> 
    (Cstruct.sub seq length (Cstruct.length seq - length), (field_number, SliceVal (Cstruct.sub seq 0 length) ):: values_tuple , iv)



let rec parse_varint (t:Cstruct.t) values_tuple iv field_number shift=
  match Cstruct.length t with
  |0 -> (t, values_tuple, iv)
  |_ ->
    let b = Cstruct.get_byte t 0 in
    (* Printf.printf "%d\n" b; *)
    match  b  with
    | x when  x land 0x80 = 0  -> (Cstruct.sub t 1 (Cstruct.length t - 1), (field_number, VarIntVal (add_little_endian iv x shift)):: values_tuple , iv)
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
             let varint_val =  
             match snd (List.hd tmp_values_tuple) with
             |VarIntVal v -> v
             |_->  0 in
             let field_number = fst (List.hd tmp_values_tuple) in
             (* (seq, values_tuple, intermediate_value) *)
             parse_delim_length seq values_tuple field_number varint_val 0

      
    |_ -> (seq, values_tuple, intermediate_value)
      
  



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->

                  let test_buffer = Cstruct.of_hex "089601" in 
                  let (_seq, values_tuple, _iv)  = parse_protobuf test_buffer [] 0 in
	          assert(1 = fst (List.hd values_tuple));
                  assert((VarIntVal 150)  = snd (List.hd values_tuple));
                  Printf.printf "Working with Protobufs";
