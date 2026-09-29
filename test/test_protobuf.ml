open Eio.Std
open Eio.Buf_read

open List


let parse_protobuf seq = seq 
  

let  frame1 = "\x08\x01"

type proto_parser_state =
  |AwaitingTag
  | ParsingVarInt of { field_number : int; iv : int; shift : int }
  | ParsingLengthVarInt of { field_number : int; iv : int; shift : int }
  | ParsingLengthDelim of { field_number : int; target_len : int }
  |Finish

type protobuf_value =
  |VarIntVal of int
  |SliceVal of Cstruct.t


let add_little_endian prev_b new_b  shift =
  let payload = new_b land 0x7f in
  let shifted_payload = payload lsl shift in
  shifted_payload + prev_b
(*one more parameter needed field number*)

(*  loosing here field number  *)
let rec parse_delim_length (seq:Cstruct.t) values_tuple field_number length iv =
  match Cstruct.length seq - length with
  |l when l < 0  -> (ParsingLengthDelim {field_number;  target_len=length},seq, values_tuple, iv)
  |_ -> 
    (AwaitingTag, Cstruct.sub seq length (Cstruct.length seq - length), (field_number, SliceVal (Cstruct.sub seq 0 length) ):: values_tuple , iv)

(* loosing here field number  and shift  *)

let rec parse_varint (t:Cstruct.t) values_tuple iv field_number shift=
  match Cstruct.length t with
  |0 -> (ParsingVarInt {field_number=field_number; iv=iv; shift=shift},t, values_tuple, iv)
  |_ ->
    let b = Cstruct.get_byte t 0 in
    (* Printf.printf "%d\n" b; *)
    match  b  with
    | x when  x land 0x80 = 0  -> (AwaitingTag, Cstruct.sub t 1 (Cstruct.length t - 1), (field_number, VarIntVal (add_little_endian iv x shift)):: values_tuple , iv)
    | x  -> parse_varint (Cstruct.sub t 1 (Cstruct.length t - 1))  values_tuple (add_little_endian iv x shift) field_number (shift + 7) 

     
     
(* iv  is a value which we parse from little endian *)
let parse_protobuf state seq  values_tuple intermediate_value  =
  match Cstruct.length seq with
  |0 -> (state, seq, values_tuple, intermediate_value)
  |_ -> match state with
        |ParsingVarInt {field_number; iv ;shift } -> parse_varint seq values_tuple iv field_number shift
        |ParsingLengthDelim {field_number; target_len } -> parse_delim_length  seq values_tuple field_number target_len 0
                                                   
        |_ -> 
    
(* not sure about field number above the rest of the values must be there *)
    let b = Cstruct.get_byte seq 0 in
    match b with
      (* get field number from varint and pass it further*)
    |x when  x land 7 = 0 ->  parse_varint (Cstruct.sub seq 1 (Cstruct.length seq - 1)) values_tuple 0 (x lsr 3) 0
    |x when  x land 7 = 2 -> 
             let (state, seq, tmp_values_tuple, intermediate_value) =
               parse_varint (Cstruct.sub seq 1 (Cstruct.length seq - 1)) values_tuple 0 (x lsr 3) 0  in
             let varint_val =  
             match snd (List.hd tmp_values_tuple) with
             |VarIntVal v -> v
             |_->  0 in
             let field_number = fst (List.hd tmp_values_tuple) in
             (* (seq, values_tuple, intermediate_value) *)
             parse_delim_length seq values_tuple field_number varint_val 0

      
    |_ -> (Finish, seq, values_tuple, intermediate_value)
      
  



let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->

                  let test_buffer = Cstruct.of_hex "089601" in 
                  let (state,_seq, values_tuple, _iv)  =  parse_protobuf  AwaitingTag test_buffer [] 0 in
	          assert(1 = fst (List.hd values_tuple));
                  assert((VarIntVal 150)  = snd (List.hd values_tuple));
                   (* Тест 2: Length-delimited (Wire Type 2) *)
                  let test_buffer_type2 = Cstruct.of_hex "120568656c6c6f" in
                  let (state,_, values_tuple2, _) = parse_protobuf AwaitingTag test_buffer_type2 [] 0 in
                  assert (fst (List.hd values_tuple2) = 2);
                  let slice = match snd (List.hd values_tuple2) with SliceVal s -> s | _ -> failwith "Expected SliceVal" in
                  assert (Cstruct.to_string slice = "hello");
                  Printf.printf "Working with Protobufs";
