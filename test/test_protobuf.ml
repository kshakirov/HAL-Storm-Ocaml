open Eio.Std
open Eio.Buf_read

open List

(* Явный тип для функции-обработчика полей *)

let parse_protobuf seq = seq 
  

let  frame1 = "\x08\x01"

type proto_parser_state =
  |AwaitingTag
  | ParsingVarInt of { field_number : int; iv : int; shift : int }
  | ParsingLengthDelim of { field_number : int; target_len : int }
  | GettingLength  of {iv : int; shift:int; field_number : int}
  |Finish

type protobuf_value =
  |VarIntVal of int
  |SliceVal of Cstruct.t

type 'ctx field_handler = 'ctx -> protobuf_value -> int -> 'ctx
let add_little_endian prev_b new_b  shift =
  let payload = new_b land 0x7f in
  let shifted_payload = payload lsl shift in
  shifted_payload + prev_b
(*one more parameter needed field number*)

(*  loosing here field number  *)
let rec parse_delim_length (seq:Cstruct.t)  field_number length ctx handler =
  match Cstruct.length seq - length with
  |l when l < 0  -> (ParsingLengthDelim {field_number;  target_len=length},seq,ctx)
  |_ ->
    let new_ctx = handler ctx  (SliceVal (Cstruct.sub seq 0 length)) field_number in
    (AwaitingTag, Cstruct.sub seq length (Cstruct.length seq - length), new_ctx) 

(* loosing here field number  and shift  *)
let rec get_length (t:Cstruct.t)  iv  shift field_number=
  match Cstruct.length t with
  |0 -> (GettingLength {iv=iv; shift=shift; field_number= field_number},t, iv, shift, field_number)
  |_ ->
    let b = Cstruct.get_byte t 0 in
    (* Printf.printf "%d\n" b; *)
    match  b  with
    | x when  x land 0x80 = 0  -> ( ParsingLengthDelim {field_number=field_number; target_len=(add_little_endian iv x shift)}, Cstruct.sub t 1 (Cstruct.length t - 1), (add_little_endian iv x shift), shift, field_number)
    | x  -> get_length (Cstruct.sub t 1 (Cstruct.length t - 1))  (add_little_endian iv x shift) (shift + 7) field_number



let rec parse_varint (t:Cstruct.t)  iv field_number shift ctx handler=
  match Cstruct.length t with
  |0 -> (ParsingVarInt {field_number=field_number; iv=iv; shift=shift},t, ctx)
  |_ ->
    let b = Cstruct.get_byte t 0 in
    (* Printf.printf "%d\n" b; *)
    match  b  with
    | x when  x land 0x80 = 0  ->
       let new_ctx = handler ctx  (VarIntVal (add_little_endian iv x shift)) field_number in
       (AwaitingTag ,Cstruct.sub t 1 (Cstruct.length t - 1), new_ctx)
    | x  -> parse_varint (Cstruct.sub t 1 (Cstruct.length t - 1))   (add_little_endian iv x shift) field_number (shift + 7)ctx  handler 

     
     
(* iv  is a value which we parse from little endian *)
let rec parse_protobuf state seq ctx (handler : 'ctx field_handler)  =
  match Cstruct.length seq with
  |0 -> (state, seq, ctx)
  |_ -> match state with
        |ParsingVarInt {field_number; iv ;shift } -> parse_varint  seq  iv field_number shift ctx handler
        |ParsingLengthDelim {field_number; target_len } -> parse_delim_length  seq  field_number target_len ctx handler
        (* here match the GettingLength state then call for *)
        |GettingLength {iv;shift;field_number} ->
           let (state, seq, varint_val, shift, field_number) =
             get_length seq  iv shift field_number in
           (match state with
           |GettingLength {iv;shift;field_number} -> parse_protobuf state seq ctx  handler
           |_ -> parse_delim_length seq  field_number varint_val ctx  handler
           )
        |_ -> 
    
(* not sure about field number above the rest of the values must be there *)
    let b = Cstruct.get_byte seq 0 in
    match b with
      (* get field number from varint and pass it further*)
    |x when  x land 7 = 0 -> parse_varint (Cstruct.sub seq 1 (Cstruct.length seq - 1))  0 (x lsr 3) 0 ctx handler


    |x when  x land 7 = 2 ->
             let field_number =     (x lsr 3) in 
             let (state, seq, varint_val, shift, field_number) =
               get_length (Cstruct.sub seq 1 (Cstruct.length seq - 1))  0  0 field_number in
             parse_protobuf state seq ctx  handler


      
    |_ -> (Finish, seq, ctx)
      




let () =
  (* Обязательно заворачиваем вn Eio_main, чтобы работал Eio.Buf_read *)
  Eio_main.run @@ fun _env ->

                  let test_buffer = Cstruct.of_hex "089601" in
                  let ctx = 0 in
                  let value_handler counter _protobuf_value _field_number  = counter + 1 in 
                  let (_state,_seq, ctx)  =  parse_protobuf  AwaitingTag test_buffer ctx value_handler in
	          assert(ctx = 1);
                  (* assert((VarIntVal 150)  = snd (List.hd values_tuple)); *)
                   (* Тест 2: Length-delimited (Wire Type 2) *)
                  let test_buffer_type2 = Cstruct.of_hex "120568656c6c6f" in
                  let value_handler_2 list_of_values protobuf_value _field_number  = protobuf_value :: list_of_values in
                  let ctx_2 = [] in 
                  let (_state,_,ctx_3) = parse_protobuf AwaitingTag test_buffer_type2 ctx_2 value_handler_2 in
                  match ctx_3 with
                  |[SliceVal slice] -> assert (Cstruct.to_string slice = "hello")
                  |_-> assert(1 = 0);
                  (* let slice = match snd (List.hd values_) with SliceVal s -> s | _ -> failwith "Expected SliceVal" in *)
                  (* assert (Cstruct.to_string slice = "hello"); *)
                  Printf.printf "Working with Protobufs";

                  let test_slice_1 = Cstruct.of_hex "1282" in
                  let (state_1,_,ctx) = parse_protobuf AwaitingTag test_slice_1 ctx value_handler in
                  assert(state_1 = GettingLength {
    iv = 2;
    shift = 7;
    field_number = 2;
  }
                    );

                  let test_slice_2 =
                    Cstruct.of_hex
                      ("01" ^ String.concat "" (List.init 130 (fun _ -> "41")))
                  in

                  let (state,_,ctx) = parse_protobuf state_1 test_slice_2 ctx value_handler in
                  assert(state = AwaitingTag)
                  
