open Printf


type atom_role = Key | Value

type tag =
  | OpenObject | CloseObject
  | OpenArray  | CloseArray
  | Atom of atom_role

type tape_entry = {
    tag : tag;
    source_offset : int;       (* Позиция начала элемента в исходном потоке. *)
    length : int;              (* Длина элемента в исходном потоке. *)
    mutable link : int option; (* Индекс парной записи в Tape, если она уже известна. *)
  }

type tape = tape_entry Dynarray.t



type object_state =
    | ReadingKeyOrClose
    | ReadingColon
    | ReadingValue
    | ReadingCommaOrClose
    | ReadingKey


type array_state =
    | ReadingValueOrClose
    | ReadingCommaOrClose
    | ReadingValue


 type frame =
    | ObjectFrame of {
        open_index : int;      (* Индекс соответствующего OpenObject в Tape. *)
        state : object_state;  (* Текущая фаза разбора объекта. *)
      }
    | ArrayFrame of {
        open_index : int;      (* Индекс соответствующего OpenArray в Tape. *)
        state : array_state;   (* Текущая фаза разбора массива. *)
      }



type parse_contex = {
    position : int;    (* Текущая позиция в исходном потоке. *)
    stack : frame list; (* Стек ещё не закрытых объектов и массивов. *)
    tape : tape;        (* Общая изменяемая лента результата. *)

  }

type parser_status =
  |Success
  |Error
  |NeedsMoreData
  |Start
  |Wip


let json = {|{"a":[1,2]}|}

let hello  name =
  printf "hello %s" name


let rec parse_array (seq: Cstruct.t) (p_state: parse_contex) =
  let o = Cstruct.get_byte seq 0 in
  match o, p_state.stack with
  | 93, ArrayFrame{open_index;state=ReadingValueOrClose}::rest -> 1
  | 44, ArrayFrame{open_index;state=ReadingCommaOrClose}::rest -> 1
  |_-> 1



let rec parse_object (seq: Cstruct.t) (p_state: parse_contex) =
  (* 34 = quote 44 =, 58=: 91= [, 125=} *)
  match Cstruct.length seq with
  | 0 -> (NeedsMoreData, p_state)
  | _ ->
  let o = Cstruct.get_byte seq 0 in
  match o, p_state.stack with
  (* | 34, ObjectFrame{open_index;state=ReadingKeyOrClose}::rest -> 1 *)
  (* | 34, ObjectFrame{open_index;state=ReadingKey}::rest -> 1 *)
  | 44,ObjectFrame{open_index;state=ReadingCommaOrClose}::rest -> 
     let pos = p_state.position + 1 in
     let new_frame = ObjectFrame{open_index;state=ReadingKey } in
     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=pos;stack=(new_frame :: rest);tape=p_state.tape}

  (* |58,ObjectFrame{open_index;state=ReadingColon}::rest -> let pos = p_state.position + 1 in *)
  (*        pos *)
  (* | 91, ObjectFrame{open_index;state=ReadingValue}::rest -> 1 *)
  (* | 125, ObjectFrame{open_index;state=ReadingKeyOrClose}::rest ->  *)
  (* | 125, ObjectFrame{open_index;state=ReadingCommaOrClose}::rest -> 1 *)
  (* |b, ObjectFrame{open_index;state} ::rest -> 0 *)
  |_-> (Error,p_state)


let parse_tape (seq: Cstruct.t) (state :parse_contex) (status :parser_status)=
  match Cstruct.length seq with
  |0 -> (status, state)
  |_->
  let b = Cstruct.get_byte seq 0 in 
  match b with
  | 123 ->  let p = state.position + 1 in
            let frame = ObjectFrame{open_index=Dynarray.length state.tape;state=ReadingKeyOrClose} in
            let stack = frame ::state.stack in

            Dynarray.add_last state.tape {tag=OpenObject;source_offset=state.position;length=0;link=None};
            parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  ))  {position=p; stack=stack;tape=state.tape}  
            
            
  |_  -> (Success, state)
            
    
  



let (status, state) = 
   parse_tape (Cstruct.of_string json)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }  Start
  
