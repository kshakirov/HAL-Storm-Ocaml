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



type parser_state = {
    position : int;    (* Текущая позиция в исходном потоке. *)
    stack : frame list; (* Стек ещё не закрытых объектов и массивов. *)
    tape : tape;        (* Общая изменяемая лента результата. *)

  }



let json = {|{"a":[1,2]}|}

let hello  name =
  printf "hello %s" name


let rec parse_object (seq: Cstruct.t) (p_state: parser_state) =
  let o = Cstruct.get_byte seq 0 in
  match o with
  | 125 -> 1
  | 44 -> let pos = p_state.position + 1 in

          
          (match p_state.stack with
          | ObjectFrame {open_index;state=ReadingCommaOrClose}:: rest-> 
             let new_frame = ObjectFrame{open_index;state=ReadingKey } in
             parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=pos;stack=(new_frame :: rest);tape=p_state.tape}
          |_ -> 0)
  |58 -> let pos = p_state.position + 1 in
         pos
  |_ -> 0


let parse_tape (seq: Cstruct.t) (state :parser_state)=
  let b = Cstruct.get_byte seq 0 in 
  match b with
  | 123 ->  let p = state.position + 1 in
            let frame = ObjectFrame{open_index=Dynarray.length state.tape;state=ReadingKeyOrClose} in
            let stack = frame ::state.stack in

            Dynarray.add_last state.tape {tag=OpenObject;source_offset=state.position;length=0;link=None};
            parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  ))  {position=p; stack=stack;tape=state.tape}  
            
            
  |_  -> 0
            
    
  



let i = 
   parse_tape (Cstruct.of_string json)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }
  
