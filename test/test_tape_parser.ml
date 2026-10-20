open Printf


type atom_role = Key | Value

type tag =
  | OpenObject | CloseObject
  | OpenArray  | CloseArray
  | Atom of atom_role

type tape_entry = {
    tag : tag;
    source_offset : int;
    length : int;
    mutable link : int option;
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
        open_index : int;
        state : object_state;
      }
    | ArrayFrame of {
        open_index : int;
        state : array_state;
      }



type parser_state = {
    position : int;
    stack : frame list;
    tape : tape;

  }



let json = {|{"a":[1,2]}|}

let hello  name =
  printf "hello %s" name


let rec parse_object (seq: Cstruct.t) (state: parser_state) =
  let o = Cstruct.get_byte seq in
  1


let parse_tape (seq: Cstruct.t) (state :parser_state)=
  let b = Cstruct.get_byte seq 0 in 
  match b with
  | 123 ->  let p = state.position + 1 in
            let frame = ObjectFrame{open_index=1;state=ReadingKey} in
            let stack = frame ::state.stack in

            Dynarray.add_last state.tape {tag=OpenObject;source_offset=state.position;length=0;link=Some 0};
            parse_object seq {position=p; stack=stack;tape=state.tape}  
            
            
  |_  -> 0
            
    
  



let i = 
   parse_tape (Cstruct.of_string json)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }
  
