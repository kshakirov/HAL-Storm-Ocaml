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


let parse_tape (seq: Cstruct.t) (state :parser_state)=
  printf "json is %s" json
  



let () = 
   parse_tape (Cstruct.of_string json)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }
  
