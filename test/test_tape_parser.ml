open Printf


type atom_role = Key | Value [@@deriving show]

type tag =
  | OpenObject | CloseObject
  | OpenArray  | CloseArray
  | Atom of atom_role
  [@@deriving show]

type tape_entry = {
    tag : tag;
    source_offset : int;       (* Позиция начала элемента в исходном потоке. *)
    length : int;              (* Длина элемента в исходном потоке. *)
    mutable link : int option; (* Индекс парной записи в Tape, если она уже известна. *)
  } [@@deriving show]


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
  [@@deriving show]

let json_obj = {|{"a":2}|}
let json_array = {|{"a":[1,2]}|}
let json_empty_array = {|[]|}
let json_1_array = {|[1]|}
let json_objec_array_empty = {|[{"a":2}]|}
let json_empty = {|{}|}



let rec parse_array (seq: Cstruct.t) (p_state: parse_contex) =
  match Cstruct.length seq with
  | 0 -> (NeedsMoreData, p_state,seq)
  | _ ->
  let o = Cstruct.get_byte seq 0 in
  match o, p_state.stack with
  | 123, ArrayFrame{open_index;state=ReadingValueOrClose}::rest ->
    Printf.printf  "A 123  %d %d \n"  o (Dynarray.length p_state.tape );
    (Wip,  {position=p_state.position; stack = ArrayFrame{open_index=open_index;state=ReadingValueOrClose }:: rest; tape =p_state.tape},seq)
  | 93, ArrayFrame{open_index;state=ReadingValueOrClose}::rest ->
    let open_array_elem = Dynarray.get  p_state.tape open_index in
    Dynarray.set  p_state.tape open_index {source_offset=open_array_elem.source_offset; length=p_state.position + 1 - open_array_elem.source_offset;link=Some (Dynarray.length p_state.tape);tag=open_array_elem.tag};
    Dynarray.add_last p_state.tape {source_offset=p_state.position;length=1;link= Some (open_index );tag=CloseArray};
    (Wip,  {position=p_state.position + 1; stack = rest; tape =p_state.tape},Cstruct.sub seq 1 (Cstruct.length seq - 1))
  (* | 44, ArrayFrame{open_index;state=ReadingCommaOrClose}::rest -> 1 *)
  |_, ArrayFrame{open_index;state=ReadingValueOrClose}::rest ->
    let new_frame = ArrayFrame{open_index;state=ReadingValue } in
    Dynarray.add_last p_state.tape {source_offset=p_state.position;length=0;link= None;tag=Atom Value};
    parse_array (Cstruct.sub seq 1 (Cstruct.length seq - 1)) {position=p_state.position + 1;stack=(new_frame::rest); tape=p_state.tape}
      
  | 93, ArrayFrame{open_index;state=ReadingValue}::rest ->
    let value_array_elem =Dynarray.get_last p_state.tape in
    let open_array_elem = Dynarray.get  p_state.tape open_index in
    Dynarray.set p_state.tape (Dynarray.length p_state.tape - 1) {source_offset=value_array_elem.source_offset;length=p_state.position  - value_array_elem.source_offset;link= None;tag=value_array_elem.tag};
    Dynarray.set  p_state.tape open_index {source_offset=open_array_elem.source_offset; length=p_state.position + 1 - open_array_elem.source_offset;link=Some (Dynarray.length p_state.tape);tag=open_array_elem.tag};
    Dynarray.add_last p_state.tape {source_offset=p_state.position;length=1;link= Some (open_index);tag=CloseArray};

    (Wip,  {position=p_state.position + 1; stack = rest; tape =p_state.tape},Cstruct.sub seq 1 (Cstruct.length seq - 1))
  |_, ArrayFrame{open_index;state=ReadingValue}::rest ->
    
    parse_array (Cstruct.sub seq 1 (Cstruct.length seq - 1)) {position=p_state.position + 1;stack=ArrayFrame{open_index; state=ReadingValue}::rest; tape=p_state.tape}

    
  |_-> (Error, p_state,seq)



let rec parse_object (seq: Cstruct.t) (p_state: parse_contex) =
  (* 34 = quote 44 =, 58=: 91= [, 125=} *)
  match Cstruct.length seq with
  | 0 -> (NeedsMoreData, p_state,seq)
  | _ ->
  let o = Cstruct.get_byte seq 0 in
  match o, p_state.stack with
  | 34, ObjectFrame{open_index;state=ReadingKeyOrClose}::rest ->
     let pos = p_state.position + 1 in
     let new_frame = ObjectFrame{open_index;state=ReadingKey } in
     Dynarray.add_last p_state.tape {source_offset=p_state.position;length=0;link=None;tag=Atom Key};

     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=pos;stack=(new_frame :: rest);tape=p_state.tape}

  | 34, ObjectFrame{open_index;state=ReadingKey}::rest -> 
     let pos = p_state.position + 1 in
     let new_frame = ObjectFrame{open_index;state=ReadingColon } in
     let tape_elem = Dynarray.get_last  p_state.tape  in
     Dynarray.set p_state.tape (Dynarray.length  p_state.tape  - 1) {source_offset=tape_elem.source_offset; length=pos - tape_elem.source_offset;link= None;tag=tape_elem.tag};

     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=pos;stack=(new_frame :: rest);tape=p_state.tape}

  | _, ObjectFrame{open_index;state=ReadingKey}::rest -> 
     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=p_state.position + 1;stack=(ObjectFrame{open_index;state=ReadingKey}::rest);tape=p_state.tape}

  | 58, ObjectFrame{open_index;state=ReadingColon}::rest ->

    let pos = p_state.position + 1 in
    let new_frame = ObjectFrame{open_index;state=ReadingCommaOrClose } in
    Dynarray.add_last p_state.tape {source_offset=pos;length=0;link=None;tag=Atom Value};
     
     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=pos;stack=(new_frame::rest);tape=p_state.tape}
(* this below is not correct it requires quote to read *)
  | 44, ObjectFrame{open_index;state=ReadingCommaOrClose}::rest -> 
     let pos = p_state.position + 1 in
     let new_frame = ObjectFrame{open_index;state=ReadingKey } in
     let last  = Dynarray.get_last  p_state.tape  in
     ( match last.tag with
       | Atom Value ->       Dynarray.set p_state.tape (Dynarray.length p_state.tape - 1) {source_offset=last.source_offset;length=p_state.position -  last.source_offset;link=None;tag=Atom Value};
       |_ -> ();
     );

     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=pos;stack=(new_frame :: rest);tape=p_state.tape}



  (* |58,ObjectFrame{open_index;state=ReadingColon}::rest -> let pos = p_state.position + 1 in *)
  (*        pos *)
  (* | 91, ObjectFrame{open_index;state=ReadingValue}::rest -> 1 *)
  | 125, ObjectFrame{open_index;state=(ReadingKeyOrClose | ReadingCommaOrClose)}::rest ->


    let len = Dynarray.length p_state.tape in 
    let tape_elem_object = Dynarray.get  p_state.tape open_index  in
    (* Dynarray.set p_state.tape (len  - 1) {source_offset=tape_elem_object.source_offset; length=p_state.position  - tape_elem_object.source_offset;link= None;tag=tape_elem_object.tag}; *)
    let last  = Dynarray.get_last  p_state.tape  in
    ( match last.tag with
      | Atom Value ->       Dynarray.set p_state.tape (Dynarray.length p_state.tape - 1) {source_offset=last.source_offset;length=p_state.position -  last.source_offset;link=None;tag=Atom Value};
      |_ -> ();
    );

    Dynarray.set p_state.tape open_index {source_offset=tape_elem_object.source_offset; length=p_state.position + 1 - tape_elem_object.source_offset;link= Some(len);tag=tape_elem_object.tag};
    Dynarray.add_last p_state.tape {source_offset=p_state.position ;length=1;link=Some open_index;tag=CloseObject};


    (Wip, {position=p_state.position + 1; stack= rest; tape=p_state.tape}, (Cstruct.sub seq 1 (Cstruct.length seq - 1))) 


  | _, ObjectFrame{open_index;state=ReadingCommaOrClose}::rest -> 
    Printf.printf "value %d \n" o;
     parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  )) {position=p_state.position + 1;stack=(ObjectFrame{open_index;state=ReadingCommaOrClose}::rest);tape=p_state.tape}

  (* | 125, ObjectFrame{open_index;state=ReadingCommaOrClose}::rest -> 1 *)
  (* |b, ObjectFrame{open_index;state} ::rest -> 0 *)
  |_-> (Error,p_state, seq)


let rec parse_tape (seq: Cstruct.t) (state :parse_contex) (status :parser_status)=
  match Cstruct.length seq with
  |0 -> (status, state)
  |_->
  let b = Cstruct.get_byte seq 0 in 
  match b with
  | 123 ->  let p = state.position + 1 in
            let frame = ObjectFrame{open_index=Dynarray.length state.tape;state=ReadingKeyOrClose} in
            let stack = frame ::state.stack in

            Dynarray.add_last state.tape {tag=OpenObject;source_offset=state.position;length=0;link=None};
            let (n_status, n_state,seq) = parse_object (Cstruct.sub seq 1 (Cstruct.length seq -1  ))  {position=p; stack=stack;tape=state.tape} in
            (match n_status with
             | Wip -> parse_tape  seq n_state n_status
             | _-> (Error, n_state)
            )

  | 91 -> let p = state.position + 1 in 
                let frame = ArrayFrame{open_index=Dynarray.length state.tape;state=ReadingValueOrClose} in
    let stack = frame ::state.stack in
    Printf.printf  "Array \n"  ;
    Dynarray.add_last state.tape {tag=OpenArray;source_offset=state.position;length=0;link=None};
            let (n_status, n_state,seq) = parse_array (Cstruct.sub seq 1 (Cstruct.length seq -1  ))  {position=p; stack=stack;tape=state.tape} in
            (match n_status with
             | Wip -> parse_tape  seq n_state n_status
             | _-> (Error, n_state)
            )
  | 93 ->  let (n_status, n_state,seq) = parse_array seq state in
            (match n_status with
             | Wip -> parse_tape  seq n_state n_status
             | _-> (Error, n_state)
            )

  |_  -> (Success, state)
            
    
  

let () =

let (status, state) =    parse_tape (Cstruct.of_string json_empty)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }  Start
in

assert(status =Wip);
assert(state.position = 2);
assert(List.length state.stack = 0);
assert(Dynarray.length state.tape = 2);
assert( Dynarray.get state.tape 0 = {tag=OpenObject; source_offset=0;length=2;link=Some 1});
assert( Dynarray.get state.tape 1 = {tag=CloseObject; source_offset=1;length=1;link=Some 0});
  
let (status, state) =    parse_tape (Cstruct.of_string json_obj)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }  Start
in




assert(status =Wip);
assert(state.position = 7);
assert(List.length state.stack = 0);
assert(Dynarray.length state.tape = 4);
assert( Dynarray.get state.tape 0 = {tag=OpenObject; source_offset=0;length=7;link=Some 3});
assert( Dynarray.get state.tape 3 = {tag=CloseObject; source_offset=6;length=1;link=Some 0});
assert( Dynarray.get state.tape 1 = {tag=Atom Key; source_offset=1;length=3;link=None});

let (a_status, a_state) =    parse_tape (Cstruct.of_string json_empty_array)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }  Start
in


assert(a_status =Wip);
assert(a_state.position = 2);
assert(Dynarray.length a_state.tape = 2);
assert( Dynarray.get a_state.tape 0 = {tag=OpenArray; source_offset=0;length=2;link=Some 1});
assert( Dynarray.get a_state.tape 1 = {tag=CloseArray; source_offset=1;length=1;link=Some 0});


let (a_status, a_state) =    parse_tape (Cstruct.of_string json_1_array)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }  Start
in


assert(a_status =Wip);
assert(a_state.position = 3);
assert(Dynarray.length a_state.tape = 3);
assert( Dynarray.get a_state.tape 0 = {tag=OpenArray; source_offset=0;length=3;link=Some 2});
assert( Dynarray.get a_state.tape 2 = {tag=CloseArray; source_offset=2;length=1;link=Some 0});


let (b_status, b_state) =    parse_tape (Cstruct.of_string json_objec_array_empty)  {
    position = 0;
    stack = [];
    tape = Dynarray.create ();
  }  Start
in
print_endline "Testging json objec  array";
print_int(b_state.position);

print_endline (show_parser_status  b_status);
print_int (Dynarray.length b_state.tape) ;
assert(b_status =Wip);
assert(b_state.position = 9);  
assert(Dynarray.length b_state.tape = 6);
print_endline (show_tape_entry  (Dynarray.get b_state.tape 5));
assert( Dynarray.get b_state.tape 0 = {tag=OpenArray; source_offset=0;length=9;link=Some 5});
assert( Dynarray.get b_state.tape 1= {tag=OpenObject; source_offset=1;length=7;link=Some 4});
assert( Dynarray.get b_state.tape 5= {tag=CloseArray; source_offset=8;length=1;link=Some 0});
assert( Dynarray.get b_state.tape 4= {tag=CloseObject; source_offset=7;length=1;link=Some 1});
