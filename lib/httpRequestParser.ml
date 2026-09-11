open Eio.Std
open Eio.Buf_read
open List
open Parser

type httpParserState =
  |Finished
  |Error
  |NeedsMoreData



let rec httpRequestParse  (fragment: Cstruct.t) (parser_state: parserState) : (httpParserState* Cstruct.t * parserState) =
  let n_state = wirth_parser fragment parser_state in
 match ( n_state.state) with
  | Success -> (Finished, fragment, n_state)
  | Error -> (Error,fragment, n_state)
  | _ -> (NeedsMoreData,fragment,n_state)

     
