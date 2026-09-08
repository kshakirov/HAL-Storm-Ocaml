open Eio.Std
open Eio.Buf_read
open List
open Parser

type httpParserState =
  |Finished
  |Error
  |NeedsMoreData



let rec httpRequestParse (buf: Cstruct.t) (fragment: Cstruct.t) (parser_state: parserState) : (httpParserState* Cstruct.t) =
  let n_state = wirth_parser fragment parser_state in
 match ( n_state.state) with
  | Success -> (Finished, buf)
  | Error -> (Error,buf)
  | _ -> (NeedsMoreData,buf)

     
