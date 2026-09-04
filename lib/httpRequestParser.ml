open Eio.Std
open Eio.Buf_read
open List
open Parser

type httpParserState =
  |Finished
  |Error
  |NeedsMoreData



let rec httpRequestParse (buf: Eio.Buf_read.t) (fragment: Eio.Buf_read.t) (parser_state: parserState) : httpParserState =

  let n_state = wirth_parser fragment parser_state in
  match ( n_state.state) with
  | Success -> Finished
  | Error -> Error
  | _ -> NeedsMoreData

     
