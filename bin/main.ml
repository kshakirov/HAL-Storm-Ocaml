open Eio.Std
open Hal_storm_lib  (* Открываем нашу библиотеку парсеров *)

let () =
  Eio_main.run @@ fun env ->
                  let request_line = "GET /api/v1/users HTTP/1.1\r\n" in

                  let  s  = Parser.test_wirth request_line 0 {state=ReqParse; offsets=[0] }  in 
                  traceln "%d" (List.nth s.offsets 0)







