open Eio.Std
open Hal_storm_lib  (* Открываем нашу библиотеку парсеров *)

let () =
  Eio_main.run @@ fun env ->

                  let idx = Parser.test_wirth "tell me why" 0  in
                  traceln "%d" idx







