open Eio.Std
open Hal_storm_lib  (* Открываем нашу библиотеку парсеров *)

let () =
  Eio_main.run @@ fun env ->
                  (* let request_line = "GET /api/v1/users HTTP/1.1\r\n" in *)
                  let http_request =
                    "GET /api/v1/users HTTP/1.1\r\n" ^
                      "Host: example.com\r\n" ^
                        "User-Agent: Mozilla/5.0 (X11; Linux x86_64)\r\n" ^
                          "Accept: application/json\r\n" ^
                            "Accept-Language: en-US,en;q=0.9\r\n" ^
                              "Connection: keep-alive\r\n" ^
                                "\r\n" in 

                  let  s  = Parser.test_wirth http_request 0 {state=ReqParse; offsets=[0] }  in 
                  traceln "main: the length of offsets is %d" (List.length s.offsets)







