# Transport and HTTP

[Русский](Transport-and-HTTP-RU) · [Home](Home) · [Русская главная](Home-RU)

The current vertical path is Eio accept → client handler → `single_read` → `Cstruct.sub buffer 0 read_bytes` → `httpRequestParse` → `wirth_parser`.

Each connection owns a 1028-byte reusable read buffer. The updated HTTP state is passed to the next read. HTTP offsets are absolute positions; the recognizer does not accumulate request fragments. This transport has not been connected to the JSON Tape experiment.

The implementation is a prototype: HTTP syntax, framing, bodies, EOF and pipelining are incomplete. Both recognized success and error currently receive the same minimal 200 response. This is not production HTTP behavior.

A `doc/automaton_1.smv` model exists. It is not evidence of full implementation correctness; correspondence and stronger properties remain open. See [Research](Research).
