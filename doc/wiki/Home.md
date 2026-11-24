# HAL-Storm Wiki

[Русский](Home-RU) · [Home](Home) · [Русская главная](Home-RU)

HAL-Storm is an OCaml 5 research project exploring byte-stream recognizers and structural indexes without an object AST. HTTP, protobuf and JSON are experimental test beds. It is not yet a complete protocol implementation; zero-allocation execution has not been established.

## Architecture — what exists

- [Architecture](Architecture): current components and execution paths.
- [Transport and HTTP](Transport-and-HTTP)
- [JSON Structural Tape](JSON-Structural-Tape)
- [Markov Rule Matcher](Markov-Rule-Matcher)

## Engineering Decisions — accepted decisions and history

[Engineering Decisions](Engineering-Decisions) is the central ADR registry. Existing ADR identifiers and historical documents are preserved.

## Research — unresolved questions

[Research](Research) tracks positional rules, stream boundaries, allocation costs and future specialization.

Current milestone: whitespace around JSON structure and a positional rule mechanism for atom boundaries. Documentation checked against source on 2026-11-24.
