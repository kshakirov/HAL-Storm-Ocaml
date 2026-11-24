# Architecture

[Русский](Architecture-RU) · [Home](Home) · [Русская главная](Home-RU)

The current implementation contains separate experiments, not a single integrated universal parser.

| Component | Source | Current behavior |
|---|---|---|
| Eio transport | `bin/main.ml` | TCP port 8081, explicit reads into a reusable per-connection Cstruct |
| HTTP recognizer | `lib/parser.ml`, `lib/httpRequestParser.ml` | Byte transitions, absolute offsets, state passed between read windows |
| JSON Tape | `test/test_tape_parser.ml` | In-memory structural index, explicit container stack, nested-container examples |
| Rule matcher | `lib/markov_parser.ml` | Ordered Cstruct patterns, first matching position; no rewriting yet |
| Protobuf experiment | `test/test_protobuf.ml` | Field callback, varint, length-delimited and fixed32 experiments |

Transport windows are temporary. Parser context preserves the meaning of the consumed prefix. A field consumer chooses whether to retain decoded results. JSON intentionally retains Tape entries rather than materialized objects.

See [Transport and HTTP](Transport-and-HTTP), [JSON Tape](JSON-Structural-Tape), and [Rule Matcher](Markov-Rule-Matcher). Future compilation of composed primitives into an application automaton remains [research](Research).
