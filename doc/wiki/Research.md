# Research

[Русский](Research-RU) · [Home](Home) · [Русская главная](Home-RU)

These are open questions, not accepted engineering decisions.

| Direction | Next evidence |
|---|---|
| Positional Markov rules | Implement empty-replacement termination from ADR-0002; verify source ranges and termination; issue #6 |
| String boundaries across windows | Test escaped quotes/backslashes, partial patterns and true EOF; choose continuation, accumulation or temporary substitutions explicitly |
| Whitespace and atom completion | Exclude trailing whitespace from atom ranges without discarding spaces inside strings |
| JSON Tape completeness | Verify additional nesting, separators, invalid inputs and arbitrary chunking; issues #9 and #10 |
| Allocation costs | Measure copied bytes, Cstruct view creation, context/frame allocation and matcher scans separately |
| Protobuf / DRPC | Complete partial field continuation, payload ownership and outer message framing; issue #8 |
| Formal correspondence | Align nuXmv transitions with actual parser and strengthen tested properties |
| Specialized application automata | Test whether protocol primitives compose into equivalent specialized recognizers |

No placeholder-rewriting technique has yet been accepted for JSON strings. Source retention and mutation policy must be explicit before implementation.

The existing [Russian hypothesis](hypothesis) is preserved. Imported repository snapshots under `context/` are historical references, not current implementation documentation.

[The copying/restoration fork is recorded in ADR-0002](ADR-0002-Positional-Markov).
