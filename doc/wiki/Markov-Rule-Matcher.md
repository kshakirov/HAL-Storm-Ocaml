# Positional Markov: current prototype

[Русский](Markov-Rule-Matcher-RU) · [ADR-0002](ADR-0002-Positional-Markov)

`lib/markov_parser.ml` compares Cstruct bytes, tests rules at each position, applies equal-length replacements to shared memory and retries at the same position. On no match it advances the window and absolute position by one byte.

The current terminal test is pattern length 1. A test asserts success and closing quote position 72 after rewriting escaped pairs. AA/BB replacements appear in output but their bytes are not yet asserted.

ADR-0002 accepts empty replacement as the new terminal marker; code has not switched to it yet. Cross-window continuation, restoration, the Tape range contract and JSON integration remain unimplemented. Copying versus mutation/restoration is an open fork. Cstruct.sub representations are still created; zero allocation has not been established.
