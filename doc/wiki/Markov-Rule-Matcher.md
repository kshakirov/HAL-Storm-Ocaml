# Markov Rule Matcher

[Русский](Markov-Rule-Matcher-RU) · [Home](Home) · [Русская главная](Home-RU)

Patterns and replacement placeholders are `Cstruct.t` pairs. `match_rule` compares the pattern against the input prefix directly by byte. It no longer converts candidate prefixes to strings. Rules are checked in list order at each position; the scanner advances left to right and returns the first match and remaining input view.

This is a matcher prototype, not a completed normal Markov algorithm. Replacement application, terminal rules and a consumed-source-length result are not implemented. The existing test checks a match of `Te` in `Tell me why`.

Zero-copy is not zero-allocation: comparison and scanning currently create subviews through `Cstruct.sub`; rules are searched again at each position. No allocation benchmark has established zero overhead.

The next experiment uses positional rules to locate JSON atom boundaries, especially strings. Proposed handling of escaped quote and escaped backslash patterns, terminal quote rules, temporary substitutions and restoration is unresolved. Rules split across windows must distinguish incomplete input from mismatch. Preserve original source coordinates regardless of replacement length. See [Research](Research).
