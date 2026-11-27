# ADR-0002 — Positional Markov rules for JSON string boundaries

## Context

The JSON Tape parser needs a source-byte range for a string without building a decoded object. The current matcher scans positions left to right, tries rules in list order at each position and mutates shared Cstruct memory on ordinary matches. A possible one-byte backslash rule makes the prototype's one-byte-pattern termination test ambiguous.

## Decision

- Keep rules as pairs of Cstruct pattern and replacement. Patterns are nonempty and initialized outside the scanning loop.
- Scan left to right; at each position choose the first matching rule in list order. After an ordinary replacement, retry rules at the same position; advance one byte when none matches. This is our positional variant, not a claim of equivalence to the classical global rule-priority algorithm.
- Ordinary rules use nonempty replacements of exactly the pattern's byte length. Their replacements must not cause endless reapplication at the same position.
- An empty replacement marks a terminal rule: report the match and stop without replacing or deleting its pattern. This supersedes the prototype's pattern-length test. Empty replacement is metadata in this contract, not a deletion operation.
- For the current JSON string rule set, escaped quote and escaped backslash rules precede the terminal unescaped quote rule, which is last in the list. Enter after the opening quote. Stop at the first unescaped closing quote; leave later input untouched.
- Track original-byte positions. The current result is the terminal quote position plus a view starting at that quote; range/consumption conversion must explicitly account for the caller's starting offset and inclusion of both quotes.
- A lone backslash at an input-window boundary needs a distinct incomplete-input outcome. Its rule action and continuation representation remain open. It must not spin through an identity replacement or be treated as an ordinary mismatch.

## Rationale

Equal byte lengths preserve offsets. Cstruct subviews share storage, so replacement starts at offset zero of the matching view; the absolute position is not added again. Using the empty right side distinguishes terminal behavior without a third rule field while retaining ordinary one-byte patterns.

## Consequences

Ordinary mutation, rescanning and terminal-position tracking exist in the prototype. Empty-right-side termination, partial-pattern continuation, restoration and Tape integration are not implemented yet. Current tests assert success and terminal position 72; they do not yet assert all replacement bytes, multiple closing quotes or chunk equivalence. Cstruct subviews still allocate representations; zero overhead has not been measured.

### Open fork: ownership and mutation

| Option | Benefit | Cost / unresolved work |
|---|---|---|
| Copy the string/input and rewrite the copy | Original bytes stay unchanged; no restoration pass | Buffer memory and copying; avoid repeated whole-prefix concatenation; define limits and partial-input handling |
| Rewrite the current window, then restore | Avoid a full string copy | Forward and reverse writes; restore on success, pause and error before buffer reuse; define ownership and cross-window matches |

No option is selected. Example reversible two-byte markers are `0x01 0x02` for escaped quote and `0x01 0x01` for escaped backslash. Raw marker bytes cannot occur in a valid JSON string; malformed input requires an explicit collision policy. Test placeholders AA/BB are not collision-safe restoration markers. Restoration must be restricted to the transformed range, preserving subsequent input.

Both options still need recognition across windows. A short window is not true EOF; accumulating bytes alone does not identify the end of a string. Source bytes referenced by Tape need an explicit lifetime policy. No implementation of the lone-backslash rule is approved by this ADR.

## Status

Accepted, 2026-11-27; partially implemented. The ownership fork and streaming continuation remain Research. Related: issue #6; JSON Tape epic #9 and array issue #10. Historical ADR-0001 is unchanged.
