# JSON Structural Tape

[Русский](JSON-Structural-Tape-RU) · [Home](Home) · [Русская главная](Home-RU)

Tape is a `Dynarray` of records: tag, absolute `source_offset`, source `length`, and optional `link`. Atom roles are Key and Value. No JSON object tree is built and source bytes are not copied into entries. The source must remain available if consumers later retrieve the referenced bytes.

The stack stores container frames with an opening Tape index and a grammar state. It is not the output tree. Advancing within a container replaces its top frame state; nesting pushes a frame; closing pops it.

On opening, append a record and save its index. On closing, use that index directly: update the opening entry, append the closing entry, and set reciprocal links. No Tape search is needed for construction. Atoms have one entry and `link=None`.

Current range convention: an opening entry covers its whole container, including brackets; a closing entry covers its one-byte delimiter. Keys include both quotes. This differs from the early imported plan's one-byte opening entries; that snapshot is retained as history.

The dispatcher opens nested containers and resumes parents on closing delimiters or commas. Container parsers consume their own closing delimiter. An opening delimiter handed to the dispatcher is left unread.

Tests cover empty containers, simple atoms, multiple fields/elements, nested containers and selected continuation-after-child cases. Whitespace handling recognizes space, tab, LF and CR. A combined test checks all 11 Tape entries, offsets and links, including a space inside a key. After an array comma, `ExpectingArrayValue` delays atom creation until a non-whitespace byte arrives.

Limitations: quoted-value parsing and escaping are not implemented; simple atom lengths still include trailing whitespace before separators. Window continuation and malformed-input behavior are incomplete. Current success checkpoints return `Wip`; public completion semantics are not finalized. `Cstruct.sub`, context records and stack cells still incur potential allocation costs.
