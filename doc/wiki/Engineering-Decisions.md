# Engineering Decisions

[Русский](Engineering-Decisions-RU) · [Home](Home) · [Русская главная](Home-RU)

This is the central registry of accepted architectural decisions. New decisions use Context, Decision, Rationale, Consequences and Status. Implemented means the decision is present in code; it does not imply complete protocol correctness or proven performance.

| ADR | Title | Status |
|---|---|---|
| [ADR-0001](ADR-0001-Native-OCaml-5) | Migration to native OCaml 5 | Implemented — platform migration |
| [ADR-0002](ADR-0002-Positional-Markov) | Positional Markov rules for JSON strings | Accepted — partially implemented |

The original ADR remains unchanged in `doc/adr/0001-migration-to-native-ocaml5.md`. The Wiki provides a structured reading guide and translation, not a renumbered replacement. Its original status is Approved, July 2026. Some performance claims in that historical text were aspirations, not measurements.

ADR-0002 accepts the positional rule contract; ownership and buffering choices remain in [Research](Research). New ADRs should use the [template](ADR-Template).
