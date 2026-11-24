# ADR-0001 — Migration to native OCaml 5

[Русский](ADR-0001-Native-OCaml-5-RU) · [Home](Home) · [Русская главная](Home-RU)

## Context

The earlier F#/.NET experiment encountered compiler and runtime constraints around byref-like data and generic parser composition. The original July 2026 ADR records that motivation.

## Decision

Move implementation to OCaml 5, native compilation via ocamlopt, Dune builds and Eio-based asynchronous transport.

## Rationale

Use a native functional implementation with explicit state transitions and effect-based I/O to explore specialized stream recognizers.

## Consequences

The repository now contains OCaml sources, an Eio server and parser experiments. Compatibility with .NET libraries is lost. Native compilation does not itself prove zero allocation, universal monomorphization, or a specific generated jump table. Those require measurement or inspection.

## Status

Implemented for platform migration. Original decision: Approved, July 2026. This is a structured guide to the unchanged historical ADR at `doc/adr/0001-migration-to-native-ocaml5.md`; experimental performance claims are not upgraded to established facts.
