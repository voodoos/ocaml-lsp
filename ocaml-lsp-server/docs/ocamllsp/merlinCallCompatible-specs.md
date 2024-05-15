# Merlin Call Compatible Request

## Description

Allows Merlin commands to be invoked from LSP, in the same way as the
`ocamlmerlin` binary, using a custom request. Invoking this command returns the
result in the form of a character string (which can be JSON or SEXP)
representing the result of a Merlin command. This makes it possible to implement
clients capable of fallbacking on Merlin in the event of a missing feature.

### Why this custom request needed

A temporary solution to ensure that LSP can be used as a primary front-end as
quickly as possible. The interest in this query will diminish as iterations
attempt to achieve parity of functionality with `ocamlmerlin`.

## Client capability

nothing that should be noted. But each Merlin command should be handled
specifically by customers.

## Server capability

property name: `handleMerlinCallCompatible`

property type: `boolean`

## Request

- method: `ocamllsp/merlinCallCompatible`
- params:

```json
{
  "uri": DocumentUri,
  "command": string,
  "args": string[],
  "resultAsSexp": boolean
}
```

- `uri`: is the reference of the current document
- `command`: is the name of the command invoked (ie: `case-analysis`)
- `args`: all the parameters passed to the command, by default: `[]`
- `resultAsSexp`: a flag indicating whether the result should be returned in
  SEXP (`true`) or JSON (`false`), by default: `false`

## Response

```json
{
  "resultAsSexp": boolean,
  "result": string
}
```

- `resultAsSexp`: `true` if the command was invoked with the `resultAsSexp` flag,
  `false` otherwise
- `result`: the result in string (in JSON or SEXP)


If Merlin produces an error, it will be described in the same way as the binary
in the result. If parameters are badly formed (or missing) or if the command
invoked does not exist in Merlin, invoking the request throws an exception.
