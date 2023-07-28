open Import
open Fiber.O

let capability = ("handleTypeExpr", `Bool true)

let meth = "ocamllsp/typeExpr"

module Request_params = struct
  type t = Uri.t * Position.t * string

  (* Request params must have the form as in the given string. *)
  let expected_params =
    `Assoc
      [ ("textDocument", `Assoc [ ("uri", `String "<DocumentUri>") ])
      ; ("position", `String "<Position>")
      ; ("expression", `String "<Expression>")
      ]

  let t_of_structured_json params : t option =
    match params with
    | `Assoc
        [ ("textDocument", `Assoc [ ("uri", uri) ])
        ; ("position", pos)
        ; ("expression", expr)
        ] ->
      let uri = Uri.t_of_yojson uri in
      let pos = Position.t_of_yojson pos in
      let expr = Yojson.Safe.Util.to_string expr in
      Some (uri, pos, expr)
    | _ -> None

  let parse_exn (params : Jsonrpc.Structured.t option) : t =
    let raise_invalid_params ?data ~message () =
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ?data
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message
           ()
    in
    match params with
    | None ->
      raise_invalid_params ~message:"Expected params but received none" ()
    | Some params -> (
      match t_of_structured_json params with
      | Some uri -> uri
      | None ->
        let error_json =
          `Assoc
            [ ("params_expected", expected_params)
            ; ("params_received", (params :> Json.t))
            ]
        in
        raise_invalid_params
          ~message:"Unxpected parameter format"
          ~data:error_json
          ())
end

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
      let uri, position, expr = Request_params.parse_exn params in
      let store = state.store in
      let doc = Document_store.get_opt store uri in
      match doc with
      | None ->
        Jsonrpc.Response.Error.raise
        @@ Jsonrpc.Response.Error.make
             ~code:Jsonrpc.Response.Error.Code.InvalidParams
             ~message:
               (Printf.sprintf
                  "Document %s wasn't found in the document store"
                  (Uri.to_string uri))
             ()
      | Some doc ->
        let pos = Position.logical position in
        let command =
          Query_protocol.Type_enclosing
            (Some (expr, String.length expr - 1), pos, None)
        in
        let+ types =
          Document.Merlin.dispatch_exn (Document.merlin_exn doc) command
        in
        let first =
          match types with
          | (_, `String res, _) :: _ -> res
          | _ -> "notype"
        in
        Json.of_string first)
