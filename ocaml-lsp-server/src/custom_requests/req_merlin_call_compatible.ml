open Import

let capability = ("handleMerlinCallCompatible", `Bool true)

let meth = "ocamllsp/merlinCallCompatible"

type params =
  { uri : Uri.t
  ; result_as_sexp : bool
  ; command : string
  ; args : string list
  }

let expected_params =
  `Assoc
    [ ("uri", `String "<DocumentUri>")
    ; ("command", `String "<MerlinCommand>")
    ; ("resultAsSexp?", `String "<true | false>")
    ; ("args?", `String "<ListOfArguments>")
    ]

let as_sexp_from_structured_json params =
  match List.assoc_opt "resultAsSexp" params with
  | Some (`Bool value) -> value
  | _ ->
    (* If the parameter is incorrectly formatted or missing, it is assumed that
       the result is not requested in the form of Sexp *)
    false

let command_from_structured_json params =
  match List.assoc_opt "command" params with
  | Some (`String command_name) -> Some command_name
  | _ ->
    (* If the parameter is incorrectly formatted or missing, we refuse to build
       the parameter, [command] is mandatory. *)
    None

let uri_from_structured_json params =
  match List.assoc_opt "uri" params with
  | Some uri -> Some (Uri.t_of_yojson uri)
  | _ ->
    (* If the parameter is incorrectly formatted or missing, we refuse to build
       the parameter, [command] is mandatory. *)
    None

let partition_args =
  (* The function is relatively optimistic and attempts to treat literal data as
     strings of characters. *)
  function
  | `String s -> Either.Left s
  | `Bool b -> Either.Left (string_of_bool b)
  | `Float f -> Either.Left (string_of_float f)
  | `Int i -> Either.Left (string_of_int i)
  | `Intlit i -> Either.Left i
  | x -> Either.Right x

let args_from_structured_json params =
  match List.assoc_opt "args" params with
  | Some (`List args) -> (
    (* Only lists containing stringish data are accepted. *)
    match List.partition_map ~f:partition_args args with
    | args, [] -> Some args (* No invalid arg. *)
    | _, _ :: _ -> None (* Some invalid args. *))
  | _ ->
    (* If args is not a list or is absent, we assume that an empty list is
       passed. *)
    Some []

let from_structured_json = function
  | `Assoc params ->
    let result_as_sexp = as_sexp_from_structured_json params in
    let open Option.O in
    let* command = command_from_structured_json params in
    let* args = args_from_structured_json params in
    let* uri = uri_from_structured_json params in
    Some { result_as_sexp; command; args; uri }
  | _ -> None

let raise_invalid_params ?data ~message () =
  let open Jsonrpc.Response.Error in
  raise @@ make ?data ~code:Code.InvalidParams ~message ()

let from_structured_json_exn = function
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params -> (
    match from_structured_json params with
    | Some params -> params
    | None ->
      let data =
        `Assoc
          [ ("expectedParams", expected_params)
          ; ("receivedParams", (params :> Json.t))
          ]
      in
      raise_invalid_params ~data ~message:"Unexpected params format" ())

let with_pipeline state uri f =
  match Document_store.get_opt state.State.store uri with
  | Some doc ->
    let merlin = Document.merlin_exn doc in
    Document.Merlin.with_pipeline_exn merlin f
  | None ->
    let message =
      "Document " ^ Uri.to_string uri ^ " wasn't found in the document store"
    in
    raise_invalid_params ~message ()

let parse_arguments state uri specs raw_args cmd_args =
  let conf = Merlin_config.DB.get state.State.merlin_config uri in
  let open Fiber.O in
  let+ config = Merlin_config.config conf in
  let specs = List.map ~f:snd specs in
  Mconfig.parse_arguments
    ~wd:(Sys.getcwd ())
    ~warning:ignore
    specs
    raw_args
    config
    cmd_args
  |> snd

let perform_query action params pipeline =
  let action () = action pipeline params in
  let class_, output =
    match action () with
    | result -> ("return", result)
    | exception Failure message -> ("failure", `String message)
    | exception exn ->
      let message = Printexc.to_string exn in
      ("exception", `String message)
  in
  `Assoc [ ("class", `String class_); ("value", output) ]

let on_request ~params state =
  Fiber.of_thunk (fun () ->
      let { result_as_sexp; command; args; uri } =
        from_structured_json_exn params
      in
      match
        Merlin_commands.New_commands.(find_command command all_commands)
      with
      | Merlin_commands.New_commands.Command (_name, _doc, specs, params, action)
        ->
        let open Fiber.O in
        let* params = parse_arguments state uri specs args params in
        let+ json = with_pipeline state uri @@ perform_query action params in
        let result =
          if result_as_sexp then
            Merlin_utils.(json |> Sexp.of_json |> Sexp.to_string)
          else json |> Yojson.Basic.to_string
        in
        `Assoc
          [ ("resultAsSexp", `Bool result_as_sexp); ("result", `String result) ]
      | exception Not_found ->
        let data = `Assoc [ ("command", `String command) ] in
        raise_invalid_params ~data ~message:"Unexpected command name" ())
