open Import
open Fiber.O
module StringMap = Map.Make (String)

let rename (state : State.t)
    { RenameParams.textDocument = { uri }; position; newName; _ } =
  Format.eprintf "DBG URI = %s\n%!" (DocumentUri.to_string uri);
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences
        (`Ident_at (Position.logical position), `Project)
    in
    let+ locs, _desync = Document.Merlin.dispatch_exn merlin command in
    let edits =
      let init = StringMap.empty in
      List.fold_left locs ~init ~f:(fun acc (loc : Warnings.loc) ->
          let path = loc.loc_start.pos_fname in
          let uri = DocumentUri.of_path path in
          let range = Range.of_loc loc in
          let make_edit () = TextEdit.create ~range ~newText:newName in
          let edit =
            (* TODO: to perform correct renaming we need a way to load all the
               impacted documents *)
            match Document_store.get state.store uri with
            | exception _ -> make_edit ()
            | doc -> (
              let source = Document.source doc in
              match
                let occur_start_pos =
                  Position.of_lexical_position loc.loc_start |> Option.value_exn
                in
                occur_start_pos
              with
              | { character = 0; _ } -> make_edit ()
              | pos -> (
                let mpos = Position.logical pos in
                let (`Offset index) = Msource.get_offset source mpos in
                assert (index > 0)
                (* [index = 0] if we pass [`Logical (1, 0)], but we handle the
                   case when [character = 0] in a separate matching branch *);
                let source_txt = Msource.text source in
                match source_txt.[index - 1] with
                | '~' (* the occurrence is a named argument *)
                | '?' (* is an optional argument *) ->
                  let empty_range_at_occur_end =
                    let occur_end_pos = range.Range.end_ in
                    { range with start = occur_end_pos }
                  in
                  TextEdit.create
                    ~range:empty_range_at_occur_end
                    ~newText:(":" ^ newName)
                | _ -> make_edit ()))
          in
          StringMap.add_multi acc path edit)
    in
    let workspace_edits =
      (* TODO: use documentChanges*)
      WorkspaceEdit.create
        ~changes:
          (StringMap.to_list_map edits ~f:(fun path edits ->
               (DocumentUri.of_path path, edits)))
        ()
    in
    workspace_edits
