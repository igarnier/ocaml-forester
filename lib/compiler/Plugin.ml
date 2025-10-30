open Forester_core

type step = Value.t list -> (Value.t, string) Result.t

type instance = {
  step_arity : int ;
  step : step
}

type env = Eio_unix.Stdenv.base * Eio.Switch.t

type plugin = env -> instance

let plugin_table = Hashtbl.create 11

let register name (plugin : plugin) =
  if Hashtbl.mem plugin_table name then
    Reporter.fatal (Reporter.Message.Plugin_name_already_registered name);
  Hashtbl.add plugin_table name plugin

let find_opt name =
  match Hashtbl.find_opt plugin_table name with
  | None -> None
  | Some plugin -> Some plugin

let counter : plugin = fun (_env, _sw) ->
  let c = ref 0 in
  let step_arity = 1 in
  let step (args : Value.t list) =
    assert (List.compare_length_with args step_arity = 0);
    incr c;
    match args with
    | [Value.Content (Types.Content [content])] ->
      Ok (Value.Content (Types.Content [Text (string_of_int !c); content]))
    | _ -> Error "counter plugin: unhandled input"
  in
  {
    step_arity;
    step
  }

let () = register "counter" counter
