[@@@warning "-32-33"]

module API = Elpi.API
module E = API.Data
module CD = API.RawOpaqueData

let let_ = Elpi.API.RawData.Constants.declare_global_symbol "let"
let hello = Elpi.API.RawData.Constants.declare_global_symbol "hello"
let term = let open Elpi.API.RawData in
  (mkAppL hello [mkAppL let_ [Elpi.API.RawOpaqueData.of_string "x"; Elpi.API.RawOpaqueData.of_string "y"]])

let elpi = Elpi.API.Setup.init ~builtins:[] ()


let program = Elpi.API.Parse.program_from ~elpi
    ~loc:(Elpi.API.Ast.Loc.initial __FILE__) (Lexing.from_string ~with_positions:true "hello(let(x,3, 1 + x)).")

let query = Elpi.API.Parse.goal ~elpi
    ~loc:(Elpi.API.Ast.Loc.initial __FILE__) ~text:"hello(X)"

let program_2 =
  let open Elpi.API.RawData in
  Elpi.API.Utils.clause_of_term ~depth:1
    (Elpi.API.Ast.Loc.initial __FILE__)
    term

let query = Elpi.API.Compile.query
    (Elpi.API.Compile.program ~elpi [program_2])
    query

let executable =
  Elpi.API.Compile.optimize @@
  query
                


let () =
  print_endline "\n";
  Format.printf "program is %a\n%!" Elpi.API.Pp.program query;
  print_endline @@ match Elpi.API.Execute.once ?max_steps:None ?delay_outside_fragment:None executable
  with
  | Elpi.API.Execute.Success { assignments; constraints; state; output=(); pp_ctx } ->

    let term = Elpi.API.Data.StrMap.find "X" assignments in
    begin match Elpi.API.RawData.(look ~depth:1 term) with
      | App (constant, f, args) ->
        Format.printf "found App(%d,%a,[%a])\n%!" constant (Elpi.API.Pp.term pp_ctx) f
          (Format.pp_print_list (Elpi.API.Pp.term pp_ctx)) args;
      | _ -> ()
    end;

    Format.asprintf "assignments:\n%a\nconstraints:\n%a\nstate:\n%a\n%!"
      (Elpi.API.Data.StrMap.pp (Elpi.API.Pp.term pp_ctx)) assignments
      (Elpi.API.Pp.constraints pp_ctx) constraints
      Elpi.API.Pp.state state
  | Elpi.API.Execute.Failure -> "failure"
  | Elpi.API.Execute.NoMoreSteps -> "no more steps"
