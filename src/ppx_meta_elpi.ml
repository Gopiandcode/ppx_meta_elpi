
let elpi = Elpi.API.Setup.init ~builtins:[] ()


let program = Elpi.API.Parse.program_from ~elpi
    ~loc:(Elpi.API.Ast.Loc.initial __FILE__) (Lexing.from_string ~with_positions:true "hello(let(x,3)).")

let query = Elpi.API.Parse.goal ~elpi
    ~loc:(Elpi.API.Ast.Loc.initial __FILE__) ~text:"hello(X)"


let executable =
  Elpi.API.Compile.optimize @@
  Elpi.API.Compile.query
    (Elpi.API.Compile.program ~elpi [program])
    query
                


let () =
  print_endline "\n";
  print_endline @@ match Elpi.API.Execute.once ?max_steps:None ?delay_outside_fragment:None executable
  with
  | Elpi.API.Execute.Success { assignments; constraints; state; output=(); pp_ctx } ->

    let term = Elpi.API.Data.StrMap.find "X" assignments in
    begin match (Elpi.API.RawData.kool .look ~depth:1 term) with
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
