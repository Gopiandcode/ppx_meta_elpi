  $ ./pp.exe ./resources/adt.ml
  type data =
    | Cons1 of int 
    | Cons2 of int list 
    | Cons3 of data [@@deriving elpi]
  include
    struct
      let _ = fun (_ : data) -> ()
      [@@@warning "-26-27-32-39-60"]
      let elpi_constant_type_data = "data"
      let _ = elpi_constant_type_data
      let elpi_constant_type_datac =
        Elpi.API.RawData.Constants.declare_global_symbol
          elpi_constant_type_data
      let _ = elpi_constant_type_datac
      let elpi_constant_constructor_data_Cons1 = "cons1"
      let _ = elpi_constant_constructor_data_Cons1
      let elpi_constant_constructor_data_Cons1c =
        Elpi.API.RawData.Constants.declare_global_symbol
          elpi_constant_constructor_data_Cons1
      let _ = elpi_constant_constructor_data_Cons1c
      let elpi_constant_constructor_data_Cons2 = "cons2"
      let _ = elpi_constant_constructor_data_Cons2
      let elpi_constant_constructor_data_Cons2c =
        Elpi.API.RawData.Constants.declare_global_symbol
          elpi_constant_constructor_data_Cons2
      let _ = elpi_constant_constructor_data_Cons2c
      let elpi_constant_constructor_data_Cons3 = "cons3"
      let _ = elpi_constant_constructor_data_Cons3
      let elpi_constant_constructor_data_Cons3c =
        Elpi.API.RawData.Constants.declare_global_symbol
          elpi_constant_constructor_data_Cons3
      let _ = elpi_constant_constructor_data_Cons3c
      let rec elpi_embed_data : 'c . data Elpi.API.Conversion.embedding =
        fun ~depth:elpi__depth ->
          fun elpi__state ->
            function
            | Cons1 elpi__7 ->
                let (elpi__state, elpi__9, elpi__8) =
                  Elpi.API.BuiltInData.int.Elpi.API.Conversion.embed
                    ~depth:elpi__depth elpi__state elpi__7 in
                (elpi__state,
                  (Elpi.API.RawData.mkAppL
                     elpi_constant_constructor_data_Cons1c [elpi__9]),
                  (List.concat [elpi__8]))
            | Cons2 elpi__10 ->
                let (elpi__state, elpi__12, elpi__11) =
                  (Elpi.API.BuiltInData.list Elpi.API.BuiltInData.int).Elpi.API.Conversion.embed
                    ~depth:elpi__depth elpi__state elpi__10 in
                (elpi__state,
                  (Elpi.API.RawData.mkAppL
                     elpi_constant_constructor_data_Cons2c [elpi__12]),
                  (List.concat [elpi__11]))
            | Cons3 elpi__13 ->
                let (elpi__state, elpi__15, elpi__14) =
                  elpi_embed_data ~depth:elpi__depth elpi__state elpi__13 in
                (elpi__state,
                  (Elpi.API.RawData.mkAppL
                     elpi_constant_constructor_data_Cons3c [elpi__15]),
                  (List.concat [elpi__14]))
      let _ = elpi_embed_data
      let rec elpi_readback_data : 'c . data Elpi.API.Conversion.readback =
        fun ~depth:elpi__depth ->
          fun elpi__state ->
            fun elpi__x ->
              match Elpi.API.RawData.look ~depth:elpi__depth elpi__x with
              | Elpi.API.RawData.App (elpi__hd, elpi__x, elpi__xs) when
                  elpi__hd == elpi_constant_constructor_data_Cons1c ->
                  let (elpi__state, elpi__2, elpi__1) =
                    Elpi.API.BuiltInData.int.Elpi.API.Conversion.readback
                      ~depth:elpi__depth elpi__state elpi__x in
                  (match elpi__xs with
                   | [] ->
                       (elpi__state, (Cons1 elpi__2), (List.concat [elpi__1]))
                   | _ ->
                       Elpi.API.Utils.type_error
                         ("Not enough arguments to constructor: " ^
                            (Elpi.API.RawData.Constants.show
                               elpi_constant_constructor_data_Cons1c)))
              | Elpi.API.RawData.App (elpi__hd, elpi__x, elpi__xs) when
                  elpi__hd == elpi_constant_constructor_data_Cons2c ->
                  let (elpi__state, elpi__4, elpi__3) =
                    (Elpi.API.BuiltInData.list Elpi.API.BuiltInData.int).Elpi.API.Conversion.readback
                      ~depth:elpi__depth elpi__state elpi__x in
                  (match elpi__xs with
                   | [] ->
                       (elpi__state, (Cons2 elpi__4), (List.concat [elpi__3]))
                   | _ ->
                       Elpi.API.Utils.type_error
                         ("Not enough arguments to constructor: " ^
                            (Elpi.API.RawData.Constants.show
                               elpi_constant_constructor_data_Cons2c)))
              | Elpi.API.RawData.App (elpi__hd, elpi__x, elpi__xs) when
                  elpi__hd == elpi_constant_constructor_data_Cons3c ->
                  let (elpi__state, elpi__6, elpi__5) =
                    elpi_readback_data ~depth:elpi__depth elpi__state elpi__x in
                  (match elpi__xs with
                   | [] ->
                       (elpi__state, (Cons3 elpi__6), (List.concat [elpi__5]))
                   | _ ->
                       Elpi.API.Utils.type_error
                         ("Not enough arguments to constructor: " ^
                            (Elpi.API.RawData.Constants.show
                               elpi_constant_constructor_data_Cons3c)))
              | _ ->
                  Elpi.API.Utils.type_error
                    (Format.asprintf "Not a constructor of type %s: %a" "data"
                       (Elpi.API.RawPp.term elpi__depth) elpi__x)
      let _ = elpi_readback_data
      let data : 'c . data Elpi.API.Conversion.t =
        let kind = Elpi.API.Conversion.TyName "data" in
        {
          Elpi.API.Conversion.ty = kind;
          pp_doc =
            (fun fmt ->
               fun () ->
                 Format.fprintf fmt "kind %s type." ~doc:"data";
                 Format.fprintf fmt "type %s." "cons1";
                 Format.fprintf fmt "type %s." "cons2";
                 Format.fprintf fmt "type %s." "cons3");
          pp = pp_data;
          embed = elpi_embed_data;
          readback = elpi_readback_data
        }
      let _ = data
      let elpi_data = Elpi.API.BuiltIn.MLData data
      let _ = elpi_data
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
