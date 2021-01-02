Def' (VarFree "map",
  Applic'
   (LambdaSimple' (["null?"; "car"; "cdr"; "cons"; "apply"],
     Applic'
      (LambdaSimple' (["map-many"; "map-one"],
        Seq'
         [Set' (VarParam ("map-many", 0), Box' (VarParam ("map-many", 0)));
          Set' (VarParam ("map-one", 1), Box' (VarParam ("map-one", 1)));
          BoxSet' (VarParam ("map-many", 0),
           LambdaSimple' (["f"; "lists"],
            If'
             (Applic' (Var' (VarBound ("null?", 1, 0)),
               [Applic' (Var' (VarBound ("car", 1, 1)),
                 [Var' (VarParam ("lists", 1))])]),
             Const' (Sexpr Nil),
             Applic' (Var' (VarBound ("cons", 1, 3)),
              [Applic' (Var' (VarBound ("apply", 1, 4)),
                [Var' (VarParam ("f", 0));
                 Applic' (BoxGet' (VarBound ("map-one", 0, 1)),
                  [Var' (VarBound ("car", 1, 1)); Var' (VarParam ("lists", 1))])]);
               Applic' (BoxGet' (VarBound ("map-many", 0, 0)),
                [Var' (VarParam ("f", 0));
                 Applic' (BoxGet' (VarBound ("map-one", 0, 1)),
                  [Var' (VarBound ("cdr", 1, 2)); Var' (VarParam ("lists", 1))])])]))));
          BoxSet' (VarParam ("map-one", 1),
           LambdaSimple' (["f"; "s"],
            If'
             (Applic' (Var' (VarBound ("null?", 1, 0)),
               [Var' (VarParam ("s", 1))]),
             Const' (Sexpr Nil),
             Applic' (Var' (VarBound ("cons", 1, 3)),
              [Applic' (Var' (VarParam ("f", 0)),
                [Applic' (Var' (VarBound ("car", 1, 1)),
                  [Var' (VarParam ("s", 1))])]);
               Applic' (BoxGet' (VarBound ("map-one", 0, 1)),
                [Var' (VarParam ("f", 0));
                 Applic' (Var' (VarBound ("cdr", 1, 2)),
                  [Var' (VarParam ("s", 1))])])]))));
          Applic'
           (LambdaSimple' ([],
             LambdaOpt' (["f"], "args",
              Applic' (BoxGet' (VarBound ("map-many", 1, 0)),
               [Var' (VarParam ("f", 0)); Var' (VarParam ("args", 1))]))),
           [])]),
      [Const' (Sexpr (String "whatever")); Const' (Sexpr (String "whatever"))]
      )),
   [Var' (VarFree "null?"); Var' (VarFree "car"); Var' (VarFree "cdr");
    Var' (VarFree "cons"); Var' (VarFree "apply")]))