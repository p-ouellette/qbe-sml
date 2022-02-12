structure QbePrint : QBE_PRINT =
struct

  structure T = QbeTypes

  fun say out s = TextIO.output(out, s)

  fun sayint out i = say out (Int.toString i)
  fun sayid s out a = (say out s; say out (Atom.toString a))

  val sayAggTy = sayid ":"
  val sayGlobal = sayid "$"
  val sayTemp = sayid "%"
  val sayLabel = sayid "@"

  fun tystr T.Word = "w"
    | tystr T.Long = "l"
    | tystr T.Single = "s"
    | tystr T.Double = "d"
    | tystr T.Byte = "b"
    | tystr T.HalfWord = "h"
    | tystr (T.Aggregate _) = raise Fail "unreachable"

  fun sayty out (T.Aggregate a) = sayAggTy out a
    | sayty out ty = say out (tystr ty)

  fun printTypeDef (out, {name, align, items}) = let
        val say = say out
        fun sayitem (ty, n) =
              (sayty out ty;
               if n > 1 then (say " "; sayint out n) else ();
               say ", ")
        in
          say "type "; sayAggTy out name; say " =";
          case align
            of NONE => ()
             | SOME i => (say " align "; sayint out i);
          say " { "; app sayitem items; say "}\n"
        end

  fun printDarkTypeDef (out, {name, align, size}) = let
        val say = say out
        in
          say "type "; sayAggTy out name; say " = align "; sayint out align;
          say " { "; sayint out size; say " }\n"
        end

  fun printDataDef (out, {name, exported, align, fields}) = let
        val say = say out
        fun sayitem (T.DataSymbol a) = (say " "; sayGlobal out a)
          | sayitem (T.DataStr s) = (say " \""; say(String.toString s); say "\"")
          | sayitem (T.DataConst i) = (say " "; sayint out i)

        fun sayfield (T.DataFieldTy(ty, items)) =
              (sayty out ty; app sayitem items; say ", ")
          | sayfield (T.DataFieldZ i) = (say "z "; sayint out i; say ", ")
        in
          if exported then say "export " else ();
          say "data "; sayGlobal out name; say " =";
          case align
            of NONE => ()
             | SOME i => (say " align "; sayint out i);
          say " { "; app sayfield fields; say "}\n"
        end

  fun printFn (out, {name, exported, params, variadic, result, stmts}) = let
        val say = say out
        in
          ()
        end

  fun printDef (out, T.Type d) = printTypeDef(out, d)
    | printDef (out, T.OpaqueType d) = printDarkTypeDef(out, d)
    | printDef (out, T.Data d) = printDataDef(out, d)
    | printDef (out, T.Function d) = printFn(out, d)

end
