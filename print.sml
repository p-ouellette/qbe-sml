structure QbePrint : QBE_PRINT =
  struct
    structure T = QbeTypes
    structure M = QbeModule

    fun say out s = TextIO.output (out, s)

    val fixsign = String.map (fn #"~" => #"-" | c => c)

    fun sayint out = (say out) o fixsign o Int.toString
    fun sayr32 out = (say out) o fixsign o Real32.toString
    fun sayr64 out = (say out) o fixsign o Real64.toString

    fun sayid s out id = (say out s; say out (Atom.toString id))

    val saytyp = sayid ":"
    val sayglo = sayid "$"
    val saytmp = sayid "%"
    val saylbl = sayid "@"

    fun tystr T.W = "w"
      | tystr T.L = "l"
      | tystr T.S = "s"
      | tystr T.D = "d"
      | tystr T.B = "b"
      | tystr T.H = "h"
      | tystr (T.Agg _) = raise Fail "impossible"

    fun sayty out (T.Agg name) = saytyp out name
      | sayty out ty = say out (tystr ty)

    fun saycon out (T.Int i) = say out (fixsign (Int64.toString i))
      | saycon out (T.Flts r) = (say out "s_"; sayr32 out r)
      | saycon out (T.Fltd r) = (say out "d_"; sayr64 out r)

    fun saylnk out {exported, section} =
      let
        val say = say out
        fun saystr s = (say "\""; say s; say "\"")
        fun saysec {name, flags} =
          ( say "section "
          ; saystr name
          ; say " "
          ; Option.app saystr flags
          ; say "\n"
          )
      in
        if exported then say "export " else (); Option.app saysec section
      end

    fun sayassign out (tmp, ty) =
      (saytmp out tmp; say out " ="; sayty out ty; say out " ")

    fun sayval out (T.Tmp name) = saytmp out name
      | sayval out (T.Glo name) = sayglo out name
      | sayval out (T.Con c) = saycon out c

    val opinfo =
      fn T.Add (a, b) => ("add", [a, b])
       | T.Sub (a, b) => ("sub", [a, b])
       | T.Div (a, b) => ("div", [a, b])
       | T.Mul (a, b) => ("mul", [a, b])
       | T.Neg a => ("neg", [a])
       | T.Udiv (a, b) => ("udiv", [a, b])
       | T.Rem (a, b) => ("rem", [a, b])
       | T.Urem (a, b) => ("urem", [a, b])
       | T.Or (a, b) => ("or", [a, b])
       | T.Xor (a, b) => ("xor", [a, b])
       | T.And (a, b) => ("and", [a, b])
       | T.Sar (a, b) => ("sar", [a, b])
       | T.Shr (a, b) => ("shr", [a, b])
       | T.Shl (a, b) => ("shl", [a, b])
       | T.Load a => ("load", [a])
       | T.Loadd a => ("loadd", [a])
       | T.Loads a => ("loads", [a])
       | T.Loadl a => ("loadl", [a])
       | T.Loadw a => ("loadw", [a])
       | T.Loadsw a => ("loadsw", [a])
       | T.Loaduw a => ("loaduw", [a])
       | T.Loadsh a => ("loadsh", [a])
       | T.Loaduh a => ("loaduh", [a])
       | T.Loadsb a => ("loadsb", [a])
       | T.Loadub a => ("loadub", [a])
       | T.Alloc4 a => ("alloc4", [a])
       | T.Alloc8 a => ("alloc8", [a])
       | T.Alloc16 a => ("alloc16", [a])
       | T.Ceqd (a, b) => ("ceqd", [a, b])
       | T.Ceql (a, b) => ("ceql", [a, b])
       | T.Ceqs (a, b) => ("ceqs", [a, b])
       | T.Ceqw (a, b) => ("ceqw", [a, b])
       | T.Cged (a, b) => ("cged", [a, b])
       | T.Cges (a, b) => ("cges", [a, b])
       | T.Cgtd (a, b) => ("cgtd", [a, b])
       | T.Cgts (a, b) => ("cgts", [a, b])
       | T.Cled (a, b) => ("cled", [a, b])
       | T.Cles (a, b) => ("cles", [a, b])
       | T.Cltd (a, b) => ("cltd", [a, b])
       | T.Clts (a, b) => ("clts", [a, b])
       | T.Cned (a, b) => ("cned", [a, b])
       | T.Cnel (a, b) => ("cnel", [a, b])
       | T.Cnes (a, b) => ("cnes", [a, b])
       | T.Cnew (a, b) => ("cnew", [a, b])
       | T.Cod (a, b) => ("cod", [a, b])
       | T.Cos (a, b) => ("cos", [a, b])
       | T.Csgel (a, b) => ("csgel", [a, b])
       | T.Csgew (a, b) => ("csgew", [a, b])
       | T.Csgtl (a, b) => ("csgtl", [a, b])
       | T.Csgtw (a, b) => ("csgtw", [a, b])
       | T.Cslel (a, b) => ("cslel", [a, b])
       | T.Cslew (a, b) => ("cslew", [a, b])
       | T.Csltl (a, b) => ("csltl", [a, b])
       | T.Csltw (a, b) => ("csltw", [a, b])
       | T.Cugel (a, b) => ("cugel", [a, b])
       | T.Cugew (a, b) => ("cugew", [a, b])
       | T.Cugtl (a, b) => ("cugtl", [a, b])
       | T.Cugtw (a, b) => ("cugtw", [a, b])
       | T.Culel (a, b) => ("culel", [a, b])
       | T.Culew (a, b) => ("culew", [a, b])
       | T.Cultl (a, b) => ("cultl", [a, b])
       | T.Cultw (a, b) => ("cultw", [a, b])
       | T.Cuod (a, b) => ("cuod", [a, b])
       | T.Cuos (a, b) => ("cuos", [a, b])
       | T.Dtosi a => ("dtosi", [a])
       | T.Dtoui a => ("dtoui", [a])
       | T.Exts a => ("exts", [a])
       | T.Extsb a => ("extsb", [a])
       | T.Extsh a => ("extsh", [a])
       | T.Extsw a => ("extsw", [a])
       | T.Extub a => ("extub", [a])
       | T.Extuh a => ("extuh", [a])
       | T.Extuw a => ("extuw", [a])
       | T.Sltof a => ("sltof", [a])
       | T.Ultof a => ("ultof", [a])
       | T.Stosi a => ("stosi", [a])
       | T.Stoui a => ("stoui", [a])
       | T.Swtof a => ("swtof", [a])
       | T.Uwtof a => ("uwtof", [a])
       | T.Truncd a => ("truncd", [a])
       | T.Cast a => ("cast", [a])
       | T.Copy a => ("copy", [a])
       | T.Vaarg a => ("vaarg", [a])

    fun sayinstr out ins =
      let val (name, vals) = opinfo ins in
        say out name;
        say out " ";
        case vals of
          [v] => sayval out v
        | [v1, v2] => (sayval out v1; say out ", "; sayval out v2)
        | _ => raise Fail "impossible"
      end

    fun saycall out {result, name, envp, args, vararg} =
      let
        val say = say out
        fun sayarg ((ty, v), i) =
          ( Option.app (fn i' => if i' = i then say "..., " else ()) vararg
          ; sayty out ty
          ; say " "
          ; sayval out v
          ; say ", "
          ; i + 1
          )
      in
        Option.app (sayassign out) result;
        say "call ";
        sayglo out name;
        say "(";
        Option.app (fn v => (say "env "; sayval out v; say ", ")) envp;
        foldl sayarg 0 args;
        Option.app (fn i => if i = length args then say "..." else ()) vararg;
        say ")"
      end

    fun saystmt out =
      let
        val say = say out
        fun saystore s (v1, v2) =
          (say s; say " "; sayval out v1; say ", "; sayval out v2)
      in
        fn T.Assign (name, ty, ins) =>
          (sayassign out (name, ty); sayinstr out ins)
         | T.Stored a => saystore "stored" a
         | T.Stores a => saystore "stores" a
         | T.Storel a => saystore "storel" a
         | T.Storew a => saystore "storew" a
         | T.Storeh a => saystore "storeh" a
         | T.Storeb a => saystore "storeb" a
         | T.Call c => saycall out c
         | T.Vastart v => (say "vastart "; sayval out v)
      end

    fun sayjmp out =
      let val say = say out in
        fn T.Jmp lbl => (say "jmp "; saylbl out lbl)
         | T.Jnz (v, l1, l2) =>
           ( say "jnz "
           ; sayval out v
           ; say ", "
           ; saylbl out l1
           ; say ", "
           ; saylbl out l2
           )
         | T.Ret a =>
           (say "ret"; Option.app (fn v => (say " "; sayval out v)) a)
      end

    fun printType (out, {name, align, items}) =
      let
        val say = say out
        fun sayitem (ty, n) =
          ( sayty out ty
          ; if n > 1 then
              (say " "; sayint out n)
            else
              ()
          ; say ", "
          )
      in
        say "type ";
        saytyp out name;
        say " =";
        Option.app (fn i => (say " align "; sayint out i)) align;
        say " { ";
        app sayitem items;
        say "}\n"
      end

    fun printOpaqueType (out, {name, align, size}) =
      let val say = say out in
        say "type ";
        saytyp out name;
        say " = align ";
        sayint out align;
        say " { ";
        sayint out size;
        say " }\n"
      end

    fun printData (out, {name, linkage, align, fields}) =
      let
        val say = say out
        fun sayitem (T.DataSym (name, off)) =
            ( say " "
            ; sayglo out name
            ; if off > 0 then
                (say " + "; sayint out off)
              else
                ()
            )
          | sayitem (T.DataStr s) = (say " \""; say s; say "\"")
          | sayitem (T.DataCon c) = (say " "; saycon out c)

        fun sayfield (T.DataTy (ty, items)) =
            (sayty out ty; app sayitem items; say ", ")
          | sayfield (T.DataZ i) = (say "z "; sayint out i; say ", ")
      in
        saylnk out linkage;
        say "data ";
        sayglo out name;
        say " =";
        Option.app (fn i => (say " align "; sayint out i)) align;
        say " { ";
        app sayfield fields;
        say "}\n"
      end

    fun printFn (out, {name, linkage, params, envp, variadic, result, blocks}) =
      let
        val say = say out
        fun sayparam (ty, name) =
          (sayty out ty; say " "; saytmp out name; say ", ")
        fun sayphiarg (lbl, v) =
          (saylbl out lbl; say " "; sayval out v; say ", ")
        fun sayphi {temp, args} =
          ( say "\t"
          ; sayassign out temp
          ; say "phi "
          ; app sayphiarg args
          ; say "\n"
          )
        fun sayblk {label, phis, stmts, jump} =
          ( saylbl out label
          ; say "\n"
          ; app sayphi phis
          ; app (fn s => (say "\t"; saystmt out s; say "\n")) stmts
          ; Option.app (fn j => (say "\t"; sayjmp out j; say "\n")) jump
          )
      in
        saylnk out linkage;
        say "function ";
        Option.app (fn ty => (sayty out ty; say " ")) result;
        sayglo out name;
        say "(";
        Option.app (fn t => (say "env "; saytmp out t; say ", ")) envp;
        app sayparam params;
        if variadic then
          say "..."
        else
          ();
        say ") {\n";
        app sayblk blocks;
        say "}\n"
      end

    fun printDef (out, T.Type t) = printType (out, t)
      | printDef (out, T.OpaqueType t) = printOpaqueType (out, t)
      | printDef (out, T.Data d) = printData (out, d)
      | printDef (out, T.Function f) = printFn (out, f)

    fun printDefs (out, defs) = app (fn d => printDef (out, d)) defs

    fun printModule (out, m) = printDefs (out, M.defs m)
  end
