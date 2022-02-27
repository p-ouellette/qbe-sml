structure T = QbeTypes
val id = Atom.atom

val stmts =
  [T.Assign(id "c", T.W, T.Add(T.Tmp(id "a"), T.Tmp(id "b")))]
val start = {label = id "start",
             phis = []: T.phi list,
             stmts = stmts,
             jump = SOME(T.Ret(SOME(T.Tmp(id "c"))))}
val add =
  T.Function {name = id "add",
              linkage = {exported=false, section=NONE},
              envp = NONE,
              params = [(T.W, id "a"), (T.W, id "b")],
              variadic = false,
              result = SOME T.W,
              blocks = [start]}

val stmts =
  [T.Call {result = SOME(id "r", T.W),
           name = id "add",
           envp = NONE,
           args = [(T.W, T.Con(T.Int 1)), (T.W, T.Con(T.Int 1))],
           vararg = NONE},
   T.Call {result = NONE,
           name = id "printf",
           envp = NONE,
           args = [(T.L, T.Glo(id "fmt")), (T.W, T.Tmp(id "r"))],
           vararg = SOME 1}]
val start = {label = id "start",
             phis = []: T.phi list,
             stmts = stmts,
             jump = SOME(T.Ret(SOME(T.Con(T.Int 0))))}

val main =
  T.Function {name = id "main",
              linkage = {exported=true, section=NONE},
              envp = NONE,
              params = [],
              variadic = false,
              result = SOME T.W,
              blocks = [start]}

val fmt =
  T.Data {name = id "fmt",
          linkage = {exported=false, section=NONE},
          align = NONE,
          fields = [T.DataTy(T.B, [T.DataStr "One and one make %d!\\n"]),
                    T.DataTy(T.B, [T.DataCon(T.Int 0)])]}

val m = QbeGen.newModule()
val _ = QbeGen.addDef(m, add)
val _ = QbeGen.addDef(m, main)
val _ = QbeGen.addDef(m, fmt)
val _ = QbePrint.printModule(TextIO.stdOut, m)

val strm = TextIO.openIn "hello.ssa"
val defs = QbeParse.parse(strm, "hello.ssa")
val _ = TextIO.closeIn strm
val _ = QbePrint.printDefs(TextIO.stdOut, defs)
