structure T = QbeTypes
val id = Atom.atom

val str : T.datadef =
      {name = id "str",
       exported = false,
       align = NONE,
       fields = [T.DataTy(T.B, [T.DataStr "hello world"]),
                 T.DataTy(T.B, [T.DataCon 0])]}

val stmts : T.stmt list =
  [T.Assign(id "r", T.W, T.Call(id "puts", [(T.L, T.Glo(id "str"))]))]
val start : T.block =
      {label = id "start",
       stmts = stmts,
       jump = SOME(T.Ret(SOME(T.Con 0)))}
val main : T.func =
      {name = id "main",
       exported = false,
       params = [],
       variadic = false,
       result = SOME T.W,
       blocks = [start]}

val m = QbeGen.newModule()
val _ = QbeGen.addData(m, str)
val _ = QbeGen.addFunc(m, main)
val _ = QbePrint.printModule(TextIO.stdOut, m)

val m = QbeParse.parse "hello.ssa"
val defs = QbeGen.defs m
val _ = QbePrint.printModule(TextIO.stdOut, m)
