structure QbeGen :> QBE_GEN =
struct

  structure T = QbeTypes

  type module = T.def list ref

  fun newModule () = ref []

  fun defs m = rev(!m)

  fun addDef (m, def) = m := def :: !m

  fun addType (m, t) = m := T.Type t :: !m

  fun addOpaqueType (m, t) = m := T.OpaqueType t :: !m

  fun addData (m, d) = m := T.Data d :: !m

  fun addFunc (m, f) = m := T.Function f :: !m

end
