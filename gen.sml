structure QbeGen :> QBE_GEN =
struct

  structure T = QbeTypes

  type module = T.def list ref

  fun newModule () = ref []

  fun defs m = rev(!m)

  fun addDef (m, def) = m := def :: !m

end
