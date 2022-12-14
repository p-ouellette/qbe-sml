structure QbeModule :> QBE_MODULE =
  struct
    structure T = QbeTypes

    type module = T.def list ref

    fun module () = ref []

    fun defs m = rev (! m)

    fun addDef (m, def) = m := def :: ! m
  end
