signature QBE_GEN =
  sig
    type module

    val newModule : unit -> module

    val defs : module -> QbeTypes.def list

    val addDef : module * QbeTypes.def -> unit
  end
