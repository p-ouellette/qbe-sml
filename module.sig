signature QBE_MODULE =
  sig
    type module

    val module : unit -> module

    val defs : module -> QbeTypes.def list

    val addDef : module * QbeTypes.def -> unit
  end
