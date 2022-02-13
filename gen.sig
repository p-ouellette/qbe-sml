signature QBE_GEN =
  sig
    type module

    val newModule : unit -> module

    val defs : module -> QbeTypes.def list

    val addDef : module * QbeTypes.def -> unit

    val addType : module * QbeTypes.typedef -> unit

    val addOpaqueType : module * QbeTypes.darktypedef -> unit

    val addData : module * QbeTypes.datadef -> unit

    val addFunc : module * QbeTypes.func -> unit
  end
