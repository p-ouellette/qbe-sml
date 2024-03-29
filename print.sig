signature QBE_PRINT =
  sig
    val printModule : TextIO.outstream * QbeModule.module -> unit

    val printDefs : TextIO.outstream * QbeTypes.def list -> unit

    val printDef : TextIO.outstream * QbeTypes.def -> unit

    val printType : TextIO.outstream * QbeTypes.typedef -> unit

    val printOpaqueType : TextIO.outstream * QbeTypes.darktypedef -> unit

    val printData : TextIO.outstream * QbeTypes.datadef -> unit

    val printFn : TextIO.outstream * QbeTypes.func -> unit
  end
