signature QBE_PRINT =
  sig
    val printDef : TextIO.outstream * QbeTypes.def -> unit

    val printTypeDef : TextIO.outstream * QbeTypes.typedef -> unit

    val printDarkTypeDef : TextIO.outstream * QbeTypes.darktypedef -> unit

    val printDataDef : TextIO.outstream * QbeTypes.datadef -> unit

    val printFn : TextIO.outstream * QbeTypes.func -> unit
  end
