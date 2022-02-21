signature QBE_PARSE =
  sig
    exception Error

    val parse : TextIO.instream * string -> QbeTypes.def list

    val anyErrors : unit -> bool
  end
