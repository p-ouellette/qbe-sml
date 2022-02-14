signature QBE_PARSE =
  sig
    val parse : string -> QbeTypes.def list

    val anyErrors : unit -> bool
  end
