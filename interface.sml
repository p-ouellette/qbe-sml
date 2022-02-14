(* Externally visible aspects of the lexer and parser *)

signature INTERFACE =
sig
  type pos
  val fileName : string ref
  val lineNum : pos ref
  val anyErrors : bool ref
  val init : string -> unit
  val nextLine : unit -> unit
  val error : string * pos * pos -> unit
end

structure Interface : INTERFACE =
struct

  type pos = int

  val fileName = ref ""
  val lineNum = ref 0
  val anyErrors = ref false

  fun init fname = (fileName := fname;
                    lineNum := 1;
                    anyErrors := false)

  fun nextLine () = lineNum := !lineNum + 1

  fun error (msg, line, _) =
        (anyErrors := true;
         TextIO.output(TextIO.stdErr,
           !fileName ^ ":" ^ Int.toString line ^ ": " ^ msg ^ "\n"))

end
