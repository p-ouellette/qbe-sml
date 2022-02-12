structure Qbe =
struct

  structure QbeLrVals = QbeLrValsFun(structure Token = LrParser.Token)
  structure QbeLex = QbeLexFun(structure Tokens = QbeLrVals.Tokens)
  structure QbeParser = Join(structure LrParser = LrParser
                             structure ParserData = QbeLrVals.ParserData
                             structure Lex = QbeLex)

  fun parse fname = let
        val file = TextIO.openIn fname
        val stream = QbeParser.makeLexer(fn i => TextIO.inputN(file, i))
        fun error (msg, line, _) =
              TextIO.output(TextIO.stdErr,
                fname ^ ":" ^ Int.toString line ^ ": " ^ msg ^ "\n")
        val _ = QbeLex.UserDeclarations.lineNum := 1
        val (absyn, _) = QbeParser.parse(30, stream, error, ())
         in TextIO.closeIn file;
            absyn
        end

end
