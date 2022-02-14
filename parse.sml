structure QbeParse : QBE_PARSE =
struct

  structure QbeLrVals = QbeLrValsFun(structure Token = LrParser.Token)
  structure QbeLex = QbeLexFun(structure Tokens = QbeLrVals.Tokens
                               structure Interface = Interface)
  structure QbeParser = Join(structure LrParser = LrParser
                             structure ParserData = QbeLrVals.ParserData
                             structure Lex = QbeLex)

  fun parse fname = let
        val _ = Interface.init fname
        val file = TextIO.openIn fname
        val stream = QbeParser.makeLexer(fn i => TextIO.inputN(file, i))
        val (defs, _) = QbeParser.parse(30, stream, Interface.error, ())
         in TextIO.closeIn file;
            defs
        end

  fun anyErrors () = !Interface.anyErrors

end
