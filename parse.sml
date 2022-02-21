structure QbeParse : QBE_PARSE =
struct

  structure QbeLrVals = QbeLrValsFun(structure Token = LrParser.Token)
  structure QbeLex = QbeLexFun(structure Tokens = QbeLrVals.Tokens
                               structure Interface = Interface)
  structure QbeParser = Join(structure LrParser = LrParser
                             structure ParserData = QbeLrVals.ParserData
                             structure Lex = QbeLex)

  exception Error

  fun parse (stream, name) = let
        val _ = Interface.init name
        val lexer = QbeParser.makeLexer(fn i => TextIO.inputN(stream, i))
        val (defs, _) = QbeParser.parse(15, lexer, Interface.error, ())
            handle QbeParser.ParseError => raise Error
         in defs
        end

  fun anyErrors () = !Interface.anyErrors

end
