structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token

val lineNum = ref 0
fun eof () = T.EOF(!lineNum,!lineNum)

fun ident s = Atom.atom(String.extract(s, 1, NONE))

fun makeString s = let
      val s = String.substring(s, 1, String.size s - 2)
       in T.STR(valOf(String.fromString s),!lineNum,!lineNum)
      end

structure KW = KeywordFn(struct
  type token = lexresult
  type pos = int
  fun ident (id, p1, _) =
        raise Fail("bad token '" ^ Atom.toString id ^ "' at line " ^ Int.toString p1)
  val keywords =
    [("type", T.TYPE),
     ("align", T.ALIGN),
     ("export", T.EXPORT),
     ("data", T.DATA),
     ("function", T.FUNCTION),
     ("add", T.ADD),
     ("sub", T.SUB),
     ("div", T.DIV),
     ("mul", T.MUL),
     ("neg", T.NEG),
     ("udiv", T.UDIV),
     ("rem", T.REM),
     ("urem", T.UREM),
     ("or", T.OR),
     ("xor", T.XOR),
     ("and", T.AND),
     ("sar", T.SAR),
     ("shr", T.SHR),
     ("shl", T.SHL),
     ("stored", T.STORED),
     ("stores", T.STORES),
     ("storel", T.STOREL),
     ("storew", T.STOREW),
     ("storeh", T.STOREH),
     ("storeb", T.STOREB),
     ("loadd", T.LOADD),
     ("loads", T.LOADS),
     ("loadl", T.LOADL),
     ("loadw", T.LOADW),
     ("loadsw", T.LOADSW),
     ("loaduw", T.LOADUW),
     ("loadsh", T.LOADSH),
     ("loaduh", T.LOADUH),
     ("loadsb", T.LOADSB),
     ("loadub", T.LOADUB),
     ("alloc4", T.ALLOC4),
     ("alloc8", T.ALLOC8),
     ("alloc16", T.ALLOC16),
     ("ceqd", T.CEQD),
     ("ceql", T.CEQL),
     ("ceqs", T.CEQS),
     ("ceqw", T.CEQW),
     ("cged", T.CGED),
     ("cges", T.CGES),
     ("cgtd", T.CGTD),
     ("cgts", T.CGTS),
     ("cled", T.CLED),
     ("cles", T.CLES),
     ("cltd", T.CLTD),
     ("clts", T.CLTS),
     ("cned", T.CNED),
     ("cnel", T.CNEL),
     ("cnes", T.CNES),
     ("cnew", T.CNEW),
     ("cod", T.COD),
     ("cos", T.COS),
     ("csgel", T.CSGEL),
     ("csgew", T.CSGEW),
     ("csgtl", T.CSGTL),
     ("csgtw", T.CSGTW),
     ("cslel", T.CSLEL),
     ("cslew", T.CSLEW),
     ("csltl", T.CSLTL),
     ("csltw", T.CSLTW),
     ("cugel", T.CUGEL),
     ("cugew", T.CUGEW),
     ("cugtl", T.CUGTL),
     ("cugtw", T.CUGTW),
     ("culel", T.CULEL),
     ("culew", T.CULEW),
     ("cultl", T.CULTL),
     ("cultw", T.CULTW),
     ("cuod", T.CUOD),
     ("cuos", T.CUOS),
     ("dtosi", T.DTOSI),
     ("dtoui", T.DTOUI),
     ("exts", T.EXTS),
     ("extsb", T.EXTSB),
     ("extsh", T.EXTSH),
     ("extsw", T.EXTSW),
     ("extub", T.EXTUB),
     ("extuh", T.EXTUH),
     ("extuw", T.EXTUW),
     ("sltof", T.SLTOF),
     ("ultof", T.ULTOF),
     ("stosi", T.STOSI),
     ("stoui", T.STOUI),
     ("swtof", T.SWTOF),
     ("uwtof", T.UWTOF),
     ("truncd", T.TRUNCD),
     ("cast", T.CAST),
     ("copy", T.COPY),
     ("call", T.CALL),
     ("vastart", T.VASTART),
     ("vaarg", T.VAARG),
     ("phi", T.PHI),
     ("jmp", T.JMP),
     ("jnz", T.JNZ),
     ("ret", T.RET),
     ("w", T.W),
     ("l", T.L),
     ("s", T.S),
     ("d", T.D),
     ("b", T.B),
     ("h", T.H),
     ("z", T.Z)
    ]
end)

%%
%header (functor QbeLexFun(structure Tokens : Qbe_TOKENS));
ws=[\ \t];
alpha=[A-Za-z];
digit=[0-9];
id={alpha}({alpha}|{digit})*;
optsign=(\+|\-)?;
integer={optsign}{digit}+;

%%
{ws}+      => (continue());
\n         => (lineNum := !lineNum + 1; continue());
{id}+      => (KW.keyword(yytext,!lineNum,!lineNum));
":"{id}    => (T.TYP(ident yytext,!lineNum,!lineNum));
"$"{id}    => (T.GLO(ident yytext,!lineNum,!lineNum));
"%"{id}    => (T.TMP(ident yytext,!lineNum,!lineNum));
"@"{id}    => (T.LBL(ident yytext,!lineNum,!lineNum));
{integer}  => (T.INT(valOf(Int.fromString yytext),!lineNum,!lineNum));
\"[^\"]*\" => (makeString yytext);
#.*        => (continue());
","        => (T.COMMA(!lineNum,!lineNum));
"{"        => (T.LBRACE(!lineNum,!lineNum));
"}"        => (T.RBRACE(!lineNum,!lineNum));
"("        => (T.LPAREN(!lineNum,!lineNum));
")"        => (T.RPAREN(!lineNum,!lineNum));
"="        => (T.EQ(!lineNum,!lineNum));
"..."      => (T.DOTS(!lineNum,!lineNum));
.          => (raise Fail("bad character at line " ^ Int.toString(!lineNum)));
