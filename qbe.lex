structure T = Tokens
structure Interface = Interface
open Interface
type pos = Interface.pos
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token

exception Keyword

fun eof () = T.EOF(!lineNum,!lineNum)

fun ident s = Atom.atom(String.extract(s, 1, NONE))

fun float s = valOf(Real.fromString(String.extract(s, 2, NONE)))

fun makeString s = let
      val s = String.substring(s, 1, String.size s - 2)
       in T.STR(s,!lineNum,!lineNum)
      end

structure KW = KeywordFn(struct
  type token = lexresult
  type pos = Interface.pos
  fun ident _ = raise Keyword
  val keywords =
    [("type", T.TYPE),
     ("align", T.ALIGN),
     ("export", T.EXPORT),
     ("section", T.SECTION),
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
     ("retw", T.RETW),
     ("nop", T.NOP),
     ("w", T.W),
     ("l", T.L),
     ("s", T.S),
     ("d", T.D),
     ("b", T.B),
     ("h", T.H),
     ("z", T.Z),
     ("env", T.ENV),
     ("...", T.DOTS)
    ]
end)

%%
%header (functor QbeLexFun(structure Tokens : Qbe_TOKENS
                           structure Interface : INTERFACE));
ws=[\ \t];
alpha=[A-Za-z];
digit=[0-9];
integer=\-?{digit}+;
optsign=[-+]?;
real={optsign}({digit}+\.{digit}*|\.{digit}+)((e|E){optsign}{digit}+)?;
inf=[iI][nN][fF]([iI][nN][iI][tT][yY])?;
string=\"([^\"]|\\\")*\";
kw=({alpha}|[._])({alpha}|[$._]|{digit})*;
global=\$({kw}|{string});

%%
{ws}+     => (continue());
\n        => (nextLine(); continue());
{integer} => (T.INT(valOf(Int64.fromString yytext),!lineNum,!lineNum));
s_{real}  => (T.FLTS(float yytext,!lineNum,!lineNum));
d_{real}  => (T.FLTD(float yytext,!lineNum,!lineNum));
s_\-{inf} => (T.FLTS(Real.negInf,!lineNum,!lineNum));
s_\+{inf} => (T.FLTS(Real.posInf,!lineNum,!lineNum));
d_\-{inf} => (T.FLTD(Real.negInf,!lineNum,!lineNum));
d_\+{inf} => (T.FLTD(Real.posInf,!lineNum,!lineNum));
{string}  => (makeString yytext);
{kw}      => (KW.keyword(yytext,!lineNum,!lineNum) handle Keyword =>
              (error("unknown keyword "^yytext,!lineNum,!lineNum); continue()));
":"{kw}   => (T.TYP(ident yytext,!lineNum,!lineNum));
{global}  => (T.GLO(ident yytext,!lineNum,!lineNum));
"%"{kw}   => (T.TMP(ident yytext,!lineNum,!lineNum));
"@"{kw}   => (T.LBL(ident yytext,!lineNum,!lineNum));
"#".*     => (continue());
"+"       => (T.PLUS(!lineNum,!lineNum));
"="       => (T.EQ(!lineNum,!lineNum));
","       => (T.COMMA(!lineNum,!lineNum));
"("       => (T.LPAREN(!lineNum,!lineNum));
")"       => (T.RPAREN(!lineNum,!lineNum));
"{"       => (T.LBRACE(!lineNum,!lineNum));
"}"       => (T.RBRACE(!lineNum,!lineNum));
.         => (error("invalid character "^yytext,!lineNum,!lineNum); continue());
