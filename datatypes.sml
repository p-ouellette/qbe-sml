structure QbeTypes =
struct

datatype ty = Word
            | Long
            | Single
            | Double
            | Byte
            | HalfWord
            | Aggregate of Atom.atom

datatype data_item = DataSymbol of Atom.atom
                   | DataStr of string
                   | DataConst of int

datatype data_field = DataFieldTy of ty * data_item list
                    | DataFieldZ of int

datatype value = Temp of Atom.atom
               | Global of Atom.atom
               | Const of int

datatype instr = Add of value * value
               | Sub of value * value
               | Div of value * value
               | Mul of value * value
               | Neg of value
               | Udiv of value * value
               | Rem of value * value
               | Urem of value * value
               | Or of value * value
               | Xor of value * value
               | And of value * value
               | Sar of value * value
               | Shr of value * value
               | Shl of value * value
               | Stored of value * value
               | Stores of value * value
               | Storel of value * value
               | Storew of value * value
               | Storeh of value * value
               | Storeb of value * value
               | Loadd of value
               | Loads of value
               | Loadl of value
               | Loadw of value
               | Loadsw of value
               | Loaduw of value
               | Loadsh of value
               | Loaduh of value
               | Loadsb of value
               | Loadub of value
               | Alloc4 of value
               | Alloc8 of value
               | Alloc16 of value
               | Ceq of ty * value * value
               | Cne of ty * value * value
               | Csle of ty * value * value
               | Cslt of ty * value * value
               | Csge of ty * value * value
               | Csgt of ty * value * value
               | Cule of ty * value * value
               | Cult of ty * value * value
               | Cuge of ty * value * value
               | Cugt of ty * value * value
               | Cle of ty * value * value
               | Clt of ty * value * value
               | Cge of ty * value * value
               | Cgt of ty * value * value
               | Co of ty * value * value
               | Cuo of ty * value * value
               | Dtosi of value
               | Dtoui of value
               | Exts of value
               | Extsb of value
               | Extsh of value
               | Extsw of value
               | Extub of value
               | Extuh of value
               | Extuw of value
               | Sltof of value
               | Ultof of value
               | Stosi of value
               | Stoui of value
               | Swtof of value
               | Uwtof of value
               | Truncd of value
               | Cast of value
               | Copy of value
               | Call of Atom.atom * (ty * value) list
               | Vastart of value
               | Vaarg of value
               | Phi of (Atom.atom * value) list
               | Jmp of Atom.atom
               | Jnz of value * Atom.atom * Atom.atom
               | Ret of value option
               | Retw of value option
               | Nop

datatype stmt = Assign of Atom.atom * ty * instr
              | Instr of instr

type typedef = {name: Atom.atom,
                align: int option,
                items: (ty * int) list}

type darktypedef = {name: Atom.atom, align: int, size: int}

type datadef = {name: Atom.atom,
                exported: bool,
                align: int option,
                fields: data_field list}

type block = {label: Atom.atom,
              stmts: stmt list,
              jump: instr option}

type func = {name: Atom.atom,
             exported: bool,
             params: (ty * Atom.atom) list,
             variadic: bool,
             result: ty option,
             blocks: block list}

datatype def = Type of typedef
             | OpaqueType of darktypedef
             | Data of datadef
             | Function of func

type module = def list

end
