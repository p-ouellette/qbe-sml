structure QbeTypes : QBE_TYPES =
struct

datatype ty = W
            | L
            | S
            | D
            | B
            | H
            | Aggr of Atom.atom

fun sameTy (W, W) = true
  | sameTy (L, L) = true
  | sameTy (S, S) = true
  | sameTy (D, D) = true
  | sameTy (B, B) = true
  | sameTy (H, H) = true
  | sameTy (Aggr a, Aggr b) = Atom.same(a, b)
  | sameTy _ = false

type typedef = {name: Atom.atom,
                align: int option,
                items: (ty * int) list}

type darktypedef = {name: Atom.atom, align: int, size: int}

type section = {name: string, flags: string option}

type linkage = {exported: bool, section: section option}

datatype const = Int of Int64.int
               | Flts of Real32.real
               | Fltd of Real64.real

datatype dataitem = DataSym of Atom.atom * int
                  | DataStr of string
                  | DataCon of const

datatype datafield = DataTy of ty * dataitem list
                   | DataZ of int

type datadef = {name: Atom.atom,
                linkage: linkage,
                align: int option,
                fields: datafield list}

datatype value = Tmp of Atom.atom
               | Glo of Atom.atom
               | Con of const

type phi = {temp: (Atom.atom * ty),
            args: (Atom.atom * value) list}

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
               | Ceqd of value * value
               | Ceql of value * value
               | Ceqs of value * value
               | Ceqw of value * value
               | Cged of value * value
               | Cges of value * value
               | Cgtd of value * value
               | Cgts of value * value
               | Cled of value * value
               | Cles of value * value
               | Cltd of value * value
               | Clts of value * value
               | Cned of value * value
               | Cnel of value * value
               | Cnes of value * value
               | Cnew of value * value
               | Cod of value * value
               | Cos of value * value
               | Csgel of value * value
               | Csgew of value * value
               | Csgtl of value * value
               | Csgtw of value * value
               | Cslel of value * value
               | Cslew of value * value
               | Csltl of value * value
               | Csltw of value * value
               | Cugel of value * value
               | Cugew of value * value
               | Cugtl of value * value
               | Cugtw of value * value
               | Culel of value * value
               | Culew of value * value
               | Cultl of value * value
               | Cultw of value * value
               | Cuod of value * value
               | Cuos of value * value
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
               | Vaarg of value

type call = {result: (Atom.atom * ty) option,
             name: Atom.atom,
             envp: value option,
             args: (ty * value) list,
             vararg: int option}

datatype stmt = Assign of Atom.atom * ty * instr
              | Stored of value * value
              | Stores of value * value
              | Storel of value * value
              | Storew of value * value
              | Storeh of value * value
              | Storeb of value * value
              | Call of call
              | Vastart of value
              | Nop

datatype jump = Jmp of Atom.atom
              | Jnz of value * Atom.atom * Atom.atom
              | Ret of value option
              | Retw of value option

type block = {label: Atom.atom,
              phis: phi list,
              stmts: stmt list,
              jump: jump option}

type func = {name: Atom.atom,
             linkage: linkage,
             params: (ty * Atom.atom) list,
             envp: Atom.atom option,
             variadic: bool,
             result: ty option,
             blocks: block list}

datatype def = Type of typedef
             | OpaqueType of darktypedef
             | Data of datadef
             | Function of func

end
