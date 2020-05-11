type ident = string

type builtin_type =
  | Tint
  | Tuint
  | Tbool
  | Tunit
  (* EVM specifics *)
  | Thashvalue
  (* CertiKOS specifics *)
  | Tglobalpointer
  (*
  | Tval
  | Tflatmem
  *)

type method_kind =
  | MKnormal
  | MKlogical
  | MKconst
  | MKghost
  | MKconstghost

type constant =
  | CONint of int
  | CONuint of int
  | CONbool of bool
  | CONunit
  (*| CONarray_init*)
  (* CertiKOS specifics *)
  | CONglobalpointer_undef
  (*
  | CONval_undef
  | CONflatmem_empty
  *)

type unop =
  | OPneg
  | OPnot
  | OPbitnot
  | OPbitneg  (* ??? *)
  | OPsha_1

type binop =
  | OPplus
  | OPminus
  | OPtimes
  | OPdivide
  | OPremainder
  | OPand
  | OPor
  | OPeq
  | OPne
  | OPlt
  | OPle
  | OPgt
  | OPge
  | OPshl
  | OPshr
  | OPxor
  | OPbitand  (* ??? *)
  | OPbitor  (* ??? *)
  | OPsha_2

let string_of_builtin_type = function
  | Tint -> "int"
  | Tuint -> "uint"
  | Tbool -> "bool"
  | Tunit -> "unit"
  | Thashvalue -> "hashvalue"
  | Tglobalpointer -> "globalpointer"
  (*
  | Tval -> "val"
  | Tflatmem -> "flatmem"
  *)

let string_of_method_kind = function
  | MKnormal -> ""
  | MKlogical -> "logical "
  | MKconst -> "const "
  | MKghost -> "ghost "
  | MKconstghost -> "const ghost "

let string_of_constant = function
  | CONint n -> string_of_int n
  | CONuint n -> "(Int256.repr " ^ string_of_int n ^ ")"
  | CONbool b -> if b then "true" else "false"
  | CONunit -> "()"
  (*| CONarray_init -> "array_init"*)
  | CONglobalpointer_undef -> "GLOBUndef"
  (*
  | CONval_undef -> "Vundef"
  | CONflatmem_empty -> "empty_flatmem"
  *)

let string_of_unop = function
  | OPneg -> "-"
  | OPnot -> "!"
  | OPbitnot -> "~"
  | OPbitneg -> "~"
  | OPsha_1 -> "keccak256 "

let string_of_binop = function
  | OPplus -> "+"
  | OPminus -> "-"
  | OPtimes -> "*"
  | OPdivide -> "/"
  | OPremainder -> "%"
  | OPand -> "/\\"
  | OPor -> "\\/"
  | OPeq -> "="
  | OPne -> "<>"
  | OPlt -> "<"
  | OPle -> "<="
  | OPgt -> ">"
  | OPge -> ">="
  | OPshl -> "<<"
  | OPshr -> ">>"
  | OPxor -> "^"
  | OPbitand -> "&"
  | OPbitor -> "|"
  | OPsha_2 -> "keccak256"
