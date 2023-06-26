
{
  open Lexing
  open Parser

}

let digit   = ['0' - '9']
let letter  = ['a' - 'z' 'A' - 'Z']
let int     = '-'?digit+
let frac    = '.' digit*
let exp     = ['e' 'E'] ['-' '+']? digit+
let float   = digit* frac? exp?|"nan"|"inf"
let bool    = "true"|"false"
let white   = (' '|'\t')+
let newline = '\r'|'\n'|"\r\n"

let lparent = "("
let rparent = ")"

(* Symbolic *)
let cardinal = '#'
let uscore = '_'
let var = (letter)(letter|digit)*


(* Int Operators*)
let add_int = "+_int"
let and_int = "&_int"
let or_int = "|_int"
let sub_int = "-_int"
let div_int = "/_int"
let xor_int = "^_int"
let mul_int = "*_int"
let shl_int = "<<_int"
let shra_int = ">>a_int"
let shrl_int = ">>l_int"
let rem_int = "%_int"
let neg_int = "!_int"
let eq_int = "==_int"
let lt_int = "<_int"
let le_int = "<=_int"
let ne_int = "!=_int"
let gt_int = ">_int"
let ge_int = ">=_int"

(* Bool Operators *)
let and_bool = "&_bool"
let or_bool = "|_bool"
let xor_bool = "^_bool"
let not_bool = "!_bool"
let eq_bool = "==_bool"
let ne_bool = "!=_bool"

(* Str Operators *)
let nth_str = "nth_str" (* ??? *)
let len_str = "len_str"
let eq_str = "==_str"
let ne_str = "!=_str"
let tofloat_str = "toFloat_str"

(* I32 Operators *)
let add_i32 = "+_i32"
let and_i32 = "&_i32"
let or_i32 = "|_i32"
let sub_i32 = "-_i32"
let divU_i32 = "/u_i32"
let divS_i32 = "/s_i32"
let xor_i32 = "^_i32"
let mul_i32 = "*_i32"
let shl_i32 = "<<_i32"
let shrU_i32 = ">>u_i32"
let shrS_i32 = ">>_i32"
let remU_i32 = "%u_i32"
let remS_i32 = "%_i32"
let clz_i32 = "clz_i32"
let eq_i32 = "==_i32"
let ltU_i32 = "<u_i32"
let ltS_i32 = "<_i32"
let leU_i32 = "<=u_i32"
let leS_i32 = "<=_i32"
let ne_i32 = "!=_i32"
let gtU_i32 = ">u_i32"
let gtS_i32 = ">_i32"
let geU_i32 = ">=u_i32"
let geS_i32 = ">=_i32"
let truncSF32_i32 = "truncSF32_i32"
let truncUF32_i32 = "truncUF32_i32"
let truncSF64_i32 = "truncSF64_i32"
let truncUF64_i32 = "truncUF64_i32"
let reinterpretFloat_i32 = "reinterpretFloat_i32"
let wrapI64_i32 = "wrapI64_i32"
let extendSI32_i32 = "extendSI32_i32"
let extendUI32_i32 = "extendUI32_i32"

(* I64 Operators *)
let add_i64 = "+_i64"
let and_i64 = "&_i64"
let or_i64 = "|_i64"
let sub_i64 = "-_i64"
let divU_i64 = "/u_i64"
let divS_i64 = "/_i64"
let xor_i64 = "^_i64"
let mul_i64 = "*_i64"
let shl_i64 = "<<_i64"
let shrU_i64 = ">>u_i64"
let shrS_i64 = ">>_i64"
let remU_i64 = "%u_i64"
let remS_i64 = "%_i64"
let clz_i64 = "clz_i64"
let eq_i64 = "==_i64"
let ltU_i64 = "<u_i64"
let ltS_i64 = "<_i64"
let leU_i64 = "<=u_i64"
let leS_i64 = "<=_i64"
let ne_i64 = "!=_i64"
let gtU_i64 = ">u_i64"
let gtS_i64 = ">_i64"
let geU_i64 = ">=u_i64"
let geS_i64 = ">=_i64"
let truncSF32_i64 = "truncSF32_i64"
let truncUF32_i64 = "truncUF32_i64"
let truncSF64_i64 = "truncSF64_i64"
let truncUF64_i64 = "truncUF64_i64"
let reinterpretFloat_i64 = "reinterpretFloat_i64"
let wrapI64_i64 = "wrapI64_i64"
let extendSI32_i64 = "extendSI32_i64"
let extendUI32_i64 = "extendUI32_i64"

(* F32 Operators *)
let add_f32 = "+_f32"
let sub_f32 = "-_f32"
let div_f32 = "/_f32"
let mul_f32 = "*_f32"
let min_f32 = "min_f32"
let max_f32 = "max_f32"
let neg_f32 = "!_f32"
let abs_f32 = "abs_f32"
let sqrt_f32 = "sqrt_f32"
let nearest_f32 = "nearest_f32"
let eq_f32 = "==_f32"
let lt_f32 = "<_f32"
let le_f32 = "<=_f32"
let ne_f32 = "!=_f32"
let gt_f32 = ">_f32"
let ge_f32 = ">=_f32"
let demoteF64_f32 = "demoteF64_f32"
let convertSI32_f32 = "convertSI32_f32"
let convertUI32_f32 = "convertUI32_f32"
let convertSI64_f32 = "convertSI64_f32"
let convertUI64_f32 = "convertUI64_f32"
let reinterpretInt_f32 = "reinterpretInt_f32"
let promoteF32_f32 = "promoteF32_f32"


(* F64 Operators *)
let add_f64 = "+_f64"
let sub_f64 = "-_f64"
let div_f64 = "/_f64"
let mul_f64 = "*_f64"
let min_f64 = "min_f64"
let max_f64 = "max_f64"
let neg_f64 = "!_f64"
let abs_f64 = "abs_f64"
let sqrt_f64 = "sqrt_f64"
let nearest_f64 = "nearest_f64"
let eq_f64 = "==_f64"
let lt_f64 = "<_f64"
let le_f64 = "<=_f64"
let ne_f64 = "!=_f64"
let gt_f64 = ">_f64"
let ge_f64 = ">=_f64"
let demoteF64_f64 = "demoteF64_f64"
let convertSI32_f64 = "convertSI32_f64"
let convertUI32_f64 = "convertUI32_f64"
let convertSI64_f64 = "convertSI64_f64"
let convertUI64_f64 = "convertUI64_f64"
let reinterpretInt_f64 = "reinterpretInt_f64"
let promoteF32_f64 = "promoteF32_f64"


rule read =
  parse
  | white             { read lexbuf }
  | newline           { new_line lexbuf; read lexbuf }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool              { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | cardinal          { CARDINAL }
  | uscore            { USCORE }
  | var               { VAR (Lexing.lexeme lexbuf) }
  | lparent           { LPARENT }
  | rparent           { RPARENT }
  | eof               { EOF }
  (* Int Op*)
  | add_int           { ADD_INT }
  | and_int           { AND_INT }
  | or_int            { OR_INT }
  | sub_int           { SUB_INT }
  | div_int           { DIV_INT }
  | xor_int           { XOR_INT }
  | mul_int           { MUL_INT }
  | shl_int           { SHL_INT }
  | shra_int          { SHRA_INT }
  | shrl_int          { SHRL_INT }
  | rem_int           { REM_INT }
  | neg_int           { NEG_INT }
  | eq_int            { EQ_INT }
  | lt_int            { LT_INT }
  | le_int            { LE_INT }
  | ne_int            { NE_INT }
  | gt_int            { GT_INT }
  | ge_int            { GE_INT }
  (* Bool Op *)
  | and_bool          { AND_BOOL }
  | or_bool           { OR_BOOL }
  | xor_bool          { XOR_BOOL }
  | not_bool          { NOT_BOOL }
  | eq_bool           { EQ_BOOL }
  | ne_bool           { NE_BOOL }
  (* Str Op *)
  | nth_str           { NTH_STR }
  | len_str           { LEN_STR }
  | eq_str            { EQ_STR }
  | ne_str            { NE_STR }
  | tofloat_str       { TOFLOAT_STR }
  (* I32 Op *)
  | add_i32           { ADD_I32 }
  | and_i32           { AND_I32 }
  | or_i32            { OR_I32 }
  | sub_i32           { SUB_I32 }
  | divU_i32          { DIVU_I32 }
  | divS_i32          { DIVS_I32 }
  | xor_i32           { XOR_I32 }
  | mul_i32           { MUL_I32 }
  | shl_i32           { SHL_I32 }
  | shrU_i32          { SHRU_I32 }
  | shrS_i32          { SHRS_I32 }
  | remU_i32          { REMU_I32 }
  | remS_i32          { REMS_I32 }
  | clz_i32           { CLZ_I32 }
  | eq_i32            { EQ_I32 }
  | ltU_i32           { LTU_I32 }
  | ltS_i32           { LTS_I32 }
  | leU_i32           { LEU_I32 }
  | leS_i32           { LES_I32 }
  | ne_i32            { NE_I32 }
  | gtU_i32           { GTU_I32 }
  | gtS_i32           { GTS_I32 }
  | geU_i32           { GEU_I32 }
  | geS_i32           { GES_I32 }
  | truncSF32_i32     { TRUNCSF32_I32 }
  | truncUF32_i32     { TRUNCUF32_I32 }
  | truncSF64_i32     { TRUNCSF64_I32 }
  | truncUF64_i32     { TRUNCUF64_I32 }
  | reinterpretFloat_i32 { REINTERPRETFLOAT_I32 }
  | wrapI64_i32       { WRAPI64_I32 }
  | extendSI32_i32    { EXTENDSI32_I32 }
  | extendUI32_i32    { EXTENDUI32_I32 }
  (* I64 Op *)
  | add_i64           { ADD_I64 }
  | and_i64           { AND_I64 }
  | or_i64            { OR_I64 }
  | sub_i64           { SUB_I64 }
  | divU_i64          { DIVU_I64 }
  | divS_i64          { DIVS_I64 }
  | xor_i64           { XOR_I64 }
  | mul_i64           { MUL_I64 }
  | shl_i64           { SHL_I64 }
  | shrU_i64          { SHRU_I64 }
  | shrS_i64          { SHRS_I64 }
  | remU_i64          { REMU_I64 }
  | remS_i64          { REMS_I64 }
  | clz_i64           { CLZ_I64 }
  | eq_i64            { EQ_I64 }
  | ltU_i64           { LTU_I64 }
  | ltS_i64           { LTS_I64 }
  | leU_i64           { LEU_I64 }
  | leS_i64           { LES_I64 }
  | ne_i64            { NE_I64 }
  | gtU_i64           { GTU_I64 }
  | gtS_i64           { GTS_I64 }
  | geU_i64           { GEU_I64 }
  | geS_i64           { GES_I64 }
  | truncSF32_i64     { TRUNCSF32_I64 }
  | truncUF32_i64     { TRUNCUF32_I64 }
  | truncSF64_i64     { TRUNCSF64_I64 }
  | truncUF64_i64     { TRUNCUF64_I64 }
  | reinterpretFloat_i64 { REINTERPRETFLOAT_I64 }
  | wrapI64_i64       { WRAPI64_I64 }
  | extendSI32_i64    { EXTENDSI32_I64 }
  | extendUI32_i64    { EXTENDUI32_I64 }
  (* F32 Op *)
  | add_f32            { ADD_F32 }
  | sub_f32            { SUB_F32 }
  | div_f32            { DIV_F32 }
  | mul_f32            { MUL_F32 }
  | min_f32            { MIN_F32 }
  | max_f32            { MAX_F32 }
  | neg_f32            { NEG_F32 }
  | abs_f32            { ABS_F32 }
  | sqrt_f32           { SQRT_F32 }
  | nearest_f32        { NEAREST_F32 }
  | eq_f32             { EQ_F32 }
  | lt_f32             { LT_F32 }
  | le_f32             { LE_F32 }
  | ne_f32             { NE_F32 }
  | gt_f32             { GT_F32 }
  | ge_f32             { GE_F32 }
  | demoteF64_f32      { DEMOTEF64_F32 }
  | convertSI32_f32    { CONVERTSI32_F32 }
  | convertUI32_f32    { CONVERTUI32_F32 }
  | convertSI64_f32    { CONVERTSI64_F32 }
  | convertUI64_f32    { CONVERTUI64_F32 }
  | reinterpretInt_f32 { REINTERPRETINT_F32 }
  | promoteF32_f32     { PROMOTEF32_F32 }
  (* F64 Op *)
  | add_f64            { ADD_F64 }
  | sub_f64            { SUB_F64 }
  | div_f64            { DIV_F64 }
  | mul_f64            { MUL_F64 }
  | min_f64            { MIN_F64 }
  | max_f64            { MAX_F64 }
  | neg_f64            { NEG_F64 }
  | abs_f64            { ABS_F64 }
  | sqrt_f64           { SQRT_F64 }
  | nearest_f64        { NEAREST_F64 }
  | eq_f64             { EQ_F64 }
  | lt_f64             { LT_F64 }
  | le_f64             { LE_F64 }
  | ne_f64             { NE_F64 }
  | gt_f64             { GT_F64 }
  | ge_f64             { GE_F64 }
  | demoteF64_f64      { DEMOTEF64_F64 }
  | convertSI32_f64    { CONVERTSI32_F64 }
  | convertUI32_f64    { CONVERTUI32_F64 }
  | convertSI64_f64    { CONVERTSI64_F64 }
  | convertUI64_f64    { CONVERTUI64_F64 }
  | reinterpretInt_f64 { REINTERPRETINT_F64 }
  | promoteF32_f64     { PROMOTEF32_F64 }

