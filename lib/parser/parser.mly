%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token LPARENT RPARENT
%token CARDINAL USCORE 
%token EOF
%token ADD_INT AND_INT OR_INT SUB_INT DIV_INT XOR_INT MUL_INT SHL_INT SHRA_INT SHRL_INT REM_INT NEG_INT EQ_INT LT_INT LE_INT NE_INT GT_INT GE_INT
%token AND_BOOL OR_BOOL XOR_BOOL NOT_BOOL EQ_BOOL NE_BOOL
%token NTH_STR LEN_STR EQ_STR NE_STR TOFLOAT_STR
%token ADD_I32 AND_I32 OR_I32 SUB_I32 DIVU_I32 DIVS_I32 XOR_I32 MUL_I32 SHL_I32 SHRU_I32 SHRS_I32 REMU_I32 REMS_I32 CLZ_I32 EQ_I32 LTU_I32 LTS_I32 LEU_I32 LES_I32 NE_I32 GTU_I32 GTS_I32 GEU_I32 GES_I32 TRUNCSF32_I32 TRUNCUF32_I32 TRUNCSF64_I32 TRUNCUF64_I32 REINTERPRETFLOAT_I32 WRAPI64_I32 EXTENDSI32_I32 EXTENDUI32_I32
%token ADD_I64 AND_I64 OR_I64 SUB_I64 DIVU_I64 DIVS_I64 XOR_I64 MUL_I64 SHL_I64 SHRU_I64 SHRS_I64 REMU_I64 REMS_I64 CLZ_I64 EQ_I64 LTU_I64 LTS_I64 LEU_I64 LES_I64 NE_I64 GTU_I64 GTS_I64 GEU_I64 GES_I64 TRUNCSF32_I64 TRUNCUF32_I64 TRUNCSF64_I64 TRUNCUF64_I64 REINTERPRETFLOAT_I64 WRAPI64_I64 EXTENDSI32_I64 EXTENDUI32_I64
%token ADD_F32 SUB_F32 DIV_F32 MUL_F32 MIN_F32 MAX_F32 NEG_F32 ABS_F32 SQRT_F32 NEAREST_F32 EQ_F32 LT_F32 LE_F32 NE_F32 GT_F32 GE_F32 DEMOTEF64_F32 CONVERTSI32_F32 CONVERTUI32_F32 CONVERTSI64_F32 CONVERTUI64_F32 REINTERPRETINT_F32 PROMOTEF32_F32
%token ADD_F64 SUB_F64 DIV_F64 MUL_F64 MIN_F64 MAX_F64 NEG_F64 ABS_F64 SQRT_F64 NEAREST_F64 EQ_F64 LT_F64 LE_F64 NE_F64 GT_F64 GE_F64 DEMOTEF64_F64 CONVERTSI32_F64 CONVERTUI32_F64 CONVERTSI64_F64 CONVERTUI64_F64 REINTERPRETINT_F64 PROMOTEF32_F64



%type <Expression.expr> expr_target
%start expr_target

%%

expr_target:
 | e = expr EOF { e } 

expr:
| LPARENT; e = expr; RPARENT
  { e }
| v = val_target
    { Expression.Val v }
| CARDINAL; x=VAR; USCORE; t=VAR 
  { Expression.Symbolic (Types.type_of_string t, x)}
(* Binops *)
| e1 = expr; op = int_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
| e1 = expr; op = bool_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
| e1 = expr; op = str_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
| e1 = expr; op = i32_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
| e1 = expr; op = i64_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
| e1 = expr; op = f32_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
| e1 = expr; op = f64_binop; e2 = expr
  { Expression.Binop (op, e1, e2) }
(* Unops *)
| op = int_unop; e1 = expr
  { Expression.Unop (op, e1) }
| op = bool_unop; e1 = expr
  { Expression.Unop (op, e1) }
| op = str_unop; e1 = expr
  { Expression.Unop (op, e1) }
| op = i32_unop; e1 = expr
{ Expression.Unop (op, e1) }
| op = i64_unop; e1 = expr
{ Expression.Unop (op, e1) }
| op = f32_unop; e1 = expr
{ Expression.Unop (op, e1) }
| op = f64_unop; e1 = expr
{ Expression.Unop (op, e1) }
(* Relops *)
| e1 = expr; op = int_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
| e1 = expr; op = bool_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
| e1 = expr; op = str_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
| e1 = expr; op = i32_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
| e1 = expr; op = i64_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
| e1 = expr; op = f32_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
| e1 = expr; op = f64_relop; e2 = expr
  { Expression.Relop (op, e1, e2) }
(* Cvtops *)
| op = str_cvtop; e1 = expr
  { Expression.Cvtop (op, e1) }
| op = i32_cvtop; e1 = expr
  { Expression.Cvtop (op, e1) }
| op = i64_cvtop; e1 = expr
  { Expression.Cvtop (op, e1) }
| op = f32_cvtop; e1 = expr
  { Expression.Cvtop (op, e1) }
| op = f64_cvtop; e1 = expr
  { Expression.Cvtop (op, e1) }



val_target: 
  | f = FLOAT;
    { Expression.Num (F32 (Int32.bits_of_float f)) }
  | i = INT;
    { Expression.Int i }
  | b = BOOLEAN;
    { Expression.Bool b }
;

(* OPERATORS *)

int_binop:
| ADD_INT    { Types.Int (IntOp.Add) }
| AND_INT    { Types.Int (IntOp.And) }
| OR_INT     { Types.Int (IntOp.Or) }
| SUB_INT    { Types.Int (IntOp.Sub) }
| DIV_INT    { Types.Int (IntOp.Div) }
| XOR_INT    { Types.Int (IntOp.Xor) }
| MUL_INT    { Types.Int (IntOp.Mul) }
| SHL_INT    { Types.Int (IntOp.Shl) }
| SHRA_INT   { Types.Int (IntOp.ShrA) }
| SHRL_INT   { Types.Int (IntOp.ShrL) }
| REM_INT    { Types.Int (IntOp.Rem) }

int_unop:
| NEG_INT    { Types.Int (IntOp.Neg) }

int_relop:
| EQ_INT     { Types.Int (IntOp.Eq) }
| LT_INT     { Types.Int (IntOp.Lt) }
| LE_INT     { Types.Int (IntOp.Le) }
| NE_INT     { Types.Int (IntOp.Ne) }
| GT_INT     { Types.Int (IntOp.Gt) }
| GE_INT     { Types.Int (IntOp.Ge) }

bool_binop: 
| AND_BOOL   { Types.Bool (BoolOp.And) }
| OR_BOOL    { Types.Bool (BoolOp.Or) }
| XOR_BOOL   { Types.Bool (BoolOp.Xor) }

bool_unop:
| NOT_BOOL   { Types.Bool (BoolOp.Not) }

bool_relop:
| EQ_BOOL    { Types.Bool (BoolOp.Eq) }
| NE_BOOL    { Types.Bool (BoolOp.Ne) }


str_binop:
| NTH_STR       { Types.Str (StrOp.Nth) }

str_unop:
| LEN_STR       { Types.Str (StrOp.Len) }

str_relop:
| EQ_STR        { Types.Str (StrOp.Eq) }
| NE_STR        { Types.Str (StrOp.Ne) }

str_cvtop:
| TOFLOAT_STR   { Types.Str (StrOp.ToFloat) }


i32_binop:
| ADD_I32    { Types.I32 (BvOp.Add) }
| AND_I32    { Types.I32 (BvOp.And) }
| OR_I32     { Types.I32 (BvOp.Or) }
| SUB_I32    { Types.I32 (BvOp.Sub) }
| DIVU_I32   { Types.I32 (BvOp.DivU) }
| DIVS_I32   { Types.I32 (BvOp.DivS) }
| XOR_I32    { Types.I32 (BvOp.Xor) }
| MUL_I32    { Types.I32 (BvOp.Mul) }
| SHL_I32    { Types.I32 (BvOp.Shl) }
| SHRU_I32   { Types.I32 (BvOp.ShrU) }
| SHRS_I32   { Types.I32 (BvOp.ShrS) }
| REMU_I32   { Types.I32 (BvOp.RemU) }
| REMS_I32   { Types.I32 (BvOp.RemS) }

i32_unop:
| CLZ_I32    { Types.I32 (BvOp.Clz) }

i32_relop:
| EQ_I32     { Types.I32 (BvOp.Eq) }
| LTU_I32    { Types.I32 (BvOp.LtU) }
| LTS_I32    { Types.I32 (BvOp.LtS) }
| LEU_I32    { Types.I32 (BvOp.LeU) }
| LES_I32    { Types.I32 (BvOp.LeS) }
| NE_I32     { Types.I32 (BvOp.Ne) }
| GTU_I32    { Types.I32 (BvOp.GtU) }
| GTS_I32    { Types.I32 (BvOp.GtS) }
| GEU_I32    { Types.I32 (BvOp.GeU) }
| GES_I32    { Types.I32 (BvOp.GeS) }

i32_cvtop:
| TRUNCSF32_I32       { Types.I32 (BvOp.TruncSF32) }
| TRUNCUF32_I32       { Types.I32 (BvOp.TruncUF32) }
| TRUNCSF64_I32       { Types.I32 (BvOp.TruncSF64) }
| TRUNCUF64_I32       { Types.I32 (BvOp.TruncUF64) }
| REINTERPRETFLOAT_I32 { Types.I32 (BvOp.ReinterpretFloat) }
| WRAPI64_I32         { Types.I32 (BvOp.WrapI64) }
| EXTENDSI32_I32      { Types.I32 (BvOp.ExtendSI32) }
| EXTENDUI32_I32      { Types.I32 (BvOp.ExtendUI32) }

i64_binop:
| ADD_I64    { Types.I64 (BvOp.Add) }
| AND_I64    { Types.I64 (BvOp.And) }
| OR_I64     { Types.I64 (BvOp.Or) }
| SUB_I64    { Types.I64 (BvOp.Sub) }
| DIVU_I64   { Types.I64 (BvOp.DivU) }
| DIVS_I64   { Types.I64 (BvOp.DivS) }
| XOR_I64    { Types.I64 (BvOp.Xor) }
| MUL_I64    { Types.I64 (BvOp.Mul) }
| SHL_I64    { Types.I64 (BvOp.Shl) }
| SHRU_I64   { Types.I64 (BvOp.ShrU) }
| SHRS_I64   { Types.I64 (BvOp.ShrS) }
| REMU_I64   { Types.I64 (BvOp.RemU) }
| REMS_I64   { Types.I64 (BvOp.RemS) }

i64_unop:
| CLZ_I64    { Types.I64 (BvOp.Clz) }

i64_relop:
| EQ_I64     { Types.I64 (BvOp.Eq) }
| LTU_I64    { Types.I64 (BvOp.LtU) }
| LTS_I64    { Types.I64 (BvOp.LtS) }
| LEU_I64    { Types.I64 (BvOp.LeU) }
| LES_I64    { Types.I64 (BvOp.LeS) }
| NE_I64     { Types.I64 (BvOp.Ne) }
| GTU_I64    { Types.I64 (BvOp.GtU) }
| GTS_I64    { Types.I64 (BvOp.GtS) }
| GEU_I64    { Types.I64 (BvOp.GeU) }
| GES_I64    { Types.I64 (BvOp.GeS) }

i64_cvtop:
| TRUNCSF32_I64       { Types.I64 (BvOp.TruncSF32) }
| TRUNCUF32_I64       { Types.I64 (BvOp.TruncUF32) }
| TRUNCSF64_I64       { Types.I64 (BvOp.TruncSF64) }
| TRUNCUF64_I64       { Types.I64 (BvOp.TruncUF64) }
| REINTERPRETFLOAT_I64 { Types.I64 (BvOp.ReinterpretFloat) }
| WRAPI64_I64         { Types.I64 (BvOp.WrapI64) }
| EXTENDSI32_I64      { Types.I64 (BvOp.ExtendSI32) }
| EXTENDUI32_I64      { Types.I64 (BvOp.ExtendUI32) }

f32_binop:
| ADD_F32    { Types.F32 (FloatOp.Add) }
| SUB_F32    { Types.F32 (FloatOp.Sub) }
| DIV_F32    { Types.F32 (FloatOp.Div) }
| MUL_F32    { Types.F32 (FloatOp.Mul) }
| MIN_F32    { Types.F32 (FloatOp.Min) }
| MAX_F32    { Types.F32 (FloatOp.Max) }


f32_unop:
| NEG_F32    { Types.F32 (FloatOp.Neg) }
| ABS_F32    { Types.F32 (FloatOp.Abs) }
| SQRT_F32   { Types.F32 (FloatOp.Sqrt) }
| NEAREST_F32 { Types.F32 (FloatOp.Nearest) }

f32_relop:
| EQ_F32     { Types.F32 (FloatOp.Eq) }
| LT_F32     { Types.F32 (FloatOp.Lt) }
| LE_F32     { Types.F32 (FloatOp.Le) }
| NE_F32     { Types.F32 (FloatOp.Ne) }
| GT_F32     { Types.F32 (FloatOp.Gt) }
| GE_F32     { Types.F32 (FloatOp.Ge) }

f32_cvtop:
| DEMOTEF64_F32             { Types.F32 (FloatOp.DemoteF64) }
| CONVERTSI32_F32           { Types.F32 (FloatOp.ConvertSI32) }
| CONVERTUI32_F32           { Types.F32 (FloatOp.ConvertUI32) }
| CONVERTSI64_F32           { Types.F32 (FloatOp.ConvertSI64) }
| CONVERTUI64_F32           { Types.F32 (FloatOp.ConvertUI64) }
| REINTERPRETINT_F32        { Types.F32 (FloatOp.ReinterpretInt) }
| PROMOTEF32_F32            { Types.F32 (FloatOp.PromoteF32) }

f64_binop:
| ADD_F64    { Types.F64 (FloatOp.Add) }
| SUB_F64    { Types.F64 (FloatOp.Sub) }
| DIV_F64    { Types.F64 (FloatOp.Div) }
| MUL_F64    { Types.F64 (FloatOp.Mul) }
| MIN_F64    { Types.F64 (FloatOp.Min) }
| MAX_F64    { Types.F64 (FloatOp.Max) }


f64_unop:
| NEG_F64    { Types.F64 (FloatOp.Neg) }
| ABS_F64    { Types.F64 (FloatOp.Abs) }
| SQRT_F64   { Types.F64 (FloatOp.Sqrt) }
| NEAREST_F64 { Types.F64 (FloatOp.Nearest) }

f64_relop:
| EQ_F64     { Types.F64 (FloatOp.Eq) }
| LT_F64     { Types.F64 (FloatOp.Lt) }
| LE_F64     { Types.F64 (FloatOp.Le) }
| NE_F64     { Types.F64 (FloatOp.Ne) }
| GT_F64     { Types.F64 (FloatOp.Gt) }
| GE_F64     { Types.F64 (FloatOp.Ge) }

f64_cvtop:
| DEMOTEF64_F64             { Types.F64 (FloatOp.DemoteF64) }
| CONVERTSI32_F64           { Types.F64 (FloatOp.ConvertSI32) }
| CONVERTUI32_F64           { Types.F64 (FloatOp.ConvertUI32) }
| CONVERTSI64_F64           { Types.F64 (FloatOp.ConvertSI64) }
| CONVERTUI64_F64           { Types.F64 (FloatOp.ConvertUI64) }
| REINTERPRETINT_F64        { Types.F64 (FloatOp.ReinterpretInt) }
| PROMOTEF32_F64            { Types.F64 (FloatOp.PromoteF32) }
