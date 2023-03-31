%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token EOF


%type <Expression.expr> expr_target
%start expr_target

%%

expr_target:
| v = val_target; EOF; 
    { Expression.Val v }
(* binop *)
(* unop *)


val_target: 
  | f = FLOAT;
    { Expression.Num (F32 (Int32.bits_of_float f)) }
  | i = INT;
    { Expression.Int i }
  | b = BOOLEAN;
    { Expression.Bool b }

