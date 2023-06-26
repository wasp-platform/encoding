open Types

let rec store_vars (tbl : (string, string) Hashtbl.t) (e : Expression.t) =
  let f = store_vars tbl in
  match e with
  | Val _ -> ()
  | SymPtr (_, e) -> f e;
  | Binop (_, e1, e2) -> f e1; f e2
  | Unop  (_, e) -> f e
  | Relop (_, e1, e2) -> f e1; f e2
  | Cvtop (_, e) -> f e
  | Extract (e, _, _) -> f e
  | Concat (e1, e2) -> f e1; f e2
  | Symbolic (_, x) -> 
    match Hashtbl.find_opt tbl x with 
    | Some _ -> ()
    | None -> Hashtbl.add tbl x x; ();;


let rec check_aux (tbl : (string, string) Hashtbl.t) (e : Expression.t) =
  let f = check_aux tbl in
  match e with
  | Val _ -> ()
  | SymPtr (_, e) -> f e
  | Binop (_, e1, e2) -> f e1; f e2
  | Unop  (_, e) -> f e
  | Relop (_, e1, e2) -> f e1; f e2
  | Cvtop (_, e) -> f e
  | Extract (e, _, _) -> f e
  | Concat (e1, e2) -> f e1; f e2
  | Symbolic (_, x) -> 
    match Hashtbl.find_opt tbl x with
    | Some _ -> Hashtbl.replace tbl "#slice" "false"; ()
    | None -> ();;

let check_expression (tbl : (string, string) Hashtbl.t) (e : Expression.t) : Expression.t =
  (* Decide if this expression is to be sliced *)
  Hashtbl.replace tbl "#slice" "true";
  check_aux tbl e;
  match Hashtbl.find tbl "#slice" with
  | "true" -> Val (Int 1)
  | _ -> e;;


let rec slice_aux (tbl : (string, string) Hashtbl.t) (e : Expression.t) : Expression.t =
  Hashtbl.add tbl "#slice" "true";
  let f = slice_aux tbl in
  let c = check_expression tbl in
  match e with
  (* Account for chained ANDs *)
  | Binop (op, e1, e2) -> (
    match op with
    | Int And | Bool And | I32 And | I64 And -> Binop(op, c e1, f e2)
    | _ -> c e
    )
  | _ -> c e;;



let slice (e : Expression.t) : Expression.t = 
  let tbl = Hashtbl.create 0 in
  match e with
  | Binop (op, e1, e2) -> (
    match op with
    | Int And | Bool And | I32 And | I64 And -> 
      store_vars tbl e1;  (* Store the vars of the first expression *)
      Binop(op, e1, slice_aux tbl e2)
    | _ -> e
    )
  | _ -> e;;

