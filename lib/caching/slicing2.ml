
module SExpr = struct 
  type t = (String.t * Types.expr_type)

  let compare (s1, _) (s2, _) = String.compare s1 s2
end 

module SS = Set.Make(SExpr) 

let expr_vars (e : Expression.t) : SS.t = 
  SS.of_list (Expression.get_symbols e)


let rec remaining_vars_aux (xs : SS.t) (yss : SS.t list) : SS.t = 
  let xs' 
    = List.fold_left
      (fun ac ys ->
        if (not (SS.disjoint ac ys))
          then SS.union ac ys 
          else ac    
      ) xs yss in 
  if SS.equal xs' xs 
    then xs' 
    else remaining_vars_aux xs' yss 

let remaining_vars (e : Expression.t) (es : Expression.t list) : SS.t = 
  remaining_vars_aux (expr_vars e) (List.map expr_vars es) 

let slice (e : Expression.t) (es : Expression.t list) : Expression.t list = 
  let xs = remaining_vars e es in 
  List.filter (fun e' -> not (SS.disjoint xs (expr_vars e'))) es



(**

e: x > y + 1 
es: y > z; z > 3

x > y -> x, y 
x < 1000 -> x, y  
y > z -> x, y, z 
z > 1000

x > 1000
x > y 
y > z 
w = 3 


*)


