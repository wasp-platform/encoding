open Encoding

(*
Consola com opções -  

ler o input de ficheiro e passar à função de parsing
*)

let _ = 
  (*let int_symb = Symbolic (`IntType, "x") in 
  Printf.printf "Hello Parser: %s" (Expression.to_string int_symb); *)
  let input = "3.2 4 5.2" in 
  let e = Utils.parse input in 
  Printf.printf "Hello Parser: %s" (Expression.to_string e);
  ()