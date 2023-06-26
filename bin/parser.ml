open Encoding

let () = 
  (* Ler input de um ficheiro *)
  let input_file_name = Sys.argv.(1) in
  let input_channel = open_in input_file_name in
  let input_contents = input_line input_channel in
  close_in input_channel;
  let e = Utils.parse input_contents in 
  let e' = Slicing.slice e in
  (*let e' = Normalize.normalize e in*)
  Printf.printf "Hello Parser: %s \n" (Expression.to_string e');
  try
  let s = Batch.create () in
  let b = Batch.check_sat s [e] in 
  Printf.printf "Result: %s \n" (string_of_bool b)
  with _ -> (
  List.iter (fun s -> Printf.printf "%s" s) !Common.global;   
  ()
  )
