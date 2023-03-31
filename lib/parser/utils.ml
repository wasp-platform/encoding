open Lexing


type token = [%import: Parser.token] [@@deriving show]

let print_position (outx : Format.formatter) (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Line number: %d. File: %s\n" pos.pos_lnum pos.pos_fname;
  Format.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)


let parse_inner start (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "temp" };
  
    let module MI = Parser.MenhirInterpreter in
  let last_token = ref Parser.EOF in
  let lexer lexbuf =
    let token = Lexer.read lexbuf in
    last_token := token;
    token
  in

  MI.loop_handle
    (fun result -> result)
    (function
      | MI.Rejected -> failwith "Parser rejected input"
      | MI.HandlingError _ ->
          (* let csn = ESLMI.current_state_number e in *)
          Format.eprintf "%a, last token: %s: %s.@." print_position lexbuf
            (show_token !last_token) "Error message found";
          raise Parser.Error
      | _ -> failwith "Unexpected state in failure handler!")
    (MI.lexer_lexbuf_to_supplier lexer lexbuf)
    (start lexbuf.Lexing.lex_curr_p)


let parse (str : string) : Expression.expr =
  let lexbuf = Lexing.from_string str in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "temp" };
  parse_inner Parser.Incremental.expr_target lexbuf