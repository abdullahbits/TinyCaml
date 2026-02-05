{
open Lexing
open Parser
open Errors

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

(** Helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(** Regexes for tokens *)
let number = '-'? digit+ ('.' digit*)?
let id = (alpha)(alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(** Lexer rules *)
rule read_token =
  parse
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "!"        { NOT }
  | "&&"       { AND }
  | "||"       { OR }
  | '<'        { LT }
  | '>'        { GT }
  | "=="       { EQ }
  | "!="       { NOT_EQ }
  | "<="       { LTE }
  | ">="       { GTE }
  | "+"        { PLUS }
  | "*"        { TIMES }
  | "="        { EQUALS }
  | "->"       { ARROW }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "fun"      { FUN }
  | "let"      { LET }
  | "in"       { IN }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | whitespace { read_token lexbuf }
  | newline    { next_line lexbuf; read_token lexbuf }
  | number     { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | id         { ID (Lexing.lexeme lexbuf) }
  | eof        { EOF }
  | _          { raise_syntax_error lexbuf.lex_curr_p (Lexing.lexeme lexbuf) }
