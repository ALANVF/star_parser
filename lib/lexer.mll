{
    open Lexing
    open Parser
    open Base
    
    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with
                pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1 }

    let bad_eof () = raise (SyntaxError "Unexpected EOF!")
}


let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alnum = (lower | upper | digit | '_')
let hex = ['a'-'f' 'A'-'F' '0'-'9']

let int = '-'? digit+
let dec = '-'? digit+ '.' digit+

let ident = lower alnum*
let label = (lower | '_') alnum*
let type = upper alnum*

let horiz = [' ' '\t']

let newline = '\r' | '\n' | "\r\n"

let blank = horiz+ (newline+ horiz*)* | newline+ (horiz+ newline*)*
let line_sep = horiz* (newline+ horiz*)+
let comma_sep = blank? ',' blank?
let comma = horiz* ',' horiz*

(* worry about multiline strings and interpolation later *)
rule read_token = parse
| horiz {read_token lexbuf}
| newline {new_line lexbuf; read_token lexbuf}
| ";[" {read_multiline_comment 0 lexbuf}
| ';' {read_line_comment lexbuf}
| comma_sep {C_SEP}
| comma {C_HS}
| line_sep {L_SEP}

| "true" { BOOL true }
| "false" { BOOL false }
| "this" { THIS }

| "module" { K_MODULE }
| "macro" { K_MACRO }
| "my" { K_MY }
| "on" { K_ON }
| "return" { K_RETURN }
| "init" { K_INIT }
| "deinit" { K_DEINIT }
| "operator" { K_OPERATOR }
| "class" { K_CLASS }
| "alias" { K_ALIAS }
| "type" { K_TYPE }
| "kind" { K_KIND }
| "category" { K_CATEGORY }
| "protocol" { K_PROTOCOL }
| "is" { K_IS }
| "of" { K_OF }
| "use" { K_USE }
| "has" { K_HAS }
| "if" { K_IF }
| "orif" { K_ORIF }
| "else" { K_ELSE }
| "while" { K_WHILE }
| "for" { K_FOR }
| "do" { K_DO }
| "case" { K_CASE }
| "match" { K_MATCH }
| "at" { K_AT }
| "break" { K_BREAK }
| "next" { K_NEXT }
| "throw" { K_THROW }
| "try" { K_TRY }
| "catch" { K_CATCH }

| '#' (ident as id) { TAG id }
| ':' (ident as id) { PUNNED id }
| (label as lb) ':' { LABEL lb }
| ident as id { IDENT id }
| type as t { TYPE_NAME t }
| '`' ([^ '`' '\n' '\r' ]+ as sym) '`' { LITSYM sym }

| int as i { INT (Int.of_string i) }
| dec as d { DEC (Float.of_string d) }
| '"' { read_string (Buffer.create 10) lexbuf }
| "#\"" { read_char lexbuf }

| eof { EOF }
| _ {raise (SyntaxError "wtf")}


and read_line_comment = parse
| newline {next_line lexbuf; read_token lexbuf}
| eof {EOF}
| _ {read_line_comment lexbuf}


and read_multiline_comment depth = parse
| newline { next_line lexbuf; read_multiline_comment depth lexbuf }
| '[' { read_multiline_comment (depth + 1) lexbuf }
| ']' { if depth = 0 then read_token lexbuf
        else read_multiline_comment (depth - 1) lexbuf }
| eof { bad_eof() }
| _ { raise (SyntaxError "wtf") }


and read_string buf = parse
| '"' { STR (Buffer.contents buf) }
| newline as nl {
    next_line lexbuf;
    Buffer.add_string buf nl;
    read_string buf lexbuf
}
| '\\' newline {
    next_line lexbuf;
    read_string buf lexbuf
}
| '\\' {
    Buffer.add_char buf (read_escape lexbuf);
    read_string buf lexbuf
}
| [^ '"' '\\']+ as txt {
    Buffer.add_string buf txt;
    read_string buf lexbuf
}
| eof { bad_eof() }
| _ { raise (SyntaxError "wtf") }


and read_char = parse
| '\\' { finish_char (read_escape lexbuf) lexbuf }
| [^ '"' '\\' '\n' '\r' '\t' ] as c { finish_char c lexbuf }
| eof { bad_eof() }
| _ { raise (SyntaxError "Invalid char!") }

and finish_char c = parse
| '"' { CHAR c }
| eof { bad_eof() }
| _ {  raise (SyntaxError "Invalid char!")  }


and read_escape = parse
| '\\' { '\\' }
| 'n' { '\n' }
| 'r' { '\r' }
| 't' { '\t' }
| '"' { '"' }
| 'x' (hex hex as code) {
    "0x" ^ code
    |> Int.of_string
    |> Char.of_int_exn
}
| eof { bad_eof() }
| _ { raise (SyntaxError "Invalid escape!") }