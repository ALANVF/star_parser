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

| '#' (ident as id) { TAG id }
| ':' (ident as id) { PUNNED id }
| (label as lb) ':' { LABEL lb }
| ident as id { IDENT id }
| type as t { TYPE_NAME t }

| int as i { INT (Int.of_string i) }
| dec as d { DEC (Float.of_string d) }
| '"' { read_string (Buffer.create 10) lexbuf }

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
| eof { raise (SyntaxError "unexpected EOF!") }
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
| eof { raise (SyntaxError "Unexpected EOF!") }
| _ { raise (SyntaxError "wtf") }


and read_escape = parse
| '\\' { '\\' }
| 'n' { '\n' }
| 'r' { '\r' }
| 't' { '\t' }
| '"' { '"' }
| 'x' ((hex hex) as code) {
    "0x" ^ code
    |> Int.of_string
    |> Char.of_int_exn
}
| _ { raise (SyntaxError "Invalid escape!") }