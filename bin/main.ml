open Star_parser

let () =
    let res = Parser.program Lexer.read_token (Lexing.from_string "123 45.6 #\"a\" 7890 true false \"abc\" $0 $.1 $..2 ") in
    (*let fmt = [%show: Ast.expr list] res in
    print_endline fmt;*)
    List.iter ([%show: Ast.expr] |> Base.Fn.compose print_endline) res