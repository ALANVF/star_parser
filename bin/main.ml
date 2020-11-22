let () =
    let res = Star_parser.Parser.program Star_parser.Lexer.read_token (Lexing.from_string "123 45 6 7890") in
    Base.sexp_of_list Base.sexp_of_int res |> Base.Sexp.to_string |> print_endline;
    print_endline "Hello, World!"