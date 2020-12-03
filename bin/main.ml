open Star_parser

let () =
    let tokenize src = Parser.program Lexer.read_token (Lexing.from_string src) in
    let show_tokens = List.iter ([%show: Ast.expr] |> Base.Fn.compose print_endline) in

    let test_literals = tokenize "123 45.6 #\"a\" 7890 true `true` false \"abc\" $0 $.1 `script` $..2 banana" in
    show_tokens test_literals;

    (*let test_types = tokenize "Star.Core.Func[_, Star.Core.Array[Star.Native.UInt8]] _[_, _] _.Banana" in
    show_tokens test_types*)