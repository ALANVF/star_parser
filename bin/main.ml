open Star_parser

let () =
    let format_range (Lexing.{
        lex_start_p={pos_lnum=l; pos_bol=b; pos_cnum=c; _};
        lex_curr_p={pos_lnum=l'; pos_bol=b'; pos_cnum=c'; _}
    } as lexbuf) =
        let open Format in
        let lines = if l = l'
            then sprintf "line %i" l
            else sprintf "lines %i-%i" l l'
        in
        let chars = if c = c'
            then sprintf "character %i" c
            else sprintf "characters %i-%i" c c'
        in

        sprintf "on %s, %s near:\n%s\n%s"
            lines
            chars
            (Lexing.sub_lexeme
                lexbuf
                (*(Lexing.lexeme_start lexbuf)*)
                0
                (Lexing.lexeme_end lexbuf)
            )
            (String.make c ' ' ^ String.make (c' - c) '^')
    in
    
    let tokenize src =
        let lexbuf = Lexing.from_string src in
        try
            Parser.program Lexer.read_token lexbuf
        with
        | e ->
            Format.eprintf "Syntax error: Invalid syntax %s\n" (format_range lexbuf);
            raise e
    in
    let show_tokens = List.iter ([%show: Ast.Expr.t] |> Base.Fn.compose print_endline) in
    
    let test_literals = tokenize "123 45.6 #\"a\" 7890 true `true` false \"abc\" $0 $.1 `script` $..2 banana" in
    show_tokens test_literals;

    (*let test_types = tokenize "Star.Core.Func[_, Star.Core.Array[Star.Native.UInt8]] _[_, _] _.Banana" in
    show_tokens test_types*)

    let test_tokens tokens =
        tokens |> List.iter (fun t ->
            print_endline "";
            print_endline ("Input: " ^ t);
            print_endline "Output:";
            t
            |> tokenize
            |> List.map(Ast.Expr.to_simple)
            |> List.map(Ast.Expr.Simple.show)
            |> List.iter(print_endline)
        )
    in

    print_endline "Object messaging:";
    test_tokens [
        "a[b]";
        "a[b: c]";
        "true[yes: 1 no: 2]";
        "true[yes: 1, no: 2]";
        "true[yes: 1
            no: 2]";
        "true[yes: 1,
            no: 2]";
        "1[to: 10][Array[Int]]";
        "!1[Bool]";
        "(!1)[Bool]"
    ];

    print_endline "\nUnary operators:";
    test_tokens [
        "--123"; (* might want to fix that *)
        "~abc[def]";
        "!~-\"thing\"";
        "-a++";
        "++a?";
        "#a ~b--";
        "#a b[c]";
        "#a b[c][d]"
    ];

    print_endline "\nBinary operators:";
    test_tokens [
        "a.b";
        "a.b.c";
        "A.b";
        "A.b.c";
        "A.B.c";
        "A.B.C.d";
        "A[B[C, D], E].F[G].h.i";
        "a ** b";
        "a ** b ** c";
        "-a ** ~b++ ** c--";
        "1 * 2 ** 3 / 4 ** 5 ** 6 % 7";
    ];