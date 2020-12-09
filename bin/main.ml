open Star_parser

let () =
    let format_range (Lexing.{
        lex_start_p={pos_lnum=l; pos_bol=b; pos_cnum=c; _};
        lex_curr_p={pos_lnum=l'; pos_bol=b'; pos_cnum=c'; _}
    } as lexbuf) =
        let open Format in

        let sl = l - 1 in
        let cl = l' - 1 in

        let ll = cl - sl in
        let lc = c - b - ll in
        let cc = c' - b' - ll in

        let src = Bytes.to_string lexbuf.lex_buffer in
        let lines = Base.String.split_lines src in

        let err_lines = if sl = cl
            then sprintf "line %i" l
            else sprintf "lines %i-%i" l l'
        in
        let err_chars = if lc = cc
            then sprintf "character %i" cc
            else sprintf "characters %i-%i" lc cc
        in

        let make_line = sprintf "%i | %s" in

        let code =
            if ll = 0 then
                make_line l (List.nth lines sl)
            else if l = 0 then
                failwith "todo"
            else
                lines
                |> Base.List.sub ~pos: (sl - 1) ~len: (ll + 1)
                |> List.mapi(fun i s -> make_line (l + i) s)
                |> String.concat "\n"
        in

        let bottom = if ll = 0
            then (String.make ((l' / 10) + 3 + lc) ' ' ^ String.make (if cc - lc = 0 then 1 else cc - lc) '^')
            else String.make ((l' / 10) + 3 + cc) ' ' ^ "^"
        in

        printf "%i %i %i %i\n" (c' - b' + 1) ll lc cc;
        sprintf "on %s, %s near:\n%s\n%s"
            err_lines
            err_chars
            code
            bottom
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
        "(!1)[Bool]";
        "A[b]";
        "A[B][c]";
        "A[B].C[d]"
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
        "a.b * C.d / e.f";
        "-a + -b";
        "-a - -b";
        "1 + 2 * 3"
    ];
    
    print_endline "\nParen expressions:";
    test_tokens [
        {| 1 + (
  2
  -
  (3)
 ) / 4 |};
        {| 1 + ((
  2
  -
  3
) / 4) |}
    ]