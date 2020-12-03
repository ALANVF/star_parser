open Base

type loc = [%import: Lexing.position] [@@deriving show]

type 't delims_loc = loc * 't * loc [@@deriving show]

type ident = loc * string [@@deriving show]

module Type = struct
    type segment =
        | Wildcard 
        | Name of string
    [@@deriving show]

    type params = t list delims_loc [@@deriving show]

    and t = (loc * segment * params option) list [@@deriving show]
end

type types_spec = [
    | `One of Type.t
    | `Many of Type.t list delims_loc
] [@@deriving show]

module Prefix = struct
    type t =
        | Incr
        | Decr
        | Neg
        | Not
        | Compl
    [@@deriving show]
end

module Postfix = struct
    type t =
        | Incr
        | Decr
        | Truthy
    [@@deriving show]
end

module Infix = struct
    type assignable = [
        | `Plus
        | `Minus
        | `Times
        | `Pow
        | `Div
        | `IntDiv
        | `Mod
        | `IsMod
        | `BitAnd
        | `BitOr
        | `BitXor
        | `Shl
        | `Shr
    ] [@@deriving show]

    type t = [
        assignable
        | `Eq
        | `Ne
        | `Gt
        | `Ge
        | `Lt
        | `Le
        | `And
        | `Or
        | `Xor
        | `Nor
        | `Assign of assignable option
    ] [@@deriving show]
end

type expr = ..

type single_msg = ident

type multi_label =
    | LNamed of ident * expr
    | LPunned of ident
    | LAnon of expr

type multi_msg = multi_label list

type simple_msg = [
    | `Single of single_msg
    | `Multi of multi_msg
]

type obj_msg = [
    simple_msg
    | `Cast of Type.t
]

module Stmt = struct
    module Match_expr = struct
        type expr +=
            | ECapture of {
                loc: loc;
                name: ident;
                t: Type.t option;
                value: expr option
            }
    end

    type loop_end =
        | Loop_to
        | Loop_upto
        | Loop_downto

    type block = t list delims_loc

    and t =
        | SExpr of expr
        | SVar_decl of {
            loc: loc;
            name: ident;
            t: Type.t option;
            value: expr option
        }
        | SIf of {
            loc: loc;
            cond: expr;
            then_blk: block;
            others: (loc * expr * block) list;
            else_blk: (loc * block) option
        }
        | SCase of {
            loc: loc;
            begin_loc: loc;
            cases: (loc * expr * block) list;
            default: (loc * block) option;
            end_loc: loc
        }
        | SMatch of {
            loc: loc;
            value: expr;
            begin_loc: loc;
            cases: (loc * expr list * (loc * expr) option * block) list;
            default: (loc * block) option;
            end_loc: loc
        }
        | SShortMatch of {
            loc: loc;
            value: expr;
            loc2: loc;
            pattern: expr;
            cond: (loc * expr) option;
            then_blk: block;
            else_blk: (loc * block) option
        }
        | SWhile of {
            loc: loc;
            cond: expr;
            block: block
        }
        | SDo_while of {
            loc: loc;
            block: block;
            loc2: loc;
            cond: expr;
        }
        | SFor_in of {
            loc: loc;
            var: ident * Type.t option;
            var2: (ident * Type.t option) option;
            in': loc * expr;
            while': (loc * expr) option;
            block: block
        }
        | SFor_range of {
            loc: loc;
            var: ident * Type.t option;
            from: loc * expr;
            to': loc * loop_end * expr;
            by: (loc * expr) option;
            while': (loc * expr) option;
            block: block
        }
        | SDo of loc * block
        | SReturn of loc * expr option
        | SBreak of loc * (loc * int) option
        | SNext of loc * (loc * int) option
        | SThrow of loc * expr
        | STry of {
            loc: loc;
            block: block;
            cases: (loc * ident * Type.t * block) list
        }
end

module Cascade = struct
    type k =
        | Message of simple_msg
        | Assign of ident * loc * expr
        | AssignMsg of simple_msg * loc * expr
        | Block of Stmt.block
    
    type t = {
        loc: loc;
        depth: int;
        kind_loc: loc;
        kind: k;
        nested: t list
    }
end

type expr +=
    | EName of ident
    | EType of Type.t
    | ELitsym of loc * string
    
    | ETag of ident * expr

    | EInt of loc * int
    | EDec of loc * float
    | EChar of loc * char
    | EStr of loc * string (* add interpolation later *)
    | EBool of loc * bool
    | EArray of loc * expr list
    | EHash of loc * (expr * expr) list
    | ETuple of loc * expr list
    | EThis of loc
    | EWildcard of loc
    | EFunc of {
        loc: loc;
        params: (ident * Type.t option) list;
        return: Type.t option;
        body: Stmt.t list
    }
    | EAnon_arg of {loc: loc; depth: int; index: int}
    
    | EParen of loc * expr list
    | EBlock of Stmt.block

    | EType_msg of loc * Type.t * simple_msg
    | EType_cascade of Type.t * Cascade.t list

    | EObj_msg of expr * obj_msg
    | EObj_cascade of expr * Cascade.t list

    | EMember of expr * string

    | EPrefix of loc * Prefix.t * expr
    | EPostfix of expr * loc * Postfix.t
    | EInfix of expr * loc * Infix.t * expr


module Decls = struct
    module Generic_rule = struct
        type k =
            | Eq of Type.t * loc * Type.t
            | Ne of Type.t * loc * Type.t
            | Of of Type.t * loc * Type.t
            | And of t * loc * t
            | Or of t * loc * t
            | Xor of t * loc * t
            | Nor of t * loc * t
            | Not of loc * t
            | Paren of t delims_loc
        
        and t = loc * k
    end

    type generic_param = {
        loc: loc;
        name: ident;
        params: Type.t list delims_loc option;
        parents: (loc * Type.t list) option;
        rule: (loc * Generic_rule.t) option
    }


    module Member = struct
        module Attr = struct
            type k = [
                | `Is_static
                | `Is_hidden
                | `Is_readonly
                | `Is_getter
                | `Is_setter
                | `Is_noinherit
            ]
            
            type t = loc * loc * k
        end

        type t = {
            loc: loc;
            name: ident;
            t: Type.t option;
            attrs: Attr.t list;
            value: (loc * expr) option
        }
    end
    

    module Module_attr = struct
        type k = [
            | `Is_hidden
            | `Is_main
            | `Is_native of loc * string
        ]

        type t = loc * loc * k
    end

    module Class_attr = struct
        type k = [
            | `Is_hidden
            | `Is_friend of types_spec
            | `Is_native of (ident * expr) list delims_loc
        ]

        type t = loc * loc * k
    end

    module Protocol_attr = struct
        type k = [
            | `Is_hidden
            | `Is_friend of types_spec
        ]

        type t = loc * loc * k
    end

    module Kind_attr = struct
        type k = [
            | `Is_hidden
            | `Is_friend of types_spec
            | `Is_flags
        ]

        type t = loc * loc * k
    end

    module Alias_attr = struct
        type k = [
            | `Is_hidden
        ]

        type t = loc * loc * k
    end


    module Case = struct
        type tag = [
            | `Single of ident
            | `Multi of (ident option * ident option * Type.t) list
        ]

        type k =
            | Named of ident * (loc * expr) option
            | Tagged of tag delims_loc
        
        type t = {
            loc: loc;
            kind: k;
            init: Stmt.block option
        }
    end


    module Alias = struct
        type k =
            | Opaque
            | Direct of loc * Type.t
            | Strong of Type.t
    end

    module Use = struct
        type k =
            | Import of {spec: types_spec; from: (loc * Type.t) option}
            | Pragma of loc * string
    end


    module Method = struct
        module Attr = struct
            type k = [
                | `Is_static
                | `Is_hidden
                | `Is_main
                | `Is_getter
                | `Is_setter
                | `Is_noinherit
                | `Is_unordered
                | `Is_native of (loc * string) option
            ]

            type t = loc * loc * k
        end

        type k = [
            | `Single of ident
            | `Multi of (ident option * ident option * Type.t) list
            | `Cast of Type.t
        ]

        type t = {
            generics: generic_param list;
            loc: loc;
            spec: k delims_loc;
            return: Type.t option;
            attrs: Attr.t list;
            body: Stmt.block option
        }
    end

    module Init = struct
        module Attr = struct
            type k = [
                | `Is_hidden
                | `Is_noinherit
                | `Is_unordered
                | `Is_native of (loc * string) option
            ]

            type t = loc * loc * k
        end

        type k = [
            | `Single of ident
            | `Multi of (ident option * ident option * Type.t) list
        ]

        type t = {
            generics: generic_param list;
            loc: loc;
            spec: k delims_loc;
            attrs: Attr.t list;
            body: Stmt.block option
        }
    end

    module Operator = struct
        module Attr = struct
            type k = [
                | `Is_hidden
                | `Is_noinherit
                | `Is_native of (loc * string) option
            ]

            type t = loc * loc * k
        end

        type t = {
            generics: generic_param list;
            loc: loc;
            symbol: loc * string;
            spec: (ident * Type.t) delims_loc option;
            return: Type.t option;
            attrs: Attr.t list;
            body: Stmt.block option
        }
    end

    module Base_method = struct
        module Attr = struct
            type k = [
                | `Is_static
            ]
            
            type t = loc * loc * k
        end

        type t = {
            loc: loc;
            attrs: Attr.t;
            body: Stmt.block
        }
    end


    [@@@warning "-30"]
    
    type module_decl = {
        loc: loc;
        name: ident;
        attrs: Module_attr.t list;
        body: [
            | `Module of module_decl
            | `Class of class_decl
            | `Protocol of protocol_decl
            | `Kind of kind_decl
            | `Alias of alias_decl
            | `Member of Member.t
            | `Method of Method.t
            | `Base_init of Base_method.t
            | `Deinit of Base_method.t
        ] list delims_loc
    }

    and class_decl = {
        generics: generic_param list;
        loc: loc;
        name: ident;
        params: Type.t list delims_loc option;
        parents: (loc * Type.t list) option;
        attrs: Class_attr.t list;
        body: [
            | `Module of module_decl
            | `Class of class_decl
            | `Protocol of protocol_decl
            | `Kind of kind_decl
            | `Alias of alias_decl
            | `Member of Member.t
            | `Method of Method.t
            | `Init of Init.t
            | `Base_init of Base_method.t
            | `Operator of Operator.t
            | `Deinit of Base_method.t
        ] list delims_loc
    }

    and protocol_decl = {
        generics: generic_param list;
        loc: loc;
        name: ident;
        params: Type.t list delims_loc option;
        parents: (loc * Type.t list) option;
        attrs: Protocol_attr.t list;
        body: [
            | `Module of module_decl
            | `Class of class_decl
            | `Protocol of protocol_decl
            | `Kind of kind_decl
            | `Alias of alias_decl
            | `Member of Member.t
            | `Method of Method.t
            | `Operator of Operator.t
        ] list delims_loc
    }

    and kind_decl = {
        generics: generic_param list;
        loc: loc;
        name: ident;
        params: Type.t list delims_loc option;
        parent: Type.t option;
        parents: (loc * Type.t list) option;
        attrs: Kind_attr.t list;
        body: [
            | `Module of module_decl
            | `Class of class_decl
            | `Protocol of protocol_decl
            | `Kind of kind_decl
            | `Alias of alias_decl
            | `Member of Member.t
            | `Case of Case.t
            | `Method of Method.t
            | `Base_init of Base_method.t
            | `Operator of Operator.t
            | `Deinit of Base_method.t
        ] list delims_loc
    }

    and alias_decl = {
        generics: generic_param list;
        loc: loc;
        name: ident;
        kind: Alias.k;
        attrs: Alias_attr.t list
    }


    type use_decl = {
        generics: generic_param list;
        loc: loc;
        kind: Use.k
    }
end

module Program = struct
    open Decls

    type modular = [
        | `Module of module_decl
        | `Class of class_decl
        | `Protocol of protocol_decl
        | `Kind of kind_decl
        | `Alias of alias_decl
        | `Use of use_decl
    ]

    type script = [
        modular
        | `Method of Method.t
        | `Statement of Stmt.t
    ]

    type t =
        | Modular of modular list
        | Script of script list
end


let rec pp_expr fmt e =
    (*let open Stmt.Match_expr in*)
    let open Caml.Format in

    let str = fprintf fmt "%s %s" in

    let module Anon_arg = struct
        type t = {loc: loc; depth: int; index:int} [@@deriving show]
    end in

    match e with
    | EName id -> str "EName" @@ show_ident id
    | EType t -> str "EType" @@ Type.show t
    | ELitsym(l, v) -> str "ELitsym" @@ [%show: loc * string] (l, v);
    | EInt(l, v) -> str "EInt" @@ [%show: loc * int] (l, v)
    | EDec(l, v) -> str "EDec" @@ [%show: loc * float] (l, v)
    | EChar(l, v) -> str "EChar" @@ [%show: loc * char] (l, v)
    | EStr(l, v) -> str "EStr" @@ [%show: loc * string] (l, v)
    | EBool(l, v) -> str "EBool" @@ [%show: loc * bool] (l, v)
    | EArray(l, v) -> str "EArray" @@ [%show: loc * expr list] (l, v)
    | EHash(l, v) -> str "EHash" @@ [%show: loc * (expr * expr) list] (l, v)
    | ETuple(l, v) -> str "ETuple" @@ [%show: loc * expr list] (l, v)
    | EThis l -> str "EThis" @@ show_loc l
    | EWildcard l -> str "EWildcard" @@ show_loc l
    | EAnon_arg {loc; depth; index} -> let open Anon_arg in str "EAnon_arg" @@ [%show: t] {loc; depth; index} (* hack *)
    | _ -> ()

and pp_multi_label fmt l =
    let open Caml.Format in
    match l with
    | LNamed(i, e) ->
        pp_print_text fmt "LNamed (";
        pp_ident fmt i;
        pp_print_text fmt ", ";
        pp_expr fmt e;
        pp_print_string fmt ")"
    | LPunned i ->
        pp_print_text fmt "LPunned ";
        pp_ident fmt i
    | LAnon e ->
        pp_print_text fmt "LAnon ";
        pp_expr fmt e