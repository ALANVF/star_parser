open Base

type loc = Lexing.position

type 't delims_loc = loc * 't * loc

type ident = loc * string

module Type: sig
    type segment =
        | Wildcard 
        | Name of string

    type param = {loc: loc; t: t'}

    and t' = (segment * param list) list

    and t = loc * t'
end

type types_spec = [
    | `One of Type.t
    | `Many of Type.t list delims_loc
]

module Prefix: sig
    type t =
        | Incr
        | Decr
        | Neg
        | Not
        | Compl
end

module Postfix: sig
    type t =
        | Incr
        | Decr
        | Truthy
end

module Infix: sig
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
    ]

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
    ]
end

type expr = ..

type single_msg = ident

type cast_msg = Type.t

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
    | `Cast of cast_msg
]

module Stmt: sig
    module Match_expr: sig
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

module Cascade: sig
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


module Decls: sig
    module Generic_rule: sig
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


    module Member: sig
        module Attr: sig
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


    module Module_attr: sig
        type k = [
            | `Is_hidden
            | `Is_main
            | `Is_native of loc * string
        ]

        type t = loc * loc * k
    end

    module Class_attr: sig
        type k = [
            | `Is_hidden
            | `Is_friend of types_spec
            | `Is_native of (ident * expr) list delims_loc
        ]
        
        type t = loc * loc * k
    end

    module Protocol_attr: sig
        type k = [
            | `Is_hidden
            | `Is_friend of types_spec
        ]
        
        type t = loc * loc * k
    end

    module Kind_attr: sig
        type k = [
            | `Is_hidden
            | `Is_friend of types_spec
            | `Is_flags
        ]
        
        type t = loc * loc * k
    end

    module Alias_attr: sig
        type k = [
            | `Is_hidden
        ]
        
        type t = loc * loc * k
    end


    module Case: sig
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


    module Alias: sig
        type k =
            | Opaque
            | Direct of loc * Type.t
            | Strong of Type.t
    end


    module Method: sig
        module Attr: sig
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

    module Init: sig
        module Attr: sig
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

    module Operator: sig
        module Attr: sig
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

    module Base_method: sig
        module Attr: sig
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
        spec: types_spec;
        from: (loc * Type.t) option
    }
end