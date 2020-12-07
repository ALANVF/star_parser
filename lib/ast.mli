open Base

type loc = Lexing.position [@@deriving show]

type 't delims_loc = loc * 't * loc [@@deriving show]

type ident = loc * string [@@deriving show]

module Type: sig
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

module Prefix: sig
    type t =
        | Incr
        | Decr
        | Neg
        | Not
        | Compl
    [@@deriving show]
end

module Postfix: sig
    type t =
        | Incr
        | Decr
        | Truthy
    [@@deriving show]
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
        | `And
        | `Or
        | `Xor
        | `Nor
    ] [@@deriving show]

    type t = [
        assignable
        | `Eq
        | `Ne
        | `Gt
        | `Ge
        | `Lt
        | `Le
        | `Assign of assignable option
    ] [@@deriving show]
end


module rec Label: sig
    type multi =
        | Named of ident * Expr.t
        | Punned of ident
        | Anon of Expr.t
    [@@deriving show]
end

and Message: sig
    type multi = Label.multi list [@@deriving show]

    type simple = [
        | `Single of ident
        | `Multi of multi
    ] [@@deriving show]

    type obj = [
        simple
        | `Cast of Type.t
    ] [@@deriving show]
end

and Cascade: sig
    type k =
        | Message of Message.simple
        | Assign of ident * loc * Expr.t
        | AssignMsg of Message.simple * loc * Expr.t
        | Block of Stmt.block
    [@@deriving show]

    type t = {
        loc: loc;
        depth: int;
        kind_loc: loc;
        kind: k;
        nested: t list
    } [@@deriving show]
end

and Expr: sig
    type t =
        | Name of ident
        | Type of Type.t
        | Litsym of loc * string
        
        | Tag of ident * t

        | Int of loc * int
        | Dec of loc * float
        | Char of loc * char
        | Str of loc * string (* add interpolation later *)
        | Bool of loc * bool
        | Array of t list delims_loc
        | Hash of (t * t) list delims_loc
        | Tuple of t list delims_loc
        | This of loc
        | Wildcard of loc
        | Func of {
            loc: loc;
            params: (ident * Type.t option) list;
            return: Type.t option;
            body: Stmt.t list
        }
        | Anon_arg of {loc: loc; depth: int; index: int}
        
        | Paren of t list delims_loc
        | Block of Stmt.block

        | Type_message of Type.t * Message.simple
        | Type_cascade of Type.t * Cascade.t list

        | Obj_message of t * Message.obj
        | Obj_cascade of t * Cascade.t list

        | Type_member of Type.t * string
        | Obj_member of t * string

        | Prefix of loc * Prefix.t * t
        | Postfix of t * loc * Postfix.t
        | Infix of t * loc * Infix.t * t

        | Capture of {
            loc: loc;
            name: ident;
            t: Type.t option;
            value: t option
        }
    [@@deriving show]

    module Simple: sig
        type t =
            | Name of string
            | Type of Type.t
            | Litsym of string
            
            | Tag of string * t

            | Int of int
            | Dec of float
            | Char of char
            | Str of string
            | Bool of bool
            | Array of t list
            | Hash of (t * t) list
            | Tuple of t list
            | This
            | Wildcard
            | Func of {
                params: (string * Type.t option) list;
                return: Type.t option;
                body: Stmt.t list
            }
            | Anon_arg of {depth: int; index: int}
            
            | Paren of t list
            | Block of Stmt.block

            | Type_message of Type.t * Message.simple
            | Type_cascade of Type.t * Cascade.t list

            | Obj_message of t * Message.obj
            | Obj_cascade of t * Cascade.t list

            | Type_member of Type.t * string
            | Obj_member of t * string

            | Prefix of Prefix.t * t
            | Postfix of t * Postfix.t
            | Infix of t * Infix.t * t

            | Capture of {
                name: ident;
                t: Type.t option;
                value: t option
            }
        [@@deriving show]
    end

    val to_simple: t -> Simple.t
end

and Stmt: sig
    type loop_end =
        | Loop_to
        | Loop_upto
        | Loop_downto
    [@@deriving show]

    type block = t list delims_loc [@@deriving show]

    and t =
        | SExpr of Expr.t
        | SVar_decl of {
            loc: loc;
            name: ident;
            t: Type.t option;
            value: Expr.t option
        }
        | SIf of {
            loc: loc;
            cond: Expr.t;
            then_blk: block;
            others: (loc * Expr.t * block) list;
            else_blk: (loc * block) option
        }
        | SCase of {
            loc: loc;
            begin_loc: loc;
            cases: (loc * Expr.t * block) list;
            default: (loc * block) option;
            end_loc: loc
        }
        | SMatch of {
            loc: loc;
            value: Expr.t;
            begin_loc: loc;
            cases: (loc * Expr.t list * (loc * Expr.t) option * block) list;
            default: (loc * block) option;
            end_loc: loc
        }
        | SShortMatch of {
            loc: loc;
            value: Expr.t;
            loc2: loc;
            pattern: Expr.t;
            cond: (loc * Expr.t) option;
            then_blk: block;
            else_blk: (loc * block) option
        }
        | SWhile of {
            loc: loc;
            cond: Expr.t;
            block: block
        }
        | SDo_while of {
            loc: loc;
            block: block;
            loc2: loc;
            cond: Expr.t;
        }
        | SFor_in of {
            loc: loc;
            var: ident * Type.t option;
            var2: (ident * Type.t option) option;
            in': loc * Expr.t;
            while': (loc * Expr.t) option;
            block: block
        }
        | SFor_range of {
            loc: loc;
            var: ident * Type.t option;
            from: loc * Expr.t;
            to': loc * loop_end * Expr.t;
            by: (loc * Expr.t) option;
            while': (loc * Expr.t) option;
            block: block
        }
        | SDo of loc * block
        | SReturn of loc * Expr.t option
        | SBreak of loc * (loc * int) option
        | SNext of loc * (loc * int) option
        | SThrow of loc * Expr.t
        | STry of {
            loc: loc;
            block: block;
            cases: (loc * ident * Type.t * block) list
        }
    [@@deriving show]
end


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
            value: (loc * Expr.t) option
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
            | `Is_native of (ident * Expr.t) list delims_loc
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
            | Named of ident * (loc * Expr.t) option
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

    module Use: sig
        type k =
            | Import of {spec: types_spec; from: (loc * Type.t) option}
            | Pragma of loc * string
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
        kind: Use.k
    }
end

module Program: sig
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