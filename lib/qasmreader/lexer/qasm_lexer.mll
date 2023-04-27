(* 
 * Documentation:  https://v2.ocaml.org/manual/lexyacc.html#s%3Alexyacc-example
 *)
{   
    open Tokens
    exception LexError of string
    (* It is faster to treat keywords first as identifiers and when a token matching the rule for identifier is found,
    then to check in a hashtable whether it is a keyword, and which one it is. The initial size of the Hashtable is
    meaningless. *)
    let kwd_table = Hashtbl.create 31
    let _ = List.iter (fun (kwd, tkn) -> Hashtbl.add kwd_table kwd tkn)
        [
            "opaque", OPAQUE;
            "qreg", QREG;
            "creg", CREG;
            "gate", GATE;
            "if", IF;
            "sin", SIN;
            "cos", COS;
            "tan", TAN;
            "exp", EXP;
            "ln", LN;
            "sqrt", SQRT;
            "pi", PI;
            "U", U;
            "CX", CX;
            "measure", MEASURE;
            "reset", RESET;
            "barrier", BARRIER
        ]
    
    (* The automaton generated from this lex specification becomes smaller if all punctuators are matched with one rule
    and one looks up the specific punctuator in this hashtable. *)
    let op_table = Hashtbl.create 31
    let _ = List.iter (fun (pct, tkn) -> Hashtbl.add op_table pct tkn)
        [
            "[", LBRACKET;
            "]", RBRACKET;
            "(", LPAREN;
            ")", RPAREN;
            "{", LBRACE;
            "}", RBRACE;
            "->", ARROW;
            "*", STAR;
            "+", PLUS;
            "-", MINUS;
            "/", SLASH;
            "==", EQ_EQ;
            "^", CIRC;
            ";", SEMICOLON;
            ",", COMMA
    ]
}

(* Regular expressions used in the specification of the lexer. *)
let identifier = ['a'-'z' 'A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let punctuator = "[" | "]" | "(" | ")" | "{" | "}" | "->" | "*" | "+" | "-" | "/" | "==" | "!=" | "^" | ";" | ","
let floating_constant = (['0'-'9']+ '.' ['0'-'9']* | ['0'-'9']* '.' ['0'-'9']+) (('e' | 'E') ('-' | '+')? ['0'-'9']+)?
let integer_constant = ['1'-'9'] ['0'-'9']* | '0'

(* The first rule is the entry point of the lexer. The [parse] keyword states that the regular expressions matches that
covers the longest substring of the input stream. If two cases cover the same length, the first one is prioritized. *)
rule token = parse
    | "OPENQASM 2.0;"
        { OPENQASM (2, 0) }  (* Right now this lexer only accepts OpenQASM 2.0 *)
    | [' ' '\t']
        { token lexbuf }  (* skip blanks *)
    | '\n'
        { token lexbuf }  (* skip newline character 
        In case we want to add error messages, we would increment a line counter here, 
        for this, see also the project of the practical course *)
    | "//"
        { lineComment lexbuf }  (* skip line comments *)
    | "/*"
        { comment lexbuf }  (* skip block comments *)
    | identifier as id
    (* 
     * see https://v2.ocaml.org/manual/lexyacc.html#s:lexyacc-common-errors 
     * why we treat keywords and identifiers together
     *)
        {
            try
                Hashtbl.find kwd_table id
            with Not_found ->
                IDENTIFIER id
        }
    | integer_constant as const
        { 
            let i = 
                (* we parse to float because there can occur very large intergers that cannot be represented as integers
                 * anymore *)
                try float_of_string const 
                with Failure _ -> raise ( LexError (const ^ " cannot be parsed to float") )
            in 
            INT_CONST i
        }  (* parse int constants and transform them to int *)
    | floating_constant as const
        {
            let f =
                try float_of_string const
                with Failure _ -> raise ( LexError (const ^ " cannot be parsed to float") )
            in
            FLOAT_CONST f
        }  (* parse float constants and transform them to float *)
    | punctuator as pct 
        { Hashtbl.find op_table pct }  (* parse punctuator, look up the specific punctuator in the hashtable *)
    | eof
        { EOF }
and lineComment = parse
    | '\n' 
        { token lexbuf }  (* go to the "token" rule *)
    | _
        { lineComment lexbuf }  (* skip comment *)
and comment = parse
    | "*/" 
        { token lexbuf }  (* go to the "token" rule *)
    | _
        { comment lexbuf }  (* skip comment *)
{   

}