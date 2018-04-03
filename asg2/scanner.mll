(* Matthew Tan
* mxtan
* cs112
* asg2: scanner.mll
*)

{

module Scanner = struct
    include Bigint

    type token = Number   of Bigint.bigint
               | Regoper  of char * int
               | Operator of char

    let bigstr = Bigint.bigint_of_string
    let lexeme = Lexing.lexeme
    let ord    = int_of_char
    let strlen = String.length

    let regoper lexbuf =
        let token = lexeme lexbuf
        in  Regoper (token.[0], ord token.[1])

}

let number  = '_'? ['0' - '9']*
let regoper = ['s' 'l']

rule scanner = parse
   | number    { Number (bigstr (lexeme lexbuf)) }
   | regoper _ { regoper lexbuf }
   | _         { Operator (lexeme lexbuf).[0] }
   | eof       { raise End_of_file }

{

end

}
