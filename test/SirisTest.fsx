
open System.Text
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open System.IO

open AbSyn
(* Both lex and parse a program. *)
let parse (s : string) =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)



// Parse program from string.
let parseString (s : string) : AbSyn.UntypedProg =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)


let interpretSimple (str : string) : AbSyn.Value =
    let pgm = parseString str
    Interpreter.evalProg pgm




(* Continually evaluate user expressions. *)
let loop () =
    let mutable running = true
    while running do
        printf "Input an expression: "
        try
            let program = System.Console.ReadLine()
            if program = "exit"
            then
                running <- false
            else
                printfn "Parse result: %A" (interpretSimple program)
                printfn "Syntax tree %A" (parse program)
        with
        | Failure msg -> printfn "%s" msg
        | :? System.ArgumentNullException ->
            running <- false

loop ()
