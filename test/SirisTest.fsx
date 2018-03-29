
open System.Text
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open System.IO

open AbSyn
open ReverseProg

(* Both lex and parse a program. *)
let parse (s : string) =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)


// Parse program from string.
let parseString (s : string) : AbSyn.UntypedProg =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)

let interpretSimple (str : string) : int =
    let pgm = parseString str
    Interpreter.evalProg pgm

let reverseProg(str:string) =
    let pgm = parseString str
    ReverseProg.inverseProgram(pgm)

(* Continually evaluate user expressions. *)
let loop () =
    let mutable running = true
    while running do
        printf "Input a program: "
        try
            let program = System.Console.ReadLine()
            if program = "exit"
            then
                running <- false
            else
                printfn "Parse result: %A" (Interpreter.evalProg (parse program))
                printfn "Syntax tree %A" (parse program)
                printfn "Reverse result: %A" (Interpreter.evalProg (reverseProg program))
                printfn "Reverse Syntax tree %A" (ReverseProg.inverseProgram (parse program))
        with
        | Failure msg -> printfn "%s" msg
        | :? System.ArgumentNullException ->
            running <- false

loop ()
