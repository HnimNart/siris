
open System.Text
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open System.IO

open AbSyn

exception SyntaxError of int * int
exception FileProblem of string

let printPos (errString : string) : unit =
    let rec state3 (s : string) (p : int) (lin : string) (col : int) =
        (* read digits until not *)
        let c = s.[p]
        if System.Char.IsDigit c
        then state3 s (p-1) (System.Char.ToString c + lin) col
        else raise (SyntaxError (System.Int32.Parse lin, col))

    let rec state2 (s : string) (p : int) (col : string) =
        (* skip from position until digit *)
        let c = s.[p]
        if System.Char.IsDigit c
        then state3 s (p-1) (System.Char.ToString c) (System.Int32.Parse col)
        else state2 s (p-1) col

    let rec state1 (s : string) (p : int) (col : string) =
        (* read digits until not *)
        let c = s.[p]
        if System.Char.IsDigit c
        then state1 s (p-1) (System.Char.ToString c + col)
        else state2 s (p-1) col

    let rec state0 (s : string) (p : int) =
        (* skip from end until digit *)
        let c = s.[p]
        if System.Char.IsDigit c
        then state1 s (p-1) (System.Char.ToString c)
        else state0 s (p-1)

    state0 errString (String.length errString - 1)

(* Both lex and parse a program. *)
let parse (s : string) =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)


// Parse program from string.
let parseString (s : string) : AbSyn.UntypedProg =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)


let parseFromFile (filename : string)  =
  let txt = try  // read text from file given as parameter with added extension
              let inStream = File.OpenText (filename)
              let txt = inStream.ReadToEnd()
              inStream.Close()
              txt
            with  // or return empty string
              | ex -> ""
  if txt <> "" then // valid file content
    let program =
      try
        parseString txt
      with
        | Lexer.LexicalError (info,(line,col)) ->
            printfn "%s at line %d, position %d\n" info line col
            System.Environment.Exit 1
            ([],[],[])
        | ex ->
            if ex.Message = "parse error"
            then printPos Parser.ErrorContextDescriptor
            else printfn "%s" ex.Message
            System.Environment.Exit 1
            ([],[],[])
    program
  else failwith "Invalid file name or empty file"


let interpretSimple (str : string) : int =
    let pgm = parseString str
    Interpreter.evalProg pgm

let reverseProg(str:string) =
    let pgm = parseString str
    ReverseProg.inverseProgram(pgm)

let interpret (filename : string) : Unit =
    let pgm = parseFromFile filename
    let res = Interpreter.evalProg pgm
    ()


let infLoop(direction) =
    let mutable running = true
    while running do
        printf "Input a program: "
        try
            let program = System.Console.ReadLine()
            if program = "exit"
            then
                running <- false
            else
                printfn "Parse result: %A" (direction (parse program))
                printfn "Syntax tree %A" (parse program)
                printfn "Reverse Syntax tree %A" (ReverseProg.inverseProgram (parse program))
                printfn "Reverse result: %A" (Interpreter.evalProg (reverseProg program))
        with
        | Failure msg -> printfn "%s" msg
        | :? System.ArgumentNullException ->
            running <- false


// Print error message to the standard error channel.
let errorMessage (message : string) : Unit =
    printfn "%s\n" message

let errorMessage' (errorType : string, message : string, line : int, col : int) =
    printfn "%s: %s at line %d, column %d" errorType message line col

let bad () : int =
    errorMessage "Unknown command-line arguments. Usage:\n"
    0



[<EntryPoint>]
let main(paramList: string[]) : int =
  try
  let retval =
    match paramList with
      | [|"-f"; file|]       ->
          Interpreter.evalProg (parseFromFile file)
      | [|"-r"; file|]       ->
          Interpreter.evalProg(ReverseProg.inverseProgram (parseFromFile file))

          // printfn "REVERSE TREE: %A" (ReverseProg.inverseProgram (parseFromFile file))
          // printfn "NORMAL TREE: %A" ((parseFromFile file))
      | [|"-fr"; file|]      ->
          Interpreter.evalProg (parseFromFile file) |> ignore
          Interpreter.evalProg (ReverseProg.inverseProgram (parseFromFile file))
      | [|"-fa"; file|]      ->
          Interpreter.evalProg (parseFromFile file)
      | [|"-ra"; file|]      ->
          Interpreter.evalProg (ReverseProg.inverseProgram (parseFromFile file))
      | [|"-i"|]             ->
          infLoop(Interpreter.evalProg)
          0
      | _                    -> 1

  retval
  with
    | SyntaxError (line, col) ->
        errorMessage' ("Parse error", "Error", line, col)
        System.Environment.Exit 1
        1
    | Lexer.LexicalError (message, (line, col)) ->
        errorMessage' ("Lexical error", message, line, col)
        System.Environment.Exit 1
        1
    | Interpreter.MyError (message, (line, col)) ->
        errorMessage' ("Interpreter error", message, line, col)
        System.Environment.Exit 1
        1
    | FileProblem filename ->
        errorMessage ("There was a problem with the file: " + filename)
        System.Environment.Exit 1
        1
