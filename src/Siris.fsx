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

// Parse program from string.
let parseString (s : string) : AbSyn.Program =
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

let reverseProg(str:string) =
    let pgm = parseString str
    ReverseProg.inverseProgram(pgm)

// Print error message to the standard error channel.
let errorMessage (message : string) : Unit =
    printfn "%s\n" message

let errorMessage' (errorType : string, message : string, line : int, col : int) =
    printfn "%s: %s at line %d, column %d" errorType message line col

let errorMessageList (errorType :string, message: string, list) =
    printfn "%s: %s: " errorType message
    List.map (fun (s,i) -> printfn "Variable %s has value %A" s i) list |> ignore


let errorMessageList'(errorType:string, message:string, list) =
    printfn "%s: %s: " errorType message
    List.map (fun (s,(l,c)) -> printfn "Declaration of %s at line %d, column %d is unmatched" s l c) list |> ignore

let bad () : int =
    errorMessage "Unknown command-line arguments. Usage:\n"
    0

[<EntryPoint>]
let main(paramList: string[]) : int =
  try
  match paramList with
    | [|"-f"; file|]       ->
        Interpreter.evalProg (parseFromFile file)

    | [|"-r"; file|]       ->
        Interpreter.evalProg(ReverseProg.inverseProgram (parseFromFile file))
    | _                    -> bad()

  with
    | SyntaxError (line, col) ->
        errorMessage' ("Parse error", "Error", line, col)
        System.Environment.Exit 1
        1
    | Lexer.LexicalError (message, (line, col)) ->
        errorMessage' ("Lexical error", message, line, col)
        System.Environment.Exit 1
        1
    | StaticChecker.TerminationException (message, list) ->
        errorMessageList ("Termination error", message, list)
        System.Environment.Exit 1
        1
    | StaticChecker.StaticListException (message, list) ->
        errorMessageList'("Static error", message, list)
        System.Environment.Exit 1
        1
    | StaticChecker.StaticException(message, (line, col)) ->
        errorMessage' ("Static error", message, line, col)
        System.Environment.Exit 1
        1
    | Interpreter.InterpreterErr(message, (line, col)) ->
        errorMessage' ("Run time error", message, line, col)
        System.Environment.Exit 1
        1
    | FileProblem filename ->
        errorMessage ("There was a problem with the file: " + filename)
        System.Environment.Exit 1
        1
