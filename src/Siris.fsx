
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


let parseFromFile (filename : string) : AbSyn.UntypedProg =
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
            []
        | ex ->
            // if ex.Message = "parse error"
            // then printPos Parser.ErrorContextDescriptor
            // else printfn "%s" ex.Message
            System.Environment.Exit 1
            []
    program
  else failwith "Invalid file name or empty file"

let interpret (filename : string) : Unit =
    let pgm = parseFromFile filename
    let res = Interpreter.evalProg pgm
    ()

// Print error message to the standard error channel.
let errorMessage (message : string) : Unit =
    printfn "%s\n" message

let bad () : Unit =
    errorMessage "Unknown command-line arguments. Usage:\n"


[<EntryPoint>]
let main(paramList: string[]) : int =
  match paramList with
    | [|"-f"; file|]      -> interpret file
    | [|"-r"; file|]      -> interpret file
    | _                   -> bad ()
  0
