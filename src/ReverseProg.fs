module ReverseProg

open AbSyn


// Reverses a list
// Code from https://gist.github.com/thednaz/2897337
let rec rev list acc=
    match list with
    | []  -> acc
    | [x]  -> x::acc
    | head::tail  -> rev tail (head::acc)


let rec inverseStatementList(statList : UntypedStatement List ):
                             UntypedStatement List =
    match statList with
        | [] -> []
        | ( s :: ss ) ->
            inverseStatement(s) :: inverseStatementList ss

and inverseProcedures(procedures: UntypedProcDec List) :
                     UntypedProcDec List =
     match procedures with
         | [] -> []
         | p :: ps ->
             let (name,param , decl ,statList, pos) = (getProcName p, getProcParam p , getProcDecl p, getProcStat p, getProcPos p)
             ProcDec(name, param,  decl, inverseStatementList(statList), pos) :: inverseProcedures(ps)

and inverseStatement(s : UntypedStatement) =
    match s with
    | PlusAssignment(id, e, pos) ->
        MinusAssignment(id, e, pos)
    | MinusAssignment(id, e, pos) ->
        PlusAssignment(id, e, pos)
    | If (e1, s1, s2, e2, pos) ->
        let s1' = rev (inverseStatementList s1) []
        let s2' = rev (inverseStatementList s2) []
        If (e2, s1', s2', e1, pos)
    | Repeat(s1, e1, pos) ->
        let s1' = inverseStatementList s1
        Repeat(s1', e1, pos)

    | Call (id, param, pos) ->
        Uncall(id, param, pos)
    | Uncall(id, param, pos) ->
        Call(id, param, pos)
    | Print(var, pos) ->
        Read(var, pos)
    | Read(var, pos) ->
        Print(var, pos)

and inverseProgram(p : UntypedProg) : UntypedProg =
   let (dec, statements, proc) = p
   let reverseStatements = rev (inverseStatementList statements)  []
   (dec, reverseStatements, inverseProcedures proc)
