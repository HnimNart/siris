module ReverseProg

open AbSyn


// Reverses a list
// Code from https://gist.github.com/thednaz/2897337
let rec rev list acc=
    match list with
    | []  -> acc
    | [x]  -> x::acc
    | head::tail  -> rev tail (head::acc)


let rec inverseStatementList(statList : Statement List ):
                             Statement List =
    match statList with
        | [] -> []
        | ( s :: ss ) ->
            inverseStatement(s) :: inverseStatementList ss

and inverseProcedures(p: ProcDec) :
                     ProcDec =
    let (name, param ,statList, pos) = (getProcName p, getProcParam p , getProcStat p, getProcPos p)
    ProcDec(name, param, rev (inverseStatementList(statList)) [], pos)


and invProcs(p: ProcDec List) : ProcDec List =
    match p with
        | [] -> []
        | p' :: ps ->  inverseProcedures(p') :: invProcs(ps)

and inverseStatement(s: Statement) =
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
        let s1' = rev (inverseStatementList s1) []
        Repeat(s1', e1, pos)
    | Call (id, param, pos) ->
        Uncall(id, param, pos)
    | Uncall(id, param, pos) ->
        Call(id, param, pos)
    | Print(var, pos) ->
        Read(var, pos)
    | Read(var, pos) ->
        Print(var, pos)
    | Local(var, e, pos) ->
        Delocal(var, e, pos)
    | Delocal(var, e,pos) ->
        Local(var,e, pos)
    | Swap(v1,v2, pos) ->
        Swap(v1, v2, pos)

and inverseProgram(p : Program) : Program =
   let (dec, statements, proc) = p
   let reverseStatements = rev (inverseStatementList statements)  []
   // let p' = invProcs(proc)
   (dec, reverseStatements, proc)
