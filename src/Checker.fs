module Checker

open AbSyn
open SymTab


// Check starts and ends conditions
type VarTable = SymTab.SymTab<Value>
type usedTable =  SymTab.SymTab<bool>






let rec initProcedures( p: UntypedProcDec List,
                        usedTab : usedTable) =
    match p with
        | [] -> SymTab.empty()
        | ( p :: ps) ->
            let (name, statList, pos) = (getProcName p , getProcStat p, getProcPos p)




let rec initStatList (s:UntypedStatement List,
                      usedTab : usedTable) =
    match s with
        | [] -> SymTab.empty()
        | s :: ss ->
and initExp ( e : UntypedExp)

and initExp ( e : UntypedExp)
and initStatement (s: UntypedStatemnt,
                and initExp ( e : UntypedExp)   usedTab : usedTable) =
    match s with
and initExp ( e : UntypedExp)        |

and initExp ( e : UntypedExp)

and initExp ( e : UntypedExp)

and and initExp ( e : UntypedExp,
                  usedTab : usedTable) =

  | Constant (v,_)  -> v
  | Var(id, pos)    ->
  | Plus(e1, e2, pos)  ->
        let res1   = initExp(e1, usedTab  )
        let res2   = initExp(e2, usedTab  )

  | Minus(e1, e2, pos)  ->
        let res1   = initExp(e1, usedTab  )
        let res2   = initExp(e2, usedTab  )

  | Times(e1, e2, pos)  ->
        let res1   = initExp(e1, usedTab  )
        let res2   = initExp(e2, usedTab  )

  | Divide(e1, e2, pos)  ->
        let res1   = initExp(e1, usedTab  )
        let res2   = initExp(e2, usedTab  )
  | Modulo(e1, e2, pos)  ->
        let res1   = initExp(e1, usedTab  )
        let res2   = initExp(e2, usedTab  )

  | Equal(e1, e2, pos)  ->
        let r1 = initExp(e1, usedTab  )
        let r2 = initExp(e2, usedTab  )

  | Less(e1, e2, pos)  ->
        let r1 = initExp(e1, usedTab  )
        let r2 = initExp(e2, usedTab  )








let initializeProg(prog: UntypedProg) : VarTable =
    let statements = fst prog
    let procDecs = snd prog
    let vartabStat = initStatList statements
