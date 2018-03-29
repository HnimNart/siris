module Checker

open AbSyn

// Set containing variable names
type VarSet = Set<string>

let rec initProcedures(procedureList: UntypedProcDec List,
                        varSet : VarSet) : VarSet =
    match procedureList with
        | [] -> varSet
        | (p :: ps) ->
            let (name, statList, pos) = (getProcName p , getProcStat p, getProcPos p)
            let varSet' = initStatList(statList, varSet)
            initProcedures(ps, varSet')

and initStatList(s: UntypedStatement List,
                  varSet : VarSet) : VarSet =
  match s with
    | [] -> varSet
    | s :: ss ->
        let varSet' = initStatement(s, varSet)
        initStatList(ss, varSet')

and initStatement (s: UntypedStatement,
                   varSet: VarSet): VarSet =
  match s with
  | PlusAssignment(id, e, pos)  ->
      let varSetExp = initExp(e, varSet)
      let varSet' = Set.add id varSet
      Set.union varSetExp varSet'

  | MinusAssignment(id, e, pos )  ->
      let varSetExp = initExp(e, varSet)
      let varSet' = Set.add id varSet
      Set.union varSetExp varSet'

  | If(e1, s1, s2, e2, pos)  ->
      let varSetExp = initExp(e1, varSet)
      let varSetExp2 = initExp(e2, varSetExp)
      let varSetStat1 = initStatList(s1, varSetExp2)
      initStatList(s2, varSetStat1)

  | Repeat(s1, e1, pos)  ->
      let varSetExp = initExp(e1, varSet)
      initStatList(s1, varSetExp)

  | Call (id, pos)  -> // No need to do anything here
      varSet
  | Uncall (id, pos)  -> // Same
      varSet

  | Print(var, pos) ->
      Set.add var varSet

  | Read(var, pos) ->
      Set.add var varSet

and initExp (e : UntypedExp,
             varSet : VarSet) : VarSet =
   match e with
     | Constant(v,_)  -> varSet
     | Var(id, pos)    ->
         Set.add id varSet

     | Plus(e1, e2, pos)  ->
           let varSet1 = initExp(e1, varSet)
           let varSet2 = initExp(e2, varSet)
           Set.union varSet1 varSet2

     | Minus(e1, e2, pos)  ->
           let varSet1 = initExp(e1, varSet)
           let varSet2 = initExp(e2, varSet)
           Set.union varSet1 varSet2

     | Times(e1, e2, pos)  ->
           let varSet1 = initExp(e1, varSet)
           let varSet2 = initExp(e2, varSet)
           Set.union varSet1 varSet2

     | Divide(e1, e2, pos)  ->
           let varSet1 = initExp(e1, varSet)
           let varSet2 = initExp(e2, varSet)
           Set.union varSet1 varSet2

     | Modulo(e1, e2, pos)  ->
           let varSet1 = initExp(e1, varSet)
           let varSet2 = initExp(e2, varSet)
           Set.union varSet1 varSet2

     | Equal(e1, e2, pos)  ->
           let varSet1 = initExp(e1, varSet)
           let varSet2 = initExp(e2, varSet)
           Set.union varSet1 varSet2

     | Less(e1, e2, pos)  ->
         let varSet1 = initExp(e1, varSet)
         let varSet2 = initExp(e2, varSet)
         Set.union varSet1 varSet2


let initializeProg(prog: UntypedProg) : VarSet =
    let statements = fst prog
    let procDecs = snd prog
    let statVarSet = initStatList(statements ,Set.empty)
    let procVarSet = initProcedures(procDecs, Set.empty)
    Set.union statVarSet procVarSet


let checkVarIsNonZero(x, vtab) =
    match SymTab.lookup x vtab with
        | None -> failwithf "Couldn't find %s in symbol table" x
        | Some v ->
            match v with
                | IntVal v1 ->
                    if v1 = 0
                    then
                        0
                    else
                        printfn "Variable %s is not zero a termination" x
                        1

let checkNonZeroVar(vTab, vSet): int =
    Set.fold (fun acc x -> acc + checkVarIsNonZero (x ,vTab)) 0 vSet
