module Interpreter

open System
open AbSyn

(* Exceptions for reporting run-time errors. *)
exception InterpreterErr of string * Position

type ProcTable = SymTab.SymTab<ProcDec>
type VarTable = SymTab.SymTab<Value>
type GlobalVars = List<string>
type VarMapping = List<Param*Param>

let mapVarToString(s:string, mappingList:VarMapping) : string =
    let var = List.find (fun (v,v') -> getStringOfParam(v') = s) mappingList
    getStringOfParam (fst var)


let isUniqueParam list = list = List.distinct list


let constZero = IntVal(0)

// Builds procedure symbol table
// This is almost identical to the same function
// from the IPS group project
let rec buildPtab(pdecs: ProcDec list): ProcTable =
  match pdecs with
  | []              -> SymTab.empty()
  | ( pdcl :: ps )  ->
    (* Bind the user-defined procedures, in reverse order. *)
    let pid   = getProcName pdcl
    let pos   = getProcPos pdcl
    let param = List.map (fun x -> (getStringOfParam x)) (getProcParam pdcl)
    if not (isUniqueParam param)  then
        raise(InterpreterErr("Parameters in " + pid + " does not have unique names", pos))
    let ptab  = buildPtab ps
    match SymTab.lookup pid ptab with
      | None         ->
         // Check that every local statement has a corresponding delocal
         StaticChecker.checkStatements(getProcStat pdcl, [], param)
         SymTab.bind pid pdcl ptab

      | Some ofdecl  ->
          (* Report the first occurrence of the name. *)
          raise (InterpreterErr ("Already defined procedure : "+pid, getProcPos ofdecl))

let rec evalStatement(s: Statement,
                      vtab: VarTable,
                      ptab: ProcTable,
                      gSet: GlobalVars,
                      varMap: VarMapping): VarTable =
  match s with
  | PlusAssignment(var, e, pos)  ->
      let eVal = evalExp(e, vtab)
      let value = SymTab.lookup var vtab
      match value with
        | None  ->
            raise (InterpreterErr("LHS variable " + var + " is not defined", pos))
        | Some n  ->
            let newVal = evalExp (Plus(Constant(n, pos), Constant(eVal,pos),pos), vtab )
            SymTab.bind var newVal vtab

  | MinusAssignment(var , e, pos )  ->
      let eVal = evalExp(e, vtab)
      let value = SymTab.lookup var vtab
      match value with
        | None  ->
            raise (InterpreterErr("LHS variable " + var + " is not defined", pos))
        | Some n  ->
            let newVal = evalExp (Minus(Constant(n, pos), Constant(eVal,pos),pos), vtab)
            SymTab.bind var newVal vtab

  | If(e1, s1, s2, e2, pos)  ->
      let (vtab', cond) =
          match evalExp(e1, vtab) with
          | IntVal v1  ->
              if v1 <> 0
              then
                 (evalStatementList(s1, vtab, ptab, gSet, varMap), v1)
              else
                 (evalStatementList(s2, vtab, ptab, gSet, varMap), v1)
      // Check that fi condition is the same
      match evalExp(e2, vtab') with
        | IntVal v2  ->

           if (v2 = 0 && cond = 0) || (v2 <> 0 && cond <> 0 )
           then
               vtab'
           else
               raise (InterpreterErr("fi assertion in second condition does not match.\n" +
                                     "Second condition evaluated to " + (string v2) + "" , pos))

  | Repeat(s1, e1, pos)  ->
      let e1Eval = evalExp(e1, vtab)
      match e1Eval with
        | IntVal v  ->
          if v = 0
          then
              raise (InterpreterErr("Repeat: Condition false at entry", pos))
          else
              repeatEval(s1, e1, vtab, ptab, gSet,varMap)

  | Call (id, param, pos)  ->
      match SymTab.lookup id ptab with
        | None    -> raise( InterpreterErr("Call to undefined procedure: " + id, pos))
        | Some p  -> callProcWithVtable (p, param, vtab, ptab, pos, gSet)

  | Uncall (id, param, pos)  ->
      match SymTab.lookup id ptab with
        | None    -> raise( InterpreterErr("Uncall to undefined procedure: " + id, pos))
        | Some (ProcDec(id, parg, stat, pos))  ->
            let iStat = ReverseProg.inverseStatementList (ReverseProg.rev stat [])
            callProcWithVtable (ProcDec(id, parg, iStat, pos), param , vtab, ptab, pos, gSet)

  | Print(var, pos) ->
      let value =
          match SymTab.lookup var vtab with
              | Some v -> v
              | None -> raise( InterpreterErr("Argument " + var + " in print not defined", pos))

      match value with
        | IntVal( v ) ->
             printf "%i\n" v
             SymTab.bind var constZero vtab

   | Read(var, pos) ->
       match SymTab.lookup var vtab with
         | Some v ->
           match v with
             | IntVal v1 ->
                 if v1 <> 0
                 then
                   raise( InterpreterErr("Argument "+ (mapVarToString(var, varMap)) + " in read not zero", pos))
                 else
                   let value : int  = int(Console.ReadLine())
                   let v' = IntVal(value)
                   SymTab.bind var v' vtab
         | None -> raise( InterpreterErr("Argument "+var+" in read not defined", pos))

   | Local(var, e, pos) ->
       match SymTab.lookup var vtab with
           | None ->  let e' = evalExp(e, vtab)
                      SymTab.bind var e' vtab
           | Some v ->
                      raise(InterpreterErr("Variable " + var + " is already defined", pos))

   | Delocal(var, e, pos) ->
       SymTab.remove var vtab

   | Swap(var1, var2, pos) ->
       let val1 =
           match SymTab.lookup var1 vtab with
            | None -> raise (InterpreterErr("Variable" + var1 + " not defined", pos))
            | Some v -> v
       let val2 =
           match SymTab.lookup var2 vtab with
            | None -> raise (InterpreterErr("Variable" + var2 + " not defined", pos))
            | Some v -> v
       let vtab' = SymTab.bind var1 val2 vtab
       SymTab.bind var2 val1 vtab'

and evalExp(e: Exp,
            vtab: VarTable): Value =
  match e with
  | Constant (v,_)  -> v
  | Var(id, pos)    ->
      match SymTab.lookup id vtab with
       | None    -> raise (InterpreterErr("Variable not defined: "+id, pos))
       | Some m  -> m

  | Plus(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1+n2)

  | Minus(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1-n2)

  | Times(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1*n2)

  | Divide(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal 0)  -> raise(InterpreterErr("Division zero error", pos))
          | (IntVal n1, IntVal n2)  -> IntVal (n1/n2)

  | Modulo(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal 0)  -> raise(InterpreterErr("Division zero error", pos))
          | (IntVal n1, IntVal n2)  -> IntVal (n1%n2)

  | Equal(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab )
        let r2 = evalExp(e2, vtab )
        match (r1, r2) with
          | (IntVal  n1, IntVal  n2) -> if n1 = n2
                                        then
                                            IntVal(1)
                                        else
                                            IntVal (0)

  | Less(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab )
        let r2 = evalExp(e2, vtab )
        match (r1, r2) with
          | (IntVal n1, IntVal n2) -> if n1 < n2
                                       then
                                           IntVal(1)
                                       else
                                           IntVal(0)
  | Or (e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab )
        let r2 = evalExp(e2, vtab )
        match (r1, r2) with
          | (IntVal n1, IntVal n2 )  ->
              match (n1, n2) with
                  | (x,y) when x <> 0 || y <> 0 -> IntVal(1)
                  | _                           -> IntVal(0)
  | And (e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab )
        let r2 = evalExp(e2, vtab )
        match (r1, r2) with
          | (IntVal n1, IntVal n2 )  ->
              match (n1, n2) with
                  | (x,y) when x <> 0 && y <> 0 -> IntVal(1)
                  | _                           -> IntVal(0)
  | Not (e, pos)  ->
        let r1 = evalExp(e, vtab)
        match r1 with
          | (IntVal n1)  ->
              match n1 with
                  | x when x = 0 -> IntVal(1)
                  | _            -> IntVal(0)

// A function for Repeat statement after
// the initial test that the condition is true
and repeatEval(statList: Statement List,
               e : Exp,
               vtab : VarTable,
               ptab : ProcTable,
               gSet: GlobalVars,
               varMap: VarMapping ) : VarTable =

      let vtab' = evalStatementList(statList, vtab, ptab, gSet, varMap)
      let condition =
          match evalExp(e, vtab') with
              | IntVal v  -> v

      if condition <> 0 then
          vtab'
      else
          repeatEval(statList, e, vtab', ptab, gSet, varMap)

// Executes a procedure call
and callProcWithVtable (procdec: ProcDec,
                        aargs : Param List,
                        vtab  : VarTable,
                        ptab  : ProcTable,
                        poscall : Position,
                        gSet : GlobalVars) =

    let (ProcDec (pid, pargs, statements, position)) = procdec

    // Check if arg lists match
    if pargs.Length <> aargs.Length then
        raise(InterpreterErr("Wrong number of args", poscall))

    let aargsStr = List.map (fun x -> getStringOfParam x) aargs
    // Only the values from the global list and parameters
    // are passed in to the function.
    let passedVars = Set.toList(Set.union (Set.ofList gSet) (Set.ofList aargsStr))
    let vtab' = List.fold (fun acc x ->
                           match SymTab.lookup x vtab with
                             | None   ->  // None should never happen
                                raise(InterpreterErr("Variable not found in SymbolTable", poscall))
                             | Some v ->
                                SymTab.bind x v acc ) (SymTab.empty()) passedVars

    // Creates a list of pairs of actual and formal parameters
    let varPair  = List.zip pargs aargs
    // Substitute formal parameters in body with actual
    let s' = Substitute.subStatementList(statements, varPair)
    SymTab.combine (evalStatementList(s', vtab', ptab, gSet, varPair)) vtab

// Recursively evaluates a list of statements
and evalStatementList(sList : Statement List,
                      vtab : VarTable,
                      ptab : ProcTable,
                      gSet : GlobalVars,
                      varMap: VarMapping) : VarTable =
    match sList with
        | []       -> vtab
        | s :: ss  ->
            let vtab' = evalStatement(s, vtab, ptab,gSet,varMap)
            evalStatementList(ss, vtab', ptab, gSet,varMap)

// Evaluates a whole program (The main of the interpreter)
and evalProg (prog: Program) : int =

    let (decl, statements, proc) = prog
    let ptab = buildPtab (proc)
    let declStr = List.map (fun x -> getStringOfIntVar x) decl
    // Build init symbol table with zeroes from list of 'globals'
    let vtab = List.fold (fun acc x -> SymTab.bind x constZero acc) (SymTab.empty()) declStr
    // Check static properties
    StaticChecker.checkStatements(statements, [], declStr)
    if not (declStr = List.distinct declStr) then
        raise (InterpreterErr("Dublicate variables in declaration", (0,0)))
    // Run 'main' statements
    let vtab' = evalStatementList (statements, vtab, ptab, declStr, [])
    // Check if all variables are zero
    StaticChecker.checkVarsIsZero(vtab', declStr)
