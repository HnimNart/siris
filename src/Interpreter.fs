module Interpreter

open System
open AbSyn

(* Exceptions for reporting run-time errors. *)
exception MyError of string * Position
exception MyError' of string * List<(string*int)>

type ProcTable = SymTab.SymTab<ProcDec>
type VarTable = SymTab.SymTab<Value>

let constZero = IntVal(0)

// Checks if all variables in vSet are zero
let checkNonZeroVar(vTab, decl) =
    let isZero(x, vtab) =
      match SymTab.lookup x vtab with
        | Some (IntVal 0) -> []
        | Some (IntVal v) -> [(x,v)]
        | _               -> raise(MyError("Variable" + x + "not found", (0,0) ))


    List.fold (fun acc x -> acc @ isZero (x ,vTab)) [] decl

// builds procedure symbol table
let rec buildPtab(pdecs: ProcDec list): ProcTable =
  match pdecs with
  | []              -> SymTab.empty()
  | ( pdcl :: ps )  ->
    (* Bind the user-defined procedures, in reverse order. *)
    let pid   = getProcName pdcl
    let pos   = getProcPos pdcl
    let ptab  = buildPtab ps
    match SymTab.lookup pid ptab with
      | None         -> SymTab.bind pid pdcl ptab
      | Some ofdecl  ->
          (* Report the first occurrence of the name. *)
          raise (MyError ("Already defined procedure : "+pid, getProcPos ofdecl))

let rec evalStatement(s: Statement,
                      vtab: VarTable,
                      ptab: ProcTable,
                      gSet: string list): VarTable =
  match s with
  | PlusAssignment(id, e, pos)  ->
      let eVal = evalExp(e, vtab, id)
      let value = SymTab.lookup id vtab
      match value with
        | None  ->
            raise (MyError("LHS variable " + id + " is not defined", pos))
        | Some n  ->
            let newVal = evalExp (Plus(Constant(n, pos), Constant(eVal,pos),pos), vtab , "")
            SymTab.bind id newVal vtab

  | MinusAssignment(id, e, pos )  ->
      let eVal = evalExp(e, vtab, id)
      let value = SymTab.lookup id vtab
      match value with
        | None  ->
            raise (MyError("LHS variable " + id + " is not defined", pos))
        | Some n  ->
            let newVal = evalExp (Minus(Constant(n, pos), Constant(eVal,pos),pos), vtab, "")
            SymTab.bind id newVal vtab

  | If(e1, s1, s2, e2, pos)  ->
      let (vtab', cond) =
          match evalExp(e1, vtab, "") with
          | IntVal v1  ->
              if v1 <> 0
              then
                 (evalStatementList(s1, vtab, ptab, gSet), v1)
              else
                 (evalStatementList(s2, vtab, ptab, gSet), v1)

      // Check that fi condition is the same
      match evalExp(e2, vtab', "") with
        | IntVal v2  ->
           if (v2 = 0 && cond = 0) || (v2 <> 0 && cond <> 0 )
           then
               vtab'
           else
               raise ( MyError("If: Assertion in second condition does not match.\n" +
                               "Second condition evaluated to " + (string v2) + "" , pos))

  | Repeat(s1, e1, pos)  ->
      let e1Eval = evalExp(e1, vtab, "")
      match e1Eval with
        | IntVal v  ->
          if v = 0
          then
              raise ( MyError("Repeat: Condition false at entry", pos))
          else
              repeatEval(s1, e1, vtab, ptab, gSet)

  | Call (id, param, pos)  ->
      match SymTab.lookup id ptab with
        | None    -> raise( MyError("Undefined procedure: " + id, pos))
        | Some p  -> callProcWithVtable (p, param, vtab, ptab, pos, gSet)

  | Uncall (id, param, pos)  ->
      match SymTab.lookup id ptab with
        | None    -> raise( MyError("Undefined procedure: " + id, pos))
        | Some (ProcDec(id, parg, stat, pos))  ->
            let iStat = ReverseProg.inverseStatementList (ReverseProg.rev stat [])
            callProcWithVtable (ProcDec(id, parg, iStat, pos), param , vtab, ptab, pos, gSet)

  | Print(var, pos) ->
      let value =
          match SymTab.lookup var vtab with
              | Some v -> v
              | None -> raise( MyError("Argument " + var + " in print not defined", pos))

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
                   raise( MyError("Argument "+ var + " in read not zero", pos))
                 else
                   let value : int  = int(Console.ReadLine())
                   let v' = IntVal(value)
                   SymTab.bind var v' vtab
         | None -> raise(MyError("Argument "+var+" in read not defined", pos))

   | Local(var, e, pos) ->
       match SymTab.lookup var vtab with
           | None ->  let e' = evalExp(e, vtab, var)
                      SymTab.bind var e' vtab
           | Some v ->
                      raise(MyError("Variable " + var + "is already defined", pos))

   | Delocal (var, e, pos) ->
       SymTab.remove var vtab

   | Swap(var1, var2, pos) ->
       let val1 =
           match SymTab.lookup var1 vtab with
            | None -> raise (MyError("Variable" + var1 + " not defined", pos))
            | Some v -> v
       let val2 =
           match SymTab.lookup var2 vtab with
            | None -> raise (MyError("Variable" + var2 + " not defined", pos))
            | Some v -> v
       let vtab' = SymTab.bind var1 val2 vtab
       SymTab.bind var2 val1 vtab'

and evalExp(e: Exp,
            vtab: VarTable,
            lhsVar: string): Value =
  match e with
  | Constant (v,_)  -> v
  | Var(id, pos)    ->
      if id = lhsVar then
        raise (MyError("Expression contains lhs variable "+ id , pos))

      match SymTab.lookup id vtab with
       | None    -> raise (MyError("Variable not defined: "+id, pos))
       | Some m  -> m

  | Plus(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVar)
        let res2   = evalExp(e2, vtab , lhsVar)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1+n2)

  | Minus(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVar)
        let res2   = evalExp(e2, vtab , lhsVar)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1-n2)

  | Times(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVar)
        let res2   = evalExp(e2, vtab , lhsVar)

        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1*n2)

  | Divide(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVar)
        let res2   = evalExp(e2, vtab , lhsVar)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1/n2)

  | Modulo(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVar)
        let res2   = evalExp(e2, vtab , lhsVar)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1%n2)
  | Equal(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal  n1, IntVal  n2) -> if n1 = n2
                                        then
                                            IntVal(1)
                                        else
                                            IntVal (0)

  | Less(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal n1, IntVal n2) -> if n1 < n2
                                       then
                                           IntVal(1)
                                       else
                                           IntVal(0)
  | Or (e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal n1, IntVal n2 )  ->
              match (n1, n2) with
                  | (x,y) when x <> 0 || y <> 0 -> IntVal(1)
                  | _                           -> IntVal(0)
  | And (e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal n1, IntVal n2 )  ->
              match (n1, n2) with
                  | (x,y) when x <> 0 && y <> 0 -> IntVal(1)
                  | _                           -> IntVal(0)

and repeatEval(statList: Statement List,
               e : Exp,
               vtab : VarTable,
               ptab : ProcTable,
               gSet: string list) : VarTable =
      let vtab' = evalStatementList(statList, vtab, ptab, gSet )
      let condition =
          match evalExp(e, vtab', "") with
              | IntVal v  -> v

      if condition <> 0 then
          vtab'
      else
          repeatEval(statList, e, vtab', ptab, gSet)

and callProcWithVtable (procdec: ProcDec,
                        aargs : Param List,
                        vtab  : VarTable,
                        ptab  : ProcTable,
                        poscall : Position,
                        gSet : string List) =
    let (ProcDec (pid, pargs,  statements, position)) = procdec

    // Check if arg lists match
    if pargs.Length <> aargs.Length then
        raise(MyError("Wrong number of args", poscall))

    let aargsStr = List.map (fun x -> getStringOfParam x) aargs
    let VarToPassed = Set.toList(Set.union (Set.ofList gSet) (Set.ofList aargsStr))
    // Only get the values from the global list and
    // the local variables the are passed in to procedure
    let vtab' = List.fold (fun acc x ->
                           match SymTab.lookup x vtab with
                           // None should never happen
                           | None   -> raise(MyError("Variable not found in SymbolTable", poscall))
                           | Some v -> SymTab.bind x v acc ) (SymTab.empty()) VarToPassed

    // Creates a list of pairs of actual and formal parameters
    let varPair  = List.zip pargs aargs
    // Substitute formal parameters in body with actual
    let s' = Substitute.subsVarsInSList(statements, varPair)
    evalStatementList(s', vtab', ptab, gSet)

and evalStatementList(sList : Statement List,
                      vtab : VarTable,
                      ptab : ProcTable,
                      gSet : string List) : VarTable =
    match sList with
        | []       -> vtab
        | s :: ss  ->
            let vtab' = evalStatement(s, vtab, ptab,gSet)
            evalStatementList(ss, vtab', ptab, gSet)

and evalProg (prog: Program) : int =

    let (decl, statements, proc) = prog
    let ptab = buildPtab (proc)
    let declStr = List.map (fun x -> getStringOfDecl x) decl
    // Build init symbol table with zeroes from list of 'globals'
    let vTab = List.fold (fun acc x -> SymTab.bind x constZero acc) (SymTab.empty()) declStr

    if not (declStr = List.distinct declStr) then
        raise (MyError("Dublicate variables in declaration", (0,0)))

    // Run 'main' statements
    let vTab' = evalStatementList (statements, vTab, ptab, declStr)

    // Check if all variables are zero
    let endVals = checkNonZeroVar(vTab', declStr)
    if List.isEmpty endVals then
        0
    else
        raise (MyError'("The following variables are not zero at termination", endVals))
