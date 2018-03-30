module Interpreter

open System
open AbSyn

(* An exception for reporting run-time errors. *)
exception MyError of string * Position

type ProcTable = SymTab.SymTab<ProcDec>
type VarTable = SymTab.SymTab<Value>
type VarSet = Set<string>

let constZero = IntVal(0)


let isZero(x, vtab) =
    match SymTab.lookup x vtab with
        // | None            -> failwithf "Couldn't find %s in symbol table" x
        | Some (IntVal 0) -> true
        | _               -> printfn "Variable %s is not zero a termination" x
                             false

// Checks if all variables in vSet are zero
let checkNonZeroVar(vTab, vSet): int =
    if Set.fold (fun acc x -> acc && isZero (x ,vTab)) true vSet
    then
        0
    else
        raise(MyError("Error: Variable(s) are not zero", (0,0)))

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

(* Bind the formal parameters of a function declaration to actual parameters in
   a new vtab. *)
let rec bindParams = function
  | ([], [], pid) -> SymTab.empty()

  | ([], a,  pid) ->
      failwith "BindParams error 1"
  | (b,  [], pid) ->
      failwith "BindParams error 1"

  | ( Param(paid) :: pargs, a::args, pid) ->
        let vtab = bindParams( pargs, args, pid)
        match SymTab.lookup paid vtab with
               None   -> SymTab.bind paid a vtab
             | Some m -> failwith "BindParams Error"

let rec evalStatement(s: Statement,
                      vtab: VarTable,
                      ptab: ProcTable,
                      vSet: VarSet): VarTable =
  match s with
  | PlusAssignment(id, e, pos)  ->
      let eval = evalExp(e, vtab, id)
      let value = SymTab.lookup id vtab
      match value with
        | None  ->
            raise (MyError("LHS variable " + id + " is not defined", pos))
        | Some n  ->
            let newVal = evalExp (Plus(Constant(n, pos), Constant(eval,pos),pos), vtab , "")
            SymTab.bind id newVal vtab

  | MinusAssignment(id, e, pos )  ->
      let eval = evalExp( e, vtab, id)
      let value = SymTab.lookup id vtab
      match value with
        | None  ->
            raise (MyError("LHS variable " + id + " is not defined", pos))
        | Some n  ->
            let newVal = evalExp (Minus(Constant(n, pos), Constant(eval,pos),pos), vtab, "")
            SymTab.bind id newVal vtab

  | If(e1, s1, s2, e2, pos)  ->
      let e1Val = evalExp(e1, vtab, "")
      let (vtab', branch) =
          match e1Val with
          | IntVal v1  ->
              if v1 <> 0
              then
                 (evalStatementList(s1, vtab, ptab, vSet), 1)
              else
                 (evalStatementList(s2, vtab, ptab, vSet), 0)

      let e2Val = evalExp(e2, vtab', "")
      match e2Val with
        | IntVal v2  ->
           if v2 = 0 && branch = 0
           then
               vtab'
           else if v2 = 1 && branch = 1
           then
               vtab'
           else
               raise ( MyError("If: Assertion in second condition does not match ", pos))

  | Repeat(s1, e1, pos)  ->
      let e1Eval = evalExp(e1, vtab, "")
      match e1Eval with
        | IntVal v  ->
          if v = 0
          then
              raise ( MyError("Repeat: Condition false at entry", pos))
          else
              repeatEval(s1, e1, vtab, ptab, vSet)

  | Call (id, param, pos)  ->

      match SymTab.lookup id ptab with
        | None    -> raise( MyError("Undefined procedure: " + id, pos))
        | Some p  -> callProcWithVtable (p, param, vtab, ptab, pos, vSet)

  | Uncall (id, param, pos)  ->

      match SymTab.lookup id ptab with
        | None    -> raise( MyError("Undefined procedure: " + id, pos))
        | Some p  ->
            let inverseProcedure = ReverseProg.inverseProcedures p
            callProcWithVtable (inverseProcedure, param , vtab, ptab, pos, vSet)

  | Print(var, pos) ->
      let value =
          match SymTab.lookup var vtab with
              | Some v -> v
              | None -> raise( MyError("Argument " + var + " to print not defined", pos))

      let vtab' =
        match value with
            | IntVal( v ) ->
                printf "%i\n" v
                SymTab.bind var constZero vtab
      vtab'

   | Read(var, pos) ->
       let vtab' =
           match SymTab.lookup var vtab with
               | Some v ->
                   match v with
                       | IntVal v1 ->
                           if v1 <> 0
                           then
                               raise( MyError("Argument "+ var + " to read not zero", pos))
                           else
                               let value : int  = int(Console.ReadLine())
                               let v' = IntVal(value)
                               SymTab.bind var v' vtab
               | None -> raise(MyError("Argument "+var+" to read not defined", pos))
       vtab'
   | Local(var, e, pos) ->
       let e' = evalExp(e, vtab, var)
       SymTab.bind var e' vtab
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


and callProcWithVtable (procdec: ProcDec
                      , aargs : Param list
                      , vtab  : VarTable
                      , ptab  : ProcTable
                      , pcall : Position
                      , vSet : VarSet ) =
    let (ProcDec (pid, pargs,  statements, position)) = procdec

    let evargs = List.map (fun e -> match SymTab.lookup (getStringOfParam e) vtab with
                                     | None -> raise( MyError("Parameter " + getStringOfParam e  +
                                                                 " is not defined", position))
                                     | Some v -> v) aargs

    // Binds values to formal parameters
    let vtab' = SymTab.combine (bindParams (pargs, evargs, pid)) vtab
    // Creates a list of pairs of actual and formal parameters
    // Used to later update used parameter in the celler enviroment
    let varPair  = List.zip aargs pargs
    let retTable = evalStatementList(statements, vtab', ptab, vSet)

    // Binds new values to global vtab.
    List.fold ( fun acc (act,form) -> match SymTab.lookup (getStringOfParam form) retTable with
                                        | None  -> SymTab.combine(SymTab.empty()) acc
                                        | Some v -> SymTab.bind (getStringOfParam act) v acc) vtab varPair


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
        try
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1/n2)

        with
          | :? System.DivideByZeroException ->
              failwithf "Divison zero exception at %A" pos
  | Modulo(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVar)
        let res2   = evalExp(e2, vtab , lhsVar)
        try
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1%n2)

        with
          | :? System.DivideByZeroException ->
              failwithf "Modulo: Divison zero exception at %A" pos

  | Equal(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal  n1, IntVal  n2)  ->  if n1 = n2 then IntVal(1) else IntVal (0)

  | Less(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal  n1,    IntVal  n2  )  -> if n1 < n2 then IntVal(1) else IntVal(0)
  | Or (e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVar)
        let r2 = evalExp(e2, vtab , lhsVar)
        match (r1, r2) with
          | (IntVal n1, IntVal n2 )  ->
              match (n1, n2) with
                  | (x,y) when x = 1 || y = 1 -> IntVal(1)
                  | (0,0)                     -> IntVal(0)
                  |  _    -> raise( MyError("Expression is not a condition", pos))



and repeatEval(statList: Statement List,
                 e : Exp,
                 vtab : VarTable,
                 ptab : ProcTable,
                 vSet: VarSet) : VarTable =
      let vtab' = evalStatementList(statList, vtab, ptab, vSet)
      let condition =
          match evalExp(e, vtab', "") with
              | IntVal v  -> v

      if condition <> 0 then
          vtab'
      else
          repeatEval(statList, e, vtab', ptab, vSet)


and evalStatementList(sList : Statement List,
                      vtab : VarTable,
                      ptab : ProcTable,
                      vSet : VarSet) : VarTable =
    match sList with
        | []       -> vtab
        | s :: ss  ->
            let vtab' = evalStatement(s, vtab, ptab, vSet)
            evalStatementList(ss, vtab', ptab, vSet)

and evalProg (prog: Program) : int =

    let (decl, statements, proc) = prog
    let ptab = buildPtab (proc)
    // Build init symbol table with zeroes
    let vTab = List.fold (fun acc x -> SymTab.bind (getStringOfDecl x) constZero acc) (SymTab.empty()) decl
    let vSet = Set.ofList ( List.map (fun x -> (getStringOfDecl x)) decl)
    let vTab' = evalStatementList (statements, vTab, ptab, vSet)

    // Check if all variables are zero
    checkNonZeroVar(vTab', vSet)
