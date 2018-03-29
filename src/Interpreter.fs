module Interpreter

open System
open AbSyn

(* An exception for reporting run-time errors. *)
exception MyError of string * Position

type ProcTable = SymTab.SymTab<UntypedProcDec>
type VarTable = SymTab.SymTab<Value>
type Initialzed = SymTab.SymTab<bool>

let constZero = IntVal(0)

// builds procedure symbol table
let rec buildPtab(pdecs: UntypedProcDec list): ProcTable =
  match pdecs with
  | []  ->
      SymTab.empty()
  | ( pdcl :: ps )  ->
    (* Bind the user-defined functions, in reverse order. *)
    let pid   = getProcName pdcl
    let pos   = getProcPos pdcl
    let ptab  = buildPtab ps
    match SymTab.lookup pid ptab with
      | None         -> SymTab.bind pid pdcl ptab
      | Some ofdecl  ->
          (* Report the first occurrence of the name. *)
          raise (MyError ("Already defined procedure : "+pid, getProcPos ofdecl))


let rec evalStatement(s: UntypedStatement, vtab: VarTable, ptab: ProcTable ): VarTable =
  match s with
  | PlusAssignment(id, e, pos)  ->
      let eval = evalExp(e, vtab, id)
      let value = SymTab.lookup id vtab
      match value with
        | None  ->
            SymTab.bind id eval vtab
        | Some n  ->
            // let newTab = SymTab.remove var vtab
            let newVal = evalExp (Plus(Constant(n, pos), Constant(eval,pos),pos), vtab , "")
            SymTab.bind id newVal vtab

  | MinusAssignment(id, e, pos )  ->
      let eval = evalExp( e, vtab, id)
      let value = SymTab.lookup id vtab
      match value with
        | None  ->
            SymTab.bind id eval vtab
        | Some n  ->
            let newVal = evalExp (Minus(Constant(n, pos), Constant(eval,pos),pos), vtab, "")
            SymTab.bind id newVal vtab

  | If(e1, s1, s2, e2, pos)  ->
      let e1Val = evalExp(e1, vtab, "")
      let (vtab', branch) =
          match e1Val with
          | IntVal v1  ->
              if v1 = 0
              then
                 (evalStatementList(s2, vtab, ptab), 0)
              else
                 (evalStatementList(s1, vtab, ptab), 1)

      let e2Val = evalExp(e2, vtab', "")
      try
        match e2Val with
          | IntVal v2  ->
             if v2 = 0 && branch = 0
             then
                 vtab'
             else if v2 <> 0 && branch = 1
             then
                 vtab'
             else
                 raise ( MyError("Exit condition does not match ", pos))
      with
          | MyError(str, pos) -> failwithf "If Exception: %s at %A" str pos

  | Repeat(s1, e1, pos)  ->
      let e1Eval = evalExp(e1, vtab, "")
      try
        match e1Eval with
          | IntVal v  ->
             if v = 0
             then
                 raise ( MyError("Expression false at beginning ", pos))
             else
                 repeatEval(s1, e1, vtab, ptab)
      with
          | MyError(str, pos) -> failwithf "Repeat: %s at %A" str pos


  | Call (id, pos)  ->
      let statements =
          match SymTab.lookup id ptab with
              | None  -> failwithf "Procedure %s not defined" id
              | Some p  -> getProcStat(p)

      evalStatementList(statements, vtab, ptab)

  | Uncall (id, pos)  ->
      let statements =
          match SymTab.lookup id ptab with
              | None  -> failwithf "Procedure %s not defined" id
              | Some p  -> getProcStat(p)
      let reversedStatements = ReverseProg.rev statements []
      evalStatementList(statements, vtab, ptab)

  | Print(var, pos) ->

      try
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
      with
          | MyError(str, pos) -> failwithf "Print: %s at %A" str pos

   | Read(var, pos) ->
       try
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
               | None -> raise(MyError("Argument "+var+" to Read not defined", pos))
       vtab'
       with
           | MyError(str, pos) -> failwithf "Read: %s at %A" str pos

and evalExp(e: UntypedExp,
            vtab: VarTable,
            lhsVariable: string): Value =
  match e with
  | Constant (v,_)  -> v
  | Var(id, pos)    ->
      let res = SymTab.lookup id vtab

      try
       match res with
        | None  -> raise (MyError("Variable not defined: "+id, pos))
        | Some m  ->
              if lhsVariable = id
              then
                 raise (MyError("Expression contains lhs variable "+ id , pos))
              else
                 m
      with
        | MyError(str, pos) -> failwithf "Var exception: %s at %A" str pos

  | Plus(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVariable)
        let res2   = evalExp(e2, vtab , lhsVariable)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1+n2)

  | Minus(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVariable)
        let res2   = evalExp(e2, vtab , lhsVariable)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1-n2)

  | Times(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVariable)
        let res2   = evalExp(e2, vtab , lhsVariable)
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1*n2)

  | Divide(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVariable)
        let res2   = evalExp(e2, vtab , lhsVariable)
        try
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1/n2)

        with
          | :? System.DivideByZeroException ->
              failwithf "Divison zero exception at %A" pos
  | Modulo(e1, e2, pos)  ->
        let res1   = evalExp(e1, vtab , lhsVariable)
        let res2   = evalExp(e2, vtab , lhsVariable)
        try
        match (res1, res2) with
          | (IntVal n1, IntVal n2)  -> IntVal (n1%n2)

        with
          | :? System.DivideByZeroException ->
              failwithf "Modulo: Divison zero exception at %A" pos

  | Equal(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVariable)
        let r2 = evalExp(e2, vtab , lhsVariable)
        match (r1, r2) with
          | (IntVal  n1, IntVal  n2)  ->  if n1 = n2 then IntVal(1) else IntVal (0)

  | Less(e1, e2, pos)  ->
        let r1 = evalExp(e1, vtab , lhsVariable)
        let r2 = evalExp(e2, vtab , lhsVariable)
        match (r1, r2) with
          | (IntVal  n1,    IntVal  n2  )  -> if n1 < n2 then IntVal(1) else IntVal(0)

and repeatEval(statList: UntypedStatement List,
                 e : UntypedExp,
                 vtab : VarTable,
                 ptab : ProcTable) : VarTable =
      let vtab' = evalStatementList(statList, vtab, ptab)
      let condition =
          match evalExp(e, vtab', "") with
              | IntVal v  -> v

      if condition = 0 then
          vtab'
      else
          repeatEval(statList, e, vtab', ptab)


and evalStatementList(sList : UntypedStatement List,
                      vtab : VarTable,
                      ptab : ProcTable) : VarTable =

    match sList with
        | []       -> vtab
        | s :: ss  ->
            let vtab' = evalStatement(s, vtab, ptab)
            evalStatementList(ss, vtab', ptab)

and evalProg (prog: UntypedProg) : int =
    let ptab = buildPtab (snd prog)
    // Build init symbol table
    let vSet = Checker.initializeProg(prog)
    let vTab : VarTable = Set.fold (fun acc x -> SymTab.bind x constZero acc) (SymTab.empty()) vSet

    let vTab' = evalStatementList (fst prog, vTab, ptab)
    // Check if all variables are non-zero
    Checker.checkNonZeroVar(vTab', vSet)
