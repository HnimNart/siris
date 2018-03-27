module Interpreter

open System
open AbSyn

(* An exception for reporting run-time errors. *)
exception MyError of string * Position



type ProcTable = SymTab.SymTab<UntypedProcDec>
type VarTable = SymTab.SymTab<Value>
type Initialzed = SymTab.SymTab<bool>


// Reverses a list
// Code from https://gist.github.com/thednaz/2897337
let rec rev list acc=
    match list with
    | [] -> acc
    | [x] -> x::acc
    | head::tail -> rev tail (head::acc)



let rec buildPtab(pdecs: UntypedProcDec list): ProcTable =
  match pdecs with
  | [] ->
      SymTab.empty()
  | ( pdcl :: ps ) ->
    (* Bind the user-defined functions, in reverse order. *)
    let pid   = getProcName pdcl
    let pos   = getProcPos pdcl
    let ptab  = buildPtab ps
    match SymTab.lookup pid ptab with
      | None        -> SymTab.bind pid pdcl ptab
      | Some ofdecl ->
          (* Report the first occurrence of the name. *)
          raise (MyError ("Already defined procedure : "+pid, getProcPos ofdecl))



let printVal (v : Value) :unit =
    match v with
        | IntVal v1 -> printfn "%d" v1

let rec evalStatement(s: UntypedStatement, vtab: VarTable, ptab: ProcTable ): VarTable =
  match s with
  | PlusAssignment(var, e, pos) ->
      let eval = evalExp(e, vtab)
      let varval = SymTab.lookup var vtab
      match varval with
        | None ->
            SymTab.bind var eval vtab
        | Some n ->
            let newTab = SymTab.remove var vtab
            let x = evalExp (Plus(Constant(n, pos), Constant(eval,pos),pos), newTab)
            SymTab.bind var x newTab

  | MinusAssignment(var, e, pos ) ->
      let eval = evalExp( e, vtab)
      let varval = SymTab.lookup var vtab
      match varval with
        | None ->
            SymTab.bind var eval vtab
        | Some n ->
            let newTab = SymTab.remove var vtab
            let x = evalExp (Minus(Constant(n, pos), Constant(eval,pos),pos), newTab)
            SymTab.bind var x newTab

  | If(e1, s1, s2, e2, pos) ->

      let e1Val = evalExp(e1, vtab)
      let (vtab', branch) =
          match e1Val with
          | IntVal v1 ->
              if v1 = 0
              then
                 (evalStatementList(s2, vtab, ptab), 0)
              else
                 (evalStatementList(s1, vtab, ptab), 1)

      let e2Val = evalExp(e2, vtab')
      match e2Val with
          | IntVal v2 ->
              if v2 = 0 && branch = 0 then
                  vtab'
              else if v2 <> 0 && branch = 1 then
                  vtab'
              else
                  raise (MyError("Exit condition does not match ", pos))



  | Repeat(s1, e1, pos) ->
      let e1Eval = evalExp(e1, vtab)
      let vtab' =
          match e1Eval with
          | IntVal v ->
              if v = 0
              then
                  raise (MyError("Expression false at beginning ", pos))
              else
                  repeatEval(s1, e1, vtab, ptab)
      vtab'

  | Call (id, pos) ->
      let statements =
          match SymTab.lookup id ptab with
              | None -> failwith "Procedure not defined"
              | Some p -> getProcStat(p)

      evalStatementList(statements, vtab, ptab)



  | Uncall (id, pos) ->
      let statements =
          match SymTab.lookup id ptab with
              | None -> failwith "Procedure not defined"
              | Some p -> getProcStat(p)
      let reversedStatements = rev statements []
      evalStatementList(statements, vtab, ptab)




and evalExp(e: UntypedExp, vtab: VarTable): Value =
  match e with
  | Constant (v,_) -> v
  | Var(id, pos)   ->
      let res = SymTab.lookup id vtab
      match res with
          | None -> raise (MyError("Unknown variable "+id, pos))
          | Some m -> m
  | Plus(e1, e2, pos) ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1+n2)
          // | _ -> raise ( MyError("Plus must be on ints"), pos)

  | Minus(e1, e2, pos) ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1-n2)
          // | _ -> raise ( MyError("Minus must be on ints"), pos)

  | Times(e1, e2, pos) ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1*n2)
          // | _ -> raise ( MyError("Times must be on ints"), pos)

  | Divide(e1, e2, pos) ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1/n2)
          // | _ -> raise ( MyError("Divide must be on ints"), pos)
  | Modulo(e1, e2, pos) ->
        let res1   = evalExp(e1, vtab )
        let res2   = evalExp(e2, vtab )
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1%n2)
          // | _ -> raise ( MyError("Times must be on ints"), pos)


  | Equal(e1, e2, pos) ->
        let r1 = evalExp(e1, vtab )
        let r2 = evalExp(e2, vtab )
        match (r1, r2) with
          | (IntVal  n1, IntVal  n2) ->  if n1 = n2 then IntVal(1) else IntVal (0)
          // | (_, _) -> raise( MyError("Equal must be of type Int", pos))

  | Less(e1, e2, pos) ->
        let r1 = evalExp(e1, vtab )
        let r2 = evalExp(e2, vtab )
        match (r1, r2) with
          | (IntVal  n1,    IntVal  n2  ) -> if n1 < n2 then IntVal(1) else IntVal(0)
          // | (_, _) -> raise( MyError("Less than must be of type Int", pos))

and repeatEval(statList: UntypedStatement List,
                 e : UntypedExp,
                 vtab : VarTable,
                 ptab : ProcTable) : VarTable =
      let vtab' = evalStatementList(statList, vtab, ptab)
      let condition =
          match evalExp(e, vtab') with
              | IntVal v -> v

      if condition = 0 then
          vtab'
      else
          repeatEval(statList, e, vtab', ptab)


and evalStatementList(sList : UntypedStatement List,
                      vtab : VarTable,
                      ptab : ProcTable) : VarTable =

    match sList with
        | []      -> vtab
        | s :: ss ->
            let vtab' = evalStatement(s, vtab, ptab)
            evalStatementList(ss, vtab', ptab)

and evalProg (prog: UntypedProg) : Value =
    let ptab = buildPtab (snd prog)
    let vtab = evalStatementList (fst prog, SymTab.empty(), ptab )
    IntVal(1)
