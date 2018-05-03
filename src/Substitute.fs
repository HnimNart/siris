module Substitute

open AbSyn
// List of mappings from formal to actual parameters
type PairList = List<(Param*Param)>

let counter : int ref = ref 0

let newName var_name =
    let _ = counter := !counter + 1
    let n = string (!counter)
    "_" + var_name + "_" + n + "_"

// Renames a local if there is a conflict with
// actual parameters. If not it just returns the same
// variable name
let rec renameLocal(var:string,
                    pairs:PairList) =
    match pairs with
        | []                                 -> var
        | (s1, s2) :: _ when (getStringOfParam s2) = var  -> newName var
        | _ :: pairss                        -> renameLocal(var, pairss)

// Helper function to find a corresponding variable name from pairs
// Return the same name if it's not found.
// If the variable is not found then we don't need to map it.
let rec subVar(var: string, pairs: PairList): string =
    match pairs with
        | []                          -> var // Not found in list
        | (s1,s2) :: pairs' when getStringOfParam(s1) = var   -> getStringOfParam(s2)
        | _ :: pairs'                 -> subVar(var, pairs')

// Like above just for param types.
let rec subParam(var: Param, pairs: PairList): Param =
    match pairs with
        | []                              -> var
        | (s1,s2) :: pairs' when var = s1 -> s2
        | s :: pairs'                     -> subParam(var, pairs')

// Substitutes formal parameters with actual
// in a list of statements
let rec subStatementList(ss: Statement List,
                         pairs: PairList) : Statement List =
      match ss with
      | [] -> []
      | PlusAssignment(id, e, pos) :: ss'->
          let e' = subVarsInExp(e, pairs)
          let newVar = subVar(id, pairs)
          PlusAssignment(newVar, e', pos) :: subStatementList(ss', pairs)

      | MinusAssignment(id, e, pos) :: ss' ->
          let e' = subVarsInExp(e, pairs)
          let newVar = subVar(id, pairs)
          MinusAssignment(newVar, e', pos) :: subStatementList(ss',pairs)

      | If (e1, s1, s2, e2, pos) :: ss' ->
         let s1' = subStatementList(s1, pairs)
         let s2' = subStatementList(s2, pairs)
         let e1' = subVarsInExp(e1, pairs)
         let e2' = subVarsInExp(e2, pairs)
         If (e1', s1', s2', e2', pos) :: subStatementList(ss',pairs)

      | Repeat(s1, e1, pos):: ss'  ->
         let s1' = subStatementList(s1, pairs)
         let e1' = subVarsInExp(e1, pairs)
         Repeat(s1', e1', pos) :: subStatementList(ss',pairs)

      | Call (id, param, pos):: ss'  ->
          let param' = List.map (fun x -> subParam(x, pairs)) param
          Call(id, param', pos) :: subStatementList(ss',pairs)

      | Uncall(id, param, pos):: ss'  ->
          let param' = List.map (fun x -> subParam( x, pairs)) param
          Uncall(id, param', pos) :: subStatementList(ss',pairs)

      | Print(var, pos):: ss'  ->
          let newVar = subVar(var, pairs)
          Print(newVar, pos) :: subStatementList(ss',pairs)

      | Read(var, pos):: ss'  ->
          let newVar = subVar(var, pairs)
          Read(newVar, pos) :: subStatementList(ss',pairs)

      | Local(var, e, pos):: ss'  ->
          let var'   = renameLocal(var, pairs)
          let pairs' = (Param(var), Param(var')) :: pairs
          let e'     = subVarsInExp(e, pairs)
          Local(var', e', pos) :: subStatementList(ss',pairs')

      | Delocal(var, e,pos):: ss'  ->
          let e'   = subVarsInExp(e, pairs)
          let var' = subVar(var, pairs)
          Delocal(var', e', pos) :: subStatementList(ss',pairs)

      | Swap(v1, v2, pos) :: ss'  ->
          let v1' = subVar (v1, pairs)
          let v2' = subVar (v2, pairs)
          Swap(v1', v2', pos) :: subStatementList(ss',pairs)

and subVarsInExp(e: Exp,
                 pairs: PairList) : Exp =
    match e with
      | Constant (v,p)  ->
          Constant(v,p)
      | Var(id, pos)    ->
          let id' = subVar(id, pairs)
          Var(id', pos)
      | Plus(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Plus(e1',e2', pos)
      | Minus(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Minus(e1',e2', pos)
      | Times(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Times(e1',e2', pos)
      | Divide(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Divide(e1',e2', pos)
      | Modulo(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Modulo(e1',e2', pos)
      | Equal(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Equal(e1',e2', pos)
      | Less(e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Less(e1',e2', pos)
      | Or (e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          Or(e1',e2', pos)
      | And (e1, e2, pos)  ->
          let e1' = subVarsInExp(e1, pairs)
          let e2' = subVarsInExp(e2, pairs)
          And(e1',e2', pos)
      | Neg (e, pos)  ->
          let e' = subVarsInExp(e, pairs)
          Neg(e', pos)
