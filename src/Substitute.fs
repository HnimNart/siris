module Substitute

open AbSyn
type PairList = List<(Param*Param)>

// Helper function to find a corresponding variable name from pairs
// Return the same name if it's not found.
let rec subVar(var: string, pairs: PairList): string =
    match pairs with
        | []                          -> var // Not found in list
        | (s1,s2) :: pairs'           ->
            if getStringOfParam(s1) = var then
                getStringOfParam(s2)
            else
                subVar(var, pairs')

// Like above just for param types.
let rec subParam(var: Param, pairs: PairList): Param =
    match pairs with
        | []                              -> var
        | (s1,s2) :: pairs' when var = s1 -> s2
        | s :: pairs'                     -> subParam(var, pairs')

let rec subStatementList(ss: Statement List,
                         pairs: PairList) : Statement List =
    match ss with
        | [] -> []
        | s :: ss' ->
            subStatement(s, pairs) :: subStatementList(ss', pairs)

and subStatement(s: Statement,
                  pairs: PairList) : Statement =
    match s with
      | PlusAssignment(id, e, pos) ->
          let e' = subVarsInExp(e, pairs)
          let newVar = subVar(id, pairs)
          PlusAssignment(newVar, e', pos)

      | MinusAssignment(id, e, pos) ->
          let e' = subVarsInExp(e, pairs)
          let newVar = subVar(id, pairs)
          MinusAssignment(newVar, e', pos)

      | If (e1, s1, s2, e2, pos) ->
         let s1' = subStatementList(s1, pairs)
         let s2' = subStatementList(s2, pairs)
         let e1' = subVarsInExp(e1, pairs)
         let e2' = subVarsInExp(e2, pairs)
         If (e1', s1', s2', e2', pos)

      | Repeat(s1, e1, pos) ->
         let s1' = subStatementList(s1, pairs)
         let e1' = subVarsInExp(e1, pairs)
         Repeat(s1', e1', pos)

      | Call (id, param, pos) ->
          let param' = List.map (fun x -> subParam(x, pairs)) param
          Call(id, param', pos)

      | Uncall(id, param, pos) ->
          let param' = List.map (fun x -> subParam( x, pairs)) param
          Uncall(id, param', pos)

      | Print(var, pos) ->
          let newVar = subVar(var, pairs)
          Print(newVar, pos)

      | Read(var, pos) ->
          let newVar = subVar(var, pairs)
          Read(newVar, pos)

      | Local(var, e, pos) ->
          let e'   = subVarsInExp(e, pairs)
          Local(var, e', pos)

      | Delocal(var, e,pos) ->
          let e'   = subVarsInExp(e, pairs)
          Delocal(var , e', pos)

      | Swap(v1, v2, pos) ->
          let v1' = subVar (v1, pairs)
          let v2' = subVar (v2, pairs)
          Swap(v1', v2', pos)

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
      | Not (e, pos)  ->
          let e' = subVarsInExp(e, pairs)
          Not(e', pos)
