module Substitute

open AbSyn
type PairList = List<(Param*Param)>

// Helper function to find a corresponding variable name from pairs
// Return the same name if it's not found.
let rec findVarFromPairs(var: string,
                         pairs: PairList): string =
    match pairs with
        | []                          -> var // Not found in list
        | (s1,s2) :: pairs'           ->
            if getStringOfParam(s1) = var then
                getStringOfParam(s2)
            else
                findVarFromPairs(var, pairs')

let rec findVarFromPairsPar(var: Param ,
                            pairs: PairList): Param =
    match pairs with
        | []                    -> var // Not found in list
        | (s1,s2) :: pairs' when var = (s1) -> (s2)  // Found return
        | s :: pairs'           ->  findVarFromPairsPar(var, pairs')

let rec subsVarsInSList(ss: Statement List,
                        pairs: PairList) : Statement List =
    match ss with
        | [] -> []
        | s :: ss' ->
            subVarsInStat(s, pairs) :: subsVarsInSList(ss', pairs)

and subVarsInStat(s: Statement,
                  pairs: PairList) : Statement =
    match s with
      | PlusAssignment(id, e, pos) ->
          let e' = subVarsInExp(e, pairs)
          let newVar = findVarFromPairs(id, pairs)
          PlusAssignment(newVar, e', pos)

      | MinusAssignment(id, e, pos) ->
          let e' = subVarsInExp(e, pairs)
          let newVar = findVarFromPairs(id, pairs)
          MinusAssignment(newVar, e', pos)

      | If (e1, s1, s2, e2, pos) ->
         let s1' = subsVarsInSList(s1, pairs)
         let s2' = subsVarsInSList(s2, pairs)
         let e1' = subVarsInExp(e1, pairs)
         let e2' = subVarsInExp(e2, pairs)
         If (e1', s1', s2', e2', pos)

      | Repeat(s1, e1, pos) ->
         let s1' = subsVarsInSList(s1, pairs)
         let e1' = subVarsInExp(e1, pairs)
         Repeat(s1', e1', pos)

      | Call (id, param, pos) ->
          let param' = List.map (fun x -> findVarFromPairsPar(x, pairs)) param
          Call(id, param', pos)

      | Uncall(id, param, pos) ->
          let param' = List.map (fun x -> findVarFromPairsPar( x, pairs)) param
          Uncall(id, param', pos)

      | Print(var, pos) ->
          let newVar = findVarFromPairs(var, pairs)
          Print(newVar, pos)

      | Read(var, pos) ->
          let newVar = findVarFromPairs(var, pairs)
          Read(newVar, pos)

      | Local(var, e, pos) ->
          let e'   = subVarsInExp(e, pairs)
          Local(var, e', pos)

      | Delocal(var, e,pos) ->
          let e'   = subVarsInExp(e, pairs)
          Delocal(var ,e, pos)

      | Swap(v1, v2, pos) ->
          let v1' = findVarFromPairs (v1 , pairs)
          let v2' = findVarFromPairs (v2, pairs)
          Swap(v1', v2', pos)

and subVarsInExp(e: Exp,
                 pairs: PairList) : Exp =
    match e with
      | Constant (v,p)  ->
          Constant(v,p)
      | Var(id, pos)    ->
          let id' = findVarFromPairs(id, pairs)
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
