module StaticChecker
open AbSyn

exception TerminationException of string * List<(string*int)>
exception StaticListException of string * List<string*Position>
exception StaticException of string * Position

// Checks if all global variables are zero
// Return a list of variables that are not zero
let checkVarsIsZero(vTab, decl) =
    let isZero(x, vtab) =
      match SymTab.lookup x vtab with
        | Some (IntVal 0) -> []
        | Some (IntVal v) -> [(x,v)]
        | _               ->
            // This should never happen
            raise(StaticException("Variable " + x + " not found in symbol table", (0,0) ))
    let vals = List.fold (fun acc x -> acc @ isZero (x ,vTab)) [] decl
    if List.isEmpty vals then
        0
    else
        raise (TerminationException("The following variables are not zero at termination", vals))

let rec checkLhsVar(e: Exp, lhsvar:string):unit =
    match e with
        | Constant (v,pos) -> ()
        | Var(s, pos)       ->
            if s = lhsvar then
                raise(StaticException("Expression contains LHS variable ", pos))
        | Plus(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Minus(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Times(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Divide(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Modulo(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Equal(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Less(e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Or (e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | And (e1, e2, pos)  ->
            checkLhsVar(e1,lhsvar)
            checkLhsVar(e2,lhsvar)
        | Not (e, pos)  ->
            raise(StaticException("Cannot use Not(!) in an assignment statement", pos))
            checkLhsVar(e,lhsvar)

// Performs static checks of statements
// 1. Check for matching local and delocal statemetns
// 2. Dublicate definition for local variables
// E.g. from formal paramters or local declarations
// 3. Check that expression does not contain lhs variable
// Returns a list of variables that do not have macthing
// deallocations in a statement block
let rec checkStatements(statList: Statement List,
                        stack : List<string*Position>,
                        formalParams : List<string>): unit =
    match statList with
        | [] ->
            if not stack.IsEmpty then
                raise( StaticListException("Unmatched local declaration", stack))
                ()
        | s :: ss ->
            match s with
                | Local(var, e, pos) ->
                    if List.exists(fun (v,p) -> v = var ) stack
                    then
                        raise(StaticException("Dublicate definition of local variable " + var, pos))
                    if List.exists(fun (v) -> v = var) formalParams
                    then
                        raise(StaticException("Local variable name is also a formal paramters", pos))
                    let stack' = (var, pos) :: stack
                    checkStatements(ss, stack', formalParams)

                | Delocal(var, e, pos) ->
                    if not (List.exists (fun (s,p) -> var = s) stack) then
                        raise(StaticException("No matching local of delocal", pos))
                    let stack' = List.filter (fun (s, p) -> s <> var) stack
                    checkStatements(ss, stack', formalParams)

                | If (e1, s1, s2, e2, pos) ->
                    checkStatements(s1, [], formalParams)
                    checkStatements(s2, [], formalParams)
                    checkStatements(ss, stack, formalParams)

                | Repeat (s, e, pos) ->
                    let stack' = checkStatements(s, [], formalParams)
                    checkStatements(ss, stack, formalParams)
                | PlusAssignment(var, e, pos) ->
                    checkLhsVar(e, var)
                | MinusAssignment(var, e, pos) ->
                    checkLhsVar(e, var)

                | _ ->  checkStatements(ss, stack, formalParams)
