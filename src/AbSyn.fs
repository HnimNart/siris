module AbSyn

(* position: (line, column) *)
type Position = int * int

(* Allowed values *)
type Value =
    IntVal of int

// a parameter is just a variable name
type Param =
    Param of string

let getStringOfParam (Param(s)) = s

type Declaration<'T> =
        IntVar of string  * Position

let getStringOfIntVar (IntVar(s,_)) = s
let getPosOfIntVar (IntVar(_,p)) = p

type Exp<'T> =
    Constant of Value * Position
  | Var of string * Position
  | Plus of Exp<'T> * Exp<'T> * Position
  | Minus of Exp<'T> * Exp<'T> * Position
  | Divide of Exp<'T> * Exp<'T> * Position
  | Times of Exp<'T> * Exp<'T> * Position
  | Modulo of Exp<'T> * Exp<'T> * Position
  | Less of Exp<'T> * Exp<'T> * Position
  | Equal of Exp<'T> * Exp<'T> * Position
  | Or of Exp<'T> * Exp<'T> * Position
  | And of Exp<'T> * Exp<'T> * Position
  | Not of Exp<'T> * Position

type Statement<'T> =
    PlusAssignment of string * Exp<'T> * Position
  | MinusAssignment of string * Exp<'T> * Position
  | If of Exp<'T> * Statement<'T> List * Statement<'T> List * Exp<'T> * Position
  | Repeat of Statement<'T> List * Exp<'T> * Position
  | Call of string * Param List *  Position
  | Uncall of string * Param List *  Position
  | Print of string * Position
  | Read of string * Position
  | Local of string * Exp<'T> * Position
  | Delocal of string * Exp<'T> * Position
  | Swap of string * string * Position

type ProcDec<'T> =
    ProcDec of string * Param List * Statement<'T> List * Position

let getProcName (ProcDec(name, _ , _ ,_)) = name
let getProcParam (ProcDec(_, param ,  _ ,_))  = param
let getProcStat (ProcDec(_ ,  _ , stat, _))  = stat
let getProcPos  (ProcDec(_, _ ,_ , pos))  = pos

type Prog<'T> =
    Declaration<'T> List * Statement<'T> List * ProcDec<'T> list

type Declaration = Declaration<unit>
type Program = Prog<unit>
type ProcDec = ProcDec<unit>
type Exp = Exp<unit>
type Statement = Statement<unit>
