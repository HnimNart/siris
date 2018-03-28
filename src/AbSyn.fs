module AbSyn


(* position: (line, column) *)
type Position = int * int

(* Allowed values *)
type Value =
    IntVal of int

(* Bool and Int are represented the same way *)
type Type =
    Int
  | Bool


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

type Statement<'T> =
    PlusAssignment of string * Exp<'T> * Position
  | MinusAssignment of string * Exp<'T> * Position
  | If of Exp<'T> * Statement<'T> List * Statement<'T> List * Exp<'T> * Position
  | Repeat of Statement<'T> List * Exp<'T> * Position
  | Call of string * Position
  | Uncall of string * Position
  | Print of string * Position
  | Read of string * Position


type ProcDec<'T> =
    ProcDec of string * Statement<'T> List * Position

let getProcName (ProcDec(name, _ , _)) = name
let getProcStat (ProcDec(_, stat, _))  = stat
let getProcPos  (ProcDec(_, _ , pos))  = pos

type Prog<'T> =
    Statement<'T> List * ProcDec<'T> list


type UntypedProg = Prog<unit>
type UntypedProcDec = ProcDec<unit>
type UntypedExp = Exp<unit>
type UntypedStatement = Statement<unit>
