package interpreter

/** Abstract Syntax Tree (AST) of Expressions */
enum Expr:
  case Constant(value: Int)
  case Name(name: String)
  case BinOp(op: BinOps, arg1: Expr, arg2: Expr)
  case IfNonzero(cond: Expr, caseTrue: Expr, caseFalse: Expr)
  case Call(function: Expr, arg: Expr)
  case Fun(param: String, body: Expr)
  case Empty                         /** The empty list, i.e. nil */
  case Cons(head: Expr, tail: Expr)  /** The Cons list type of a head and a tail */
  case Match(scrutinee: Expr, caseEmpty: Expr, 
             headName: String, tailName: String, caseCons: Expr)
  /** Pattern matching for Empty and Cons */

  /** Primitive operations that operation on constant values. */
enum BinOps:
  case Plus, Minus, Times, DividedBy, Modulo, LessEq
