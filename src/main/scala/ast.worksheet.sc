// package interpreter

enum BinOps:
    case Plus, Minus, Times, Power, LessEq

enum Expr:
    case C(c: BigInt)
    case N(name: String)
    case BinOp(op: BinOps, e1: Expr, e2: Expr)
    case IfNonzero(cond: Expr, trueE: Expr, falseE: Expr)
    case Call(function: String, args: List[Expr])
    // case AbsValue(arg: Expr)

case class Function(params: List[String], body: Expr)
type DefEnv = Map[String, Function]

import BinOps.*
import Expr.*

val defs: DefEnv = Map[String, Function](
    "fact" -> Function(List("n"),
        IfNonzero(N("n"),
            BinOp(Times, N("n"),
                Call("fact", List(BinOp(Minus, N("n"), C(1))))),
            C(1)))
)

// Printing
def str(e: Expr): String = e match
    case C(c) => 
        c.toString
    case BinOp(op, e1, e2) =>
        s"(${strOp(op)} ${str(e1)} ${str(e2)})"
    case IfNonzero(cond, trueE, falseE) =>
        s"(if ${str(cond)} then ${str(trueE)} else ${str(falseE)})"
    case _ => throw new Exception

def strOp(op: BinOps): String = op match
    case Plus   => "+"
    case Minus  => "-"
    case Times  => "*"
    case Power  => "^"
    case LessEq => "<="

str(IfNonzero(BinOp(LessEq, C(4),C(50)), C(10), C(20)))

// Interpreting
def evalBinOp(op: BinOps)(x: BigInt, y: BigInt): BigInt = op match
    case Plus   => x + y
    case Minus  => x - y
    case Times  => x * y
    case Power  => x.pow(y.toInt)
    case LessEq => if x <= y then 1 else 0

def eval(e: Expr): BigInt = e match
    case C(c) => c
    case N(n) => 
        throw new RuntimeException(s"Unknown name '$n'")
    case BinOp(op, e1, e2) => 
        evalBinOp(op)(eval(e1), eval(e2))
    case IfNonzero(cond, trueE, falseE) =>
        if eval(cond) != 0 then eval(trueE) 
        else eval(falseE)
    case Call(func, args) =>
        defs.get(func) match
            case Some(f) =>
                val evalArgs = args.map(e => C(eval(e)))
                val bodySub = substAll(f.body, f.params, evalArgs)
                eval(bodySub)
            case None =>
                throw new RuntimeException(s"Unknown function '$func'")

eval(IfNonzero(BinOp(LessEq, C(4), C(50)), C(10), C(20)))

// Substitution
def subst(e: Expr, n: String, r: Expr): Expr = e match
    case C(_) => e
    case N(s) => if s == n then r else e
    case BinOp(op, e1, e2) =>
         BinOp(op, subst(e1, n, r), subst(e2, n, r))
    case IfNonzero(c, trueE, falseE) =>
         IfNonzero(subst(c, n, r), subst(trueE, n, r), subst(falseE, n, r))
    case Call(func, args) =>
         Call(func, args.map(subst(_, n, r)))

def substAll(e: Expr, names: List[String], replacements: List[Expr]): Expr =
    (names, replacements) match
        case (n :: ns, r :: rs) => substAll(subst(e, n, r), ns, rs)
        case _ => e
    


def desugar(e: Expr): Expr = e match
    case C(_) => e
    case BinOp(op, e1, e2) =>
        BinOp(op, desugar(e1), desugar(e2))
    case IfNonzero(cond, trueE, falseE) =>
        IfNonzero(desugar(cond), desugar(trueE), desugar(falseE))
    case _ => throw new Exception
    // case AbsValue(arg) =>
    //     val x = desugar(arg)
    //     IfNonzero(BinOp(LessEq, x, C(0)), BinOp(Minus, C(0), x), x)

