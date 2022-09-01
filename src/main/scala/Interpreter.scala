package interpreter

object Interpreter:
  import Expr._
  import BinOps._
  import Definitions.defs

  def evalBinOp(op: BinOps)(ex: Expr, ey: Expr): Expr =
    (op, ex, ey) match
      case (Plus,   Constant(x), Constant(y)) => Constant(x + y)
      case (Minus,  Constant(x), Constant(y)) => Constant(x - y)
      case (Times,  Constant(x), Constant(y)) => Constant(x * y)
      case (LessEq, Constant(x), Constant(y)) => Constant(if x <= y then 1 else 0)
      case (Modulo, Constant(x), Constant(y)) => 
        if y != 0 then Constant(x % y) 
        else error("Division by zero")
      case (DividedBy, Constant(x), Constant(y)) => 
        if y != 0 then Constant(x / y) 
        else error("Division by zero")
      case _ => 
        error(s"Type error in ${show(BinOp(op, ex, ey))}")

  type DefEnv = Map[String, Expr]

  /**
    * Evaluate an expression 'e' given a set of definitions 'defs'
    * with logging the process by Logger object.
    * @param e An expression to be evaluated
    * @param defs A set of top-level definitions to be used
    * @return Expression after the evaluation of all occured definitions
    */
  def eval(e: Expr, defs: DefEnv): Expr =
    e match
      case Constant(c) => e
      case Name(n) =>
        defs.get(n) match
          case None => error(s"Unknown name $n")
          case Some(body) => eval(body, defs)
      case BinOp(op, e1, e2) =>
        evalBinOp(op)(eval(e1, defs), eval(e2, defs))
      case IfNonzero(cond, caseTrue, caseFalse) =>
        if eval(cond, defs) != Constant(0) then eval(caseTrue, defs)
        else eval(caseFalse, defs)
      case Fun(n, body) => e
      case Call(fun, arg) =>
        Logger.log(show(e))
        Logger.indent()
        val eFun = eval(fun, defs)
        val eArg = eval(arg, defs)
        eFun match
          case Fun(n, body) =>
            Logger.unindent()
            Logger.log(s"FUN: ${show(eFun)}  ARG: ${show(eArg)}")
            val bodySub = subst(body, n, eArg)
            Logger.log(s"${show(bodySub)}")
            Logger.indent()
            val res = eval(bodySub, defs)
            Logger.unindent()
            Logger.log(s"+--> ${show(res)}")
            res
          case _ => error(s"Cannot apply non-function ${show(eFun)} in a call")
      case Empty => Empty
      case Cons(x, xs) => Cons(eval(x,defs),eval(xs,defs))
      case Match(scrut, caseEmpty,h,hs,caseCons) => 
        eval(scrut,defs) match
          case Empty => eval(caseEmpty,defs)
          case Cons(x,xs) =>  eval(caseCons, defs + (h -> x,hs -> xs))
          case _ =>  error("not a list")

  /** Substitutes Name(n) by r in e. */
  def subst(e: Expr, n: String, r: Expr): Expr =
    e match
      case Constant(c) => e
      case Name(s) => if s == n then r else e
      case BinOp(op, e1, e2) =>
        BinOp(op, subst(e1, n, r), subst(e2, n, r))
      case IfNonzero(cond, trueE, falseE) =>
        IfNonzero(subst(cond, n, r), subst(trueE, n, r), subst(falseE, n, r))
      case Call(f, arg) =>
        Call(subst(f, n, r), subst(arg, n, r))
      case Fun(param, body) =>
        // If n conflicts with param, there cannot be a reference to n in the
        // function body, there is nothing so substite.
        if param == n then e
        else
          val fvs = freeVars(r)
          // If the free variables in r contain param the naive substitution would
          // change the meaning of param to reference to the function parameter.
          if fvs.contains(param) then
            // Perform alpha conversion in body to eliminate the name collision.
            val param1 = differentName(param, fvs)
            val body1 = alphaConvert(body, param, param1)
            Fun(param1, subst(body1, n, r))
          else
            // Otherwise, substitute in the function body anyway.
            Fun(param, subst(body, n, r))
      case Empty => e
      case Cons(x, xs) => Cons(subst(x, n, r),subst(xs, n, r))
      case Match(scrutinee, caseEmpty, headName, tailName, caseCons) =>
        if headName == n || tailName == n then
          // If n conflicts with headName or tailName, there cannot be a reference
          // to n in caseCons. Simply substite n by r in scrutinee and caseEmpty.
          Match(subst(scrutinee, n, r), subst(caseEmpty, n, r), headName, tailName, caseCons)
        else
          // If the free variables in r contain headName or tailName, the naive
          // substitution would change their meaning to reference to pattern binds.
          val f = freeVars(r)
          if f.contains(tailName)|| f.contains(headName) then
            val tn2 = differentName(tailName, f)
            val hn2 = differentName(headName, f)
            val cc2 = subst(subst(subst(caseCons, headName, Name(hn2)), tailName, Name(tn2)), n, r)
            Match(subst(scrutinee, n, r), subst(caseEmpty, n, r), hn2, tn2, cc2)
          // Perform alpha conversion in caseCons to eliminate the name collision.
          // Otherwise, substitute in scrutinee, caseEmpty & caseCons anyway.
          else Match(subst(scrutinee, n, r), subst(caseEmpty, n, r), headName, tailName, subst(caseCons, n, r))

  def differentName(n: String, s: Set[String]): String =
    if s.contains(n) then differentName(n + "'", s)
    else n

  /** Computes the set of free variable in e. */
  def freeVars(e: Expr): Set[String] =
    e match
      case Constant(c) => Set()
      case Name(s) => Set(s)
      case BinOp(op, e1, e2) => freeVars(e1) ++ freeVars(e2)
      case IfNonzero(cond, trueE, falseE) => freeVars(cond) ++ freeVars(trueE) ++ freeVars(falseE)
      case Call(f, arg) => freeVars(f) ++ freeVars(arg)
      case Fun(param, body) => freeVars(body) - param
      case Empty => Set()
      case Cons(x, xs) => freeVars(x) ++ freeVars(xs)
      case Match(s, cE, _, _, cC) => if s == Empty then freeVars(cE) else freeVars(cC)

  /** Substitutes Name(n) by Name(m) in e. */
  def alphaConvert(e: Expr, n: String, m: String): Expr =
    e match
      case Constant(c) => e
      case Name(s) => if s == n then Name(m) else e
      case BinOp(op, e1, e2) =>
        BinOp(op, alphaConvert(e1, n, m), alphaConvert(e2, n, m))
      case IfNonzero(cond, trueE, falseE) =>
        IfNonzero(alphaConvert(cond, n, m), alphaConvert(trueE, n, m), alphaConvert(falseE, n, m))
      case Call(f, arg) =>
        Call(alphaConvert(f, n, m), alphaConvert(arg, n, m))
      case Fun(param, body) =>
        // If n conflicts with param, there cannot be references to n in body,
        // as these would reference param instead.
        if param == n then e
        else Fun(param, alphaConvert(body, n, m))
      case Empty => e
      case Cons(x, xs) => Cons(alphaConvert(x, n, m), alphaConvert(xs, n, m))
      case Match(s, cE, x, xs, cC) => 
        if xs != n && x != n
        then Match(alphaConvert(s, n, m), alphaConvert(cE, n, m), x, xs, alphaConvert(cC, n, m))
        else Match(alphaConvert(s ,n, m), alphaConvert(cE, n, m), x, xs, cC)

  case class EvalException(msg: String) extends Exception(msg)

  def error(msg: String) = throw EvalException(msg)

  // Printing and displaying

  /** Pretty print an expression as a String. */
  def show(e: Expr): String =
    e match
      case Constant(c) => c.toString
      case Name(n) => n
      case BinOp(op, e1, e2) =>
        val opString = op match
          case Plus      => "+"
          case Minus     => "-"
          case Times     => "*"
          case DividedBy => "/"
          case Modulo    => "%"
          case LessEq    => "<="
        s"($opString ${show(e1)} ${show(e2)})"
      case IfNonzero(cond, caseTrue, caseFalse) =>
        s"(if ${show(cond)} then ${show(caseTrue)} else ${show(caseFalse)})"
      case Call(f, arg) => show(f) + "(" + show(arg) + ")"
      case Fun(n, body) => s"($n => ${show(body)})"
      case Empty => s" Nil "
      case Cons(h,t) => s"${show(h)} :: ${show(t)}"
      case Match(_, _, _, _, _) => error("'case Match' in 'show' call error")

  /** Pretty print top-level definition as a String. */
  def showEnv(env: Map[String, Expr]): String =
    env.map { case (name, body) => s"def $name =\n  ${show(body)}" }.mkString("\n\n") + "\n"

  /** Evaluates an expression with the given top-level definitions with logging enabled. */
  def tracingEval(e: Expr, defs: DefEnv): Expr =
    Logger.on()
    val evaluated = eval(e, defs)
    println(s" ~~> $evaluated\n")
    Logger.off()
    evaluated

  // def minus(e1: Expr, e2: Expr)     = BinOp(BinOps.Minus,  e1, e2)
  // def plus(e1: Expr, e2: Expr)      = BinOp(BinOps.Plus,   e1, e2)
  // def leq(e1: Expr, e2: Expr)       = BinOp(BinOps.LessEq, e1, e2)
  // def times(e1: Expr, e2: Expr)     = BinOp(BinOps.Times,  e1, e2)
  // def modulo(e1: Expr, e2: Expr)    = BinOp(BinOps.Modulo, e1, e2)
  // def dividedBy(e1: Expr, e2: Expr) = BinOp(BinOps.DividedBy, e1, e2)

end Interpreter