package interpreter

object Definitions:
  import Expr.{Name => N, Constant => C, _}

  def minus(e1: Expr, e2: Expr)     = BinOp(BinOps.Minus,  e1, e2)
  def plus(e1: Expr, e2: Expr)      = BinOp(BinOps.Plus,   e1, e2)
  def leq(e1: Expr, e2: Expr)       = BinOp(BinOps.LessEq, e1, e2)
  def times(e1: Expr, e2: Expr)     = BinOp(BinOps.Times,  e1, e2)
  def modulo(e1: Expr, e2: Expr)    = BinOp(BinOps.Modulo, e1, e2)
  def dividedBy(e1: Expr, e2: Expr) = BinOp(BinOps.DividedBy, e1, e2)

  val defs: DefEnv = Map[String, Expr](
    "+" -> Fun("a", Fun("b", plus(N("a"), N("b")))),

    "-" -> Fun("a", Fun("b", minus(N("a"), N("b")))),

    "*" -> Fun("a", Fun("b", times(N("a"), N("b")))),

    "/" -> Fun("a", Fun("b", dividedBy(N("a"), N("b")))),

    "<" -> Fun("a", Fun("b", leq(N("a"), N("b")))),

    ">" -> Fun("a", Fun("b", leq(N("b"), N("a")))),

    "fact" -> Fun("n",
      IfNonzero(N("n"),
        times(N("n"),
             Call(N("fact"), minus(N("n"), C(1)))),
        C(1))),

    "square" -> Fun("x", 
      times(N("x"), N("x"))),

    "twice" -> Fun("f", Fun("x", 
      Call(N("f"), Call(N("f"), N("x"))))),

    "map" -> Fun("ls", Fun("f", 
      Match(N("ls"),
          Empty,
        "x", "xs", 
          Cons(
            Call(N("f"), N("x")),
            Call(Call(N("map"), N("xs")), N("f")))))),

    "gcd" -> Fun("a", Fun("b",
      IfNonzero(N("b"), 
        Call(Call(N("gcd"), N("b")), modulo(N("a"), N("b"))), 
        N("a")))),

    "foldLeft" -> Fun("ls", Fun("acc", Fun("fold",
      Match(N("ls"),
          N("acc"),
        "x", "xs",
          Call(
            Call(
              Call(N("foldLeft"), N("xs")),
              Call(Call(N("fold"), N("acc")), N("x"))),
            N("fold")))))),

    "foldRight" -> Fun("ls", Fun("z", Fun("fold",
      Match(N("ls"), 
          N("z"),
        "x", "xs",
          Call(
            Call(N("fold"), N("x")), 
            Call(
              Call(Call(N("foldRight"), N("xs")), N("z")), 
              N("fold"))))))),
  )

end Definitions