package interpreter

class LispTokenizer(s: String) extends Iterator[String] {
  type Data = Any
  private var i = 0
  private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')'

  def hasNext: Boolean =
    while (i < s.length && s.charAt(i) <= ' ') do
      i += 1
    i < s.length

  def next: String =
    if hasNext then
      val start = i
      if isDelimiter(s.charAt(i)) then i += 1
      else
        i += 1
        while (i < s.length && !isDelimiter(s.charAt(i))) do
          i += 1
      s.substring(start, i)
    else sys.error("premature end of input")
  }

  def string2lisp(s: String): Data = {
    val it = new LispTokenizer(s)

    def parseExpr(token: String): Data = {
      if (token == "(")
        parseList
      else if (token == ")")
        sys.error("unbalanced parentheses")
      else if (token.matches("^-?\\d+$"))
        Integer.parseInt(token)
      else
        Symbol(token)
    }

    def parseList: List[Data] = {
      val token = it.next
      if (token == ")")
        List()
      else
        parseExpr(token) :: parseList
    }
    parseExpr(it.next)
  }

object Parser:
  def parse(data: Any, defs: DefEnv) =
    