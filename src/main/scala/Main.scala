package interpreter
import scala.io.{StdIn => In}

@main def main =
  print("lisp>")
  val cmd = In.readLine()
  if cmd == "exit" then
    println("Exiting Lisp interpreter")
  else
    try
      println(Lisp.evaluate(Lisp.string2lisp(cmd)))
    catch
      case e: Exception => 
        try
          println(Lisp.evaluate(LispCode.withDifferences(cmd)))
        catch
          case e: Exception => println("Command not recognized")
    main(args)
