package cs260

import scala.io.Source.fromFile
import lwnn.syntax._
import cs260.lwnn.syntax.LwnnParser.getAST
import cs260.lwnn.typechecker._

package object lwnn {
  def pprint(t: AST) = println((new cs260.lwnn.PrettyPrinter(t)).print)

  def main(args: Array[String]) = args.foreach { arg => 
    getAST(fromFile(arg).mkString) match {
      case Left(err) => println(err)
      case Right(program) =>
        val (classTable, ast) = program
        try { typecheck(ast, classTable) }
        catch {
          case i: Illtyped => println(s"TypeError: ${i.msg}")
        }
        pprint(ast)
    }
  }
}
