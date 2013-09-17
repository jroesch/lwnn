package cs260

import scala.io.Source.fromFile
import lwnn.syntax._
import cs260.lwnn.syntax.LwnnParser.getAST
import cs260.lwnn.typechecker._
import java.nio.file.Files


package object lwwn {
  def pprint(t: AST) = (new cs260.lwnn.PrettyPrinter(t)).print

  def main(args: Array[String]) = args.foreach { arg => 
    val ast = getAST(fromFile(arg).mkString)
    typecheck(ast)
    println(pprint(ast))
  }
}
