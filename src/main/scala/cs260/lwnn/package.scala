package cs260

import scala.io.Source.fromFile
import cs260.lwnn.syntax.LwnnParser.getAST
import cs260.lwnn.typechecker._
import java.nio.file.Files


package object lwwn {
  def main(args: Array[String]) = args.foreach { arg => 
    val ast = getAST(fromFile(arg).mkString)
    typecheck(ast)
    println(ast)
  }
}
