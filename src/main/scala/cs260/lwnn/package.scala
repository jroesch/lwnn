package cs260

import scala.io.Source.fromFile
import cs260.lwnn.syntax.LwnnParser.getAST
import java.nio.file.Files


package object lwwn {
  def main(args: Array[String]) = args.foreach { arg => println(getAST(fromFile(arg).mkString)) }
}