package cs260.lwnn.syntax

//——————————————————————————————————————————————————————————————————————————————
// AST base class

sealed abstract class AST {
  val id = AST.id() // unique node identifier
}

object AST {
  // create unique node identifiers
  var genID = 0
  def id(): Int = { genID += 1; genID-1 }
}

//——————————————————————————————————————————————————————————————————————————————
// Program, Class, Method, Decl

case class Program( classes:Set[Class] ) extends AST

case class Class( name:String, superclass:String, fields:Set[Decl],
  methods:Set[Method] ) extends AST

case class Method( name:String, params:Seq[Decl], τret:Type,
  body:Seq[Stmt] ) extends AST

case class Decl( x:Var, τ:Type ) extends AST

//——————————————————————————————————————————————————————————————————————————————
// Types

sealed abstract class Type {
  // the subtyping operator (Scala won't allow the more usual <: notation)
  def ⊑ ( τ:Type ): Boolean
}

case object IntT extends Type {
  // IntT isn't a subtype of anything but itself
  def ⊑ ( τ:Type ) =
    τ match {
      case IntT ⇒ true
      case _ ⇒ false
    }
}

case object BoolT extends Type {
  // BoolT isn't a subtype of anything but itself
  def ⊑ ( τ:Type ) =
    τ match {
      case BoolT ⇒ true
      case _ ⇒ false
    }
}

case object StrT extends Type {
  // StrT isn't a subtype of anything but itself
  def ⊑ ( τ:Type ) =
    τ match {
      case StrT ⇒ true
      case _ ⇒ false
    }
}

case object NullT extends Type {
  // NullT is a subtype of itself and all classes
  def ⊑ ( τ:Type ) =
    τ match {
      case NullT | _:ClassT ⇒ true
      case _ ⇒ false
    }
}

case class ClassT( name:String ) extends Type {
  // the subtyping of classes depends on the user's program
  def ⊑ ( τ:Type ) =
    sys.error("!!TODO: define subtyping on classes")
}

//——————————————————————————————————————————————————————————————————————————————
// Stmt
//
// we include Return as a Stmt for convenience; the parser will ensure
// it only shows up at the end of a method.

sealed abstract class Stmt extends AST
case class Assign( x:Var, e:Exp ) extends Stmt
case class Update( e1:Exp, x:Var, e2:Exp ) extends Stmt
case class Call( x:Var, e:Exp, mn:String, args:Seq[Exp] ) extends Stmt
case class New( x:Var, cn:String, args:Seq[Exp] ) extends Stmt
case class If( e:Exp, tb:Seq[Stmt], fb:Seq[Stmt] ) extends Stmt
case class While( e:Exp, body:Seq[Stmt] ) extends Stmt
case class Return( e:Exp ) extends Stmt

//——————————————————————————————————————————————————————————————————————————————
// Exp, BinaryOp

sealed abstract class Exp extends AST
case class Nums( ns:Set[BigInt] ) extends Exp
case class Bools( bs:Set[Boolean] ) extends Exp
case class Strs( strs:Set[String] ) extends Exp
case class Null() extends Exp
case class Var( name:String ) extends Exp
case class Access( e:Exp, x:Var ) extends Exp
case class Binop( op:BinaryOp, e1:Exp, e2:Exp ) extends Exp

sealed abstract class BinaryOp
case object ⌜+⌝ extends Bop
case object ⌜−⌝ extends Bop
case object ⌜×⌝ extends Bop
case object ⌜÷⌝ extends Bop
case object ⌜<⌝ extends Bop
case object ⌜≤⌝ extends Bop
case object ⌜∧⌝ extends Bop
case object ⌜∨⌝ extends Bop
case object ⌜=⌝ extends Bop
case object ⌜≠⌝ extends Bop

/* Parser */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object LwnnParser extends StandardTokenParsers with PackratParsers {
  type Parser[T] = PackratParser[T]
  
  // reserved keywords
  lexical.reserved += ("class", "extends", "fields", "methods",
    "def", "return", "new", "if", "else", "while", "int", "bool", "string", "null")

  lexical.delimiters += ( "+", "-", "*", "/", "&", "|", "=", "!=",
			 "<", "<=", "{", "}", "(", ")", ":=", ";",
			 ",", "[", "]", ".", ":", "..", "=>", "##" )

  def getAST(program: String) = {
    // strip out comments
    val commentR = """##((#?[^#]+)*)##""".r
    val prog = commentR.replaceAllIn( program, "" )

    // parse the program
    val lexer = new lexical.Scanner(prog)
    val result = phrase(program)(lexer)
    
    // return result or a useful error message
    result match {
      case Success(ast,_) => 
        ast

      case NoSuccess(msg,next) => { 
        println("Parse error: " + msg)
        println("At line " + next.pos.line + ", column " + next.pos.column)
        println(next.pos.longString)
        sys.exit(1) 
      }
    }
  }

  lazy val program = sep(classP)

  lazy val classP = 
    "class" ~> className ~ opt("extends" ~> className) ~ "{" ~ classBody ~ "}" ^^ {
      case name ~ None ~ "{" ~ (fields, methods) ~ "}" => 
        Class(className, "Object", fields, methods)
      case name ~ Some(superClass) ~ "{" ~ (fields, methods) ~ "}" =>
        Class(className, superClass, fields, methods)

  lazy val classBody = (Set(), Set())

  lazy val typeP = "int" | "bool" | "string" | "null" | className

  lazy val methodP = ???

  lazy val stmtP = ???

  lazy val expP = ???

  lazy val binOpP = ???
  
  lazy val int = """(0|[1-9][0-9]*])"""r

  lazy val bool = "true" | "false"

  lazy val string = stringLit

  lazy val variable = ident

  lazy val className = ident

  lazy val methodName = ident
