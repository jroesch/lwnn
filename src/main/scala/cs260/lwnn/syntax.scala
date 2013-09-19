package cs260.lwnn.syntax

//——————————————————————————————————————————————————————————————————————————————
// Convenient type aliases

object TypeAliases {
  type ClassName = String
  type MethodName = String
}

import TypeAliases._
import cs260.lwnn.typechecker._

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

case class Program( classes:Seq[Class] ) extends AST

case class Class( cn:ClassName, supercn:ClassName, fields:Set[Decl],
                  methods:Set[Method] ) extends AST

case class Method( mn:MethodName, params:Seq[Decl], τret:Type,
                   body:Seq[Stmt], rete:Exp ) extends AST

case class Decl( x:Var, τ:Type ) extends AST

//——————————————————————————————————————————————————————————————————————————————
// Types

sealed abstract class Type {
  // the subtyping operator (Scala won't allow the more usual <: notation)
  def ⊑ ( τ:Type )(implicit gamma: TypeEnv): Boolean
  def subtypeOf(t: Type)(implicit gamma: TypeEnv): Boolean = ⊑(t)
}

case object IntT extends Type {
  // IntT isn't a subtype of anything but itself
  def ⊑ ( τ:Type )(implicit gamma: TypeEnv) =
    τ match {
      case IntT ⇒ true
      case _ ⇒ false
    }
}

case object BoolT extends Type {
  // BoolT isn't a subtype of anything but itself
  def ⊑ ( τ:Type )(implicit gamma: TypeEnv) =
    τ match {
      case BoolT ⇒ true
      case _ ⇒ false
    }
}

case object StrT extends Type {
  // StrT isn't a subtype of anything but itself
  def ⊑ ( τ:Type )(implicit gamma: TypeEnv) =
    τ match {
      case StrT ⇒ true
      case _ ⇒ false
    }
}

case object NullT extends Type {
  // NullT is a subtype of itself and all classes
  def ⊑ ( τ:Type )(implicit gamma: TypeEnv) =
    τ match {
      case NullT | _:ClassT ⇒ true
      case _ ⇒ false
    }
}

case class ClassT(cn: ClassName) extends Type {
  // the subtyping of classes depends on the user's program
  def ⊑ ( τ:Type )(implicit gamma: TypeEnv) = τ match {
    case ClassT("TopClass") => true
    case ClassT(_) if cn == "TopClass" => false
    case o @ ClassT(on) =>
      val tpe = gamma.classFor(this)
      val other = gamma.classFor(o)
      println(tpe, other)
      if (tpe.selfType == other.selfType || tpe.superType == other.selfType)
        true
      else ClassT(tpe.superType).subtypeOf(o)
    case _          => false
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Stmt
//
// We include a Print statement not in the semantics, which prints out
// the value of an expression; this is used to help debug the
// interpreters by inspecting the values they compute.

sealed abstract class Stmt extends AST
case class Assign( x:Var, e:Exp ) extends Stmt
case class Update( e1:Exp, x:Var, e2:Exp ) extends Stmt
case class Call( x:Var, e:Exp, mn:MethodName, args:Seq[Exp] ) extends Stmt
case class New( x:Var, cn:ClassName, args:Seq[Exp] ) extends Stmt
case class If( e:Exp, tb:Seq[Stmt], fb:Seq[Stmt] ) extends Stmt
case class While( e:Exp, body:Seq[Stmt] ) extends Stmt
case class Print( e:Exp ) extends Stmt

//——————————————————————————————————————————————————————————————————————————————
// Exp, BinaryOp

sealed abstract class Exp extends AST
case class Nums( ns:Set[BigInt] ) extends Exp
case class Bools( bs:Set[Boolean] ) extends Exp
case class Strs( strs:Set[String] ) extends Exp
case class Nulls() extends Exp
case class Var( name:String ) extends Exp
case class Access( e:Exp, x:Var ) extends Exp
case class Binop( op:BinaryOp, e1:Exp, e2:Exp ) extends Exp

sealed abstract class BinaryOp
case object ⌜+⌝ extends BinaryOp
case object ⌜−⌝ extends BinaryOp
case object ⌜×⌝ extends BinaryOp
case object ⌜÷⌝ extends BinaryOp
case object ⌜<⌝ extends BinaryOp
case object ⌜≤⌝ extends BinaryOp
case object ⌜∧⌝ extends BinaryOp
case object ⌜∨⌝ extends BinaryOp
case object ⌜≈⌝ extends BinaryOp
case object ⌜≠⌝ extends BinaryOp

/* Parser */
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.collection.mutable.{ Map => MMap }
import cs260.lwnn._

object LwnnParser extends StandardTokenParsers with PackratParsers {

  type Parser[T] = PackratParser[T]

  var currentClass: Type = ClassT("TopClass")
  val classes = MMap[ClassT, ClassTypes]()

  // reserved keywords
  lexical.reserved ++= Seq("class", "extends", "fields", "methods",
    "def", "return", "new", "if", "else", "while", "int", "bool", "string", "null",
     "true", "false")

  lexical.delimiters ++= Seq( "+", "-", "*", "/", "&", "|", "=", "!=",
			 "<", "<=", "{", "}", "(", ")", ":=", ";",
			 ",", "[", "]", ".", ":", "..", "=>", "##" )

  def getAST(source: String): Either[String, AST] = {
    // strip out comments
    val commentR = """##((#?[^#]+)*)##""".r
    val cleanSource = commentR.replaceAllIn(source, "" )

    // parse the program
    val lexer = new lexical.Scanner(cleanSource)
    val result = phrase(program)(lexer)

    // return result or a useful error message
    result match {
      case Success(ast,_) => Right(ast)
      case NoSuccess(msg,next) =>
        Left(s"Parse error: $msg\n" +
        s"At line: ${next.pos.line} column: ${next.pos.column}\n" +
        next.pos.longString)
    }
  }

  lazy val program: Parser[Program] = rep(classP) ^^ { cs => Program(cs) }

  lazy val classP: Parser[Class] =
    "class" ~> classNameDef ~ opt("extends" ~> className) ~ "{" ~ classBody ~ "}" ^^ {
      case name ~ None ~ "{" ~ body ~ "}" =>
        Class(name, "TopClass", body._1, body._2)
      case name ~ Some(superClass) ~ "{" ~ body ~ "}" =>
        Class(name, superClass, body._1, body._2)
    }


  lazy val classBody: Parser[(Set[Decl], Set[Method])] =
    opt("fields" ~> rep1sep(fieldP, ",") <~ ";") ~ opt("methods" ~> rep1(methodP)) ^^ {
      case Some(fields) ~ Some(methods) => (fields.toSet, methods.toSet)
      case Some(fields) ~ None          => (fields.toSet, Set.empty[Method])
      case None ~ Some(methods)         => (Set.empty[Decl], methods.toSet)
      case None ~ None                  => (Set.empty[Decl], Set.empty[Method])
    }

  lazy val fieldP: Parser[Decl] = variable ~ ":" ~ typeP ^^ {
    case x ~ _ ~ t => Decl(x, t)
  }

  lazy val typeP = (
      "int"     ^^ (_ => IntT)
    | "bool"    ^^ (_ => BoolT)
    | "string"  ^^ (_ => StrT)
    | "null"    ^^ (_ => NullT)
    | className ^^ (cn => ClassT(cn))
  )

  lazy val methodP: Parser[Method] =
  "def" ~ methodName ~ ("(" ~> paramList <~ ")") ~ opt(":" ~> typeP) ~ "=" ~ ("{" ~> methodBody <~ "}") ^^ {
    case _ ~ mname ~ params ~ retType ~ _ ~ ((body, ret)) => retType match {
      case None => Method(mname, params, currentClass, body, ret)
      case Some(tpe) => Method(mname, params, tpe, body, ret)
    }
  }

  lazy val paramList: Parser[Seq[Decl]]=
    repsep(variable ~ ":" ~ typeP ^^ { case x ~ _ ~ t => Decl(x, t) }, ",")

  lazy val methodBody: Parser[(List[Stmt], Exp)] = opt(stmtSeq) ~ opt(("return" ~> expP) <~ ";") ^^ {
    case Some(stmts) ~ Some(ret) => stmts -> ret
    case Some(stmts) ~ None      => stmts -> Var("self")
    case None ~ Some(ret)        => Nil   -> ret
    case None ~ None             => Nil   -> Var("self")
  }

  lazy val stmtSeq: Parser[List[Stmt]] = rep1sep(stmtP, ";") <~ ";"

  lazy val stmtP: Parser[Stmt] = (
      update
    | newClass
    | methodCall
    | assign
    | ifStmt
    | whileStmt
  )

  lazy val assign: Parser[Stmt] = variable ~ ":=" ~ expP ^^ {
    case v ~ _ ~ e => Assign(v, e)
  }

  lazy val update: Parser[Stmt] = expP ~ "." ~ variable ~ ":=" ~ expP ^^ {
    case obj ~ _ ~ field ~ _ ~ value => Update(obj, field, value)
  }

  lazy val newClass: Parser[Stmt] = variable ~ ":=" ~ "new" ~ className ~ argList ^^ {
    case v ~ _ ~ _ ~ cls ~ params => New(v, cls, params)
  }

  lazy val methodCall: Parser[Stmt] = variable ~ ":=" ~ (variable | expP) ~ "." ~ methodName ~ argList ^^ {
    case v ~ _ ~ obj ~ _ ~ mname ~ params => Call(v, obj, mname, params)
  }

  lazy val ifStmt: Parser[Stmt] = "if" ~ ("(" ~> expP <~ ")") ~ block ~ opt("else" ~>  block) ^^ {
    case _ ~ e ~ tbranch ~ ofbranch => ofbranch match {
      case None => If(e, tbranch, Seq())
      case Some(fbranch) => If(e, tbranch, fbranch)
    }
  }

  lazy val whileStmt: Parser[Stmt] = "while" ~ expP ~ block ^^ {
    case _ ~ guard ~ body => While(guard, body)
  }

  lazy val block = "{" ~> stmtSeq <~ "}"

  lazy val argList = "(" ~> repsep(expP, ",") <~ ")"

  lazy val expP: Parser[Exp] = (
      E ~ binOpP ~ expP ^^ { case e1 ~ op ~ e2 => Binop(op, e1, e2) }
    | E
  )

  lazy val E: Parser[Exp] = (
      value
    | variable
    | expP ~ "." ~ variable ^^ { case e ~ _ ~ x => Access(e, x) }
    | "(" ~> expP <~ ")"
    )


  /* need to do non-determinism here */
  lazy val value: Parser[Exp] = (
      int ^^ (d => Nums(Set(BigInt(d))))
    | bool ^^ (b => Bools(Set(b)))
    | string ^^ (s => Strs(Set(s)))
    | "null" ^^ (_ => Nulls())
    //| "[" ~> (value ~ ".." ~ value) <~ "]" ^^ question for Ben
  )

  lazy val binOpP = (
      "+"  ^^^ ⌜+⌝
    | "_"  ^^^ ⌜−⌝
    | "*"  ^^^ ⌜×⌝
    | "/"  ^^^ ⌜÷⌝
    | "<"  ^^^ ⌜<⌝
    | "<=" ^^^ ⌜≤⌝
    | "&"  ^^^ ⌜∧⌝
    | "|"  ^^^ ⌜∨⌝
    | "="  ^^^ ⌜≈⌝
    | "!=" ^^^ ⌜≠⌝
  )

  lazy val int: Parser[String] = numericLit

  lazy val bool = "true" ^^^ true | "false" ^^^ false

  lazy val string = stringLit

  lazy val variable: Parser[Var] = ident ^^ (v => Var(v))

  lazy val className = ident

  lazy val classNameDef: Parser[String] = ident >> { (s: String) =>
    currentClass = ClassT(s); success(s)
  }

  lazy val methodName = ident
}
