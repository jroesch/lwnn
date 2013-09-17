package cs260.lwnn

import cs260.lwnn.syntax._

class PrettyPrinter(t: AST) {
  var indentationLevel = 0
  def indentation = "  " * indentationLevel
  def print: String = printTerm(t)

  def printTerm(t: AST): String = t match {
    case Program(cls) =>
      printSeq(cls, "\n\n")
    case Class(name, superClass, fields, methods) =>
      val pFields = indentBy(indent(s"fields ${ printSeq(fields.toSeq, ", ") };"))
      val pMethods = indentBy(indent(s"methods \n${ printSeq(methods.toSeq, "\n", "\n")}"))
      s"class $name extends $superClass {\n$pFields\n\n$pMethods}\n"
    case Method(methodName, params, retT, body, retE) =>
      val pBody = indentBy(printSeq(body, ";\n", ";\n"))
      val pParams = printSeq(params, ", ")
      indent(s"def $methodName($pParams): ${printType(retT)} = {\n$pBody${indentation}}")
    case Decl(Var(n), t) =>
      val pT = printType(t)
      s"$n: $pT"
    case Assign(x, e) =>
      indent(s"$x := ${printTerm(e)}")
    case Update(e1, x, e2) =>
      indent(s"${printTerm(e1)}.${printTerm(x)} := ${printTerm(e2)}")
    case Call(x, e, mn, args) =>
      indent(s"$x := $e.$mn${printSeq(args, ", ")}")
    case New(x, cn, args) =>
      indent(s"${printTerm(x)} := new $cn(${printSeq(args, ", ")})")
    case If(e, tb, fb) =>
      val pTB = indent(printSeq(tb, ";\n"))
      val pFB = indent(printSeq(fb, ";\n"))
      indent(s"if ($e) {\n$pTB\n${indentation}} else {\n$pFB${indentation}}")
    case While(e, body) =>
      val pBody = printSeq(body, ";\n")
      s"while(${printTerm(e)}) {\n$pBody\n}"
    case Print(e) =>
      s"print($e)"
    case Nums(ns) =>
      s"[${ns.mkString(", ")}]"
    case Bools(bs) =>
      s"[${bs.mkString(", ")}]"
    case Strs(ss) =>
      s"[${ss.mkString(", ")}]"
    case Nulls() =>
      "null"
    case Var(n) =>
      n
    case Access(e, x) =>
      s"${printTerm(e)}.x"
    case Binop(op, e1, e2) =>
      val pOp = op match {
        case ⌜+⌝ => "+"
        case ⌜−⌝ => "_"
        case ⌜×⌝ => "*"
        case ⌜÷⌝ => "/"
        case ⌜<⌝ => "<"
        case ⌜≤⌝ => "<="
        case ⌜∧⌝ => "&"
        case ⌜∨⌝ => "|"
        case ⌜≈⌝ => "="
        case ⌜≠⌝ => "!="
      }
      s"${printTerm(e1)} $pOp ${printTerm(e2)}"
  }

  def printSeq(xs: Seq[AST], sepBy: String, termBy: String = "") = {
    val pXS = for (x <- xs) yield printTerm(x)
    pXS.mkString(sepBy) + termBy
  }

  def printType(t: Type) = t match {
    case IntT       => "int"
    case BoolT      => "bool"
    case StrT       => "string"
    case NullT      => "null"
    case ClassT(cn) => cn
  }

  def indentBy(s: => String) = {
    indentationLevel += 1
    val result = s
    indentationLevel -= 1
    result
  }

  def indent(s: => String) = {
    indentation + s
  }
}
