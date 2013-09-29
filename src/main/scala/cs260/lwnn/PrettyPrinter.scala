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
      val pMethods = indentBy(printSeq(methods.toSeq, "\n\n"))
      s"class $name extends $superClass ${blockIndent(s"$pFields\n\n$pMethods")}"
    case Method(methodName, params, retT, body, retE) =>
      val pBody = indentBy(printSeq(body, ";\n", ";\n"))
      val pRet = indentBy(indent(s"return ${ printTerm(retE) };"))
      val pParams = printSeq(params, ", ")
      indent(s"def $methodName($pParams): ${printType(retT)} = ${blockIndent(pBody + pRet)}")
    case Decl(Var(n), t) =>
      val pT = printType(t)
      s"$n: $pT"
    case Assign(x, e) =>
      indent(s"${printTerm(x)} := ${printTerm(e)}")
    case Update(e1, x, e2) =>
      indent(s"${printTerm(e1)}.${printTerm(x)} := ${printTerm(e2)}")
    case Call(x, e, mn, args) =>
      indent(s"${printTerm(x)} := ${printTerm(e)}.$mn(${printSeq(args, ", ")})")
    case New(x, cn, args) =>
      indent(s"${printTerm(x)} := new $cn(${printSeq(args, ", ")})")
    case If(e, tb, fb) =>
      val pGuard = printTerm(e)
      val pTB = indentBy(printSeq(tb, ";\n", ";"))
      val pFB = indentBy(printSeq(fb, ";\n", ";"))
      indent(s"if ($pGuard) ${blockIndent(pTB)} else ${blockIndent(pFB)}")
    case While(e, body) =>
      val pBody = printSeq(body, ";\n")
      s"while(${printTerm(e)}) {\n$pBody\n}"
    case Print(e) =>
      s"print($e)"
    case Nums(ns) =>
      s"{ ${ns.mkString(", ")} }"
    case Bools(bs) =>
      s"{ ${bs.mkString(", ")} }"
    case Strs(ss) =>
      s"{ ${ss.map('"' + _ + '"').mkString(", ")} }"
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
    if (pXS.isEmpty) ""
    else pXS.mkString(sepBy) + termBy
  }

  def printType(t: Type) = t match {
    case IntT       => colorRed("int")
    case BoolT      => colorRed("bool")
    case StrT       => colorRed("string")
    case NullT      => colorRed("null")
    case ClassT(cn) => colorRed(cn)
    case SyntheticType =>
      colorRed("SyntheticType")
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

  def blockIndent(s: => String) = {
    s"{\n$s\n$indentation}"
  }

  def colorRed(s: String) = {
    s"\033[31m$s\033[0m"
  }

  def colorBlue(s: String) = {
    s"\033[34m$s\033[0m"
  }
}
