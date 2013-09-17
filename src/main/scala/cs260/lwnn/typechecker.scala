package cs260.lwnn

import cs260.lwnn.syntax._
import scala.collection.mutable.{ Map => MMap }

case class Illtyped(msg: String) extends Exception(msg)

object typechecker {
  /* A helper container for types related to classes (constructor, field, method, super type) */
  case class ClassTypes(clazz: Class) {

    val fieldTable = Map(
      (clazz match {
        case Class(_, _, fields, _) =>
          for (field <- fields) yield field.x -> field.τ
      }).toSeq: _*
    )

    val methodTable = Map(
      (clazz match {
        case Class(_, _, _, methods)  =>
          for (method <- methods) yield {
            method.mn -> MethodType(method.τret, method.params.map { _.τ })
          }
      }).toSeq: _ *
    )

    val constructor = methodTable.get(clazz.cn).
      getOrElse(MethodType(ClassT(clazz.cn), Nil))

    case class MethodType(returnT: Type, argTs: Seq[Type])

    def field(x: Var) = fieldTable(x)
    def method(x: Var) = methodTable(x.name)
    def superType = clazz.supercn
    def selfType  = clazz.cn
  }

  def typecheck(ast: AST): Type = {
    val top = ClassTypes(Class("TopClass", null, Set(), Set()))
    inScope(TypeEnv(Map(), MMap(ClassT("TopClass") -> top))).check(ast)
  }

  // type environment
  case class TypeEnv(env: Map[String, Type] = Map(), classTable: MMap[Type, ClassTypes] = MMap()) {
    /* Built-In types */
    val builtins = Set[Type](BoolT, IntT, StrT, NullT)

    def apply( x:String ): Type =
      env get x match {
        case Some(τ) ⇒ τ
        case None => throw Illtyped(s"UndeclaredName: $x not found. ${env}")
      }

    /* Extend the environment leaving the mutable classTable the same */
    def ++( bindings:Seq[(String, Type)] ): TypeEnv =
      TypeEnv(env ++ bindings, classTable)

    /* A check if a type has been declared in the TypeEnv */
    def isDeclared(t: Type): Boolean =
      if (builtins.contains(t) || classTable.contains(t)) true else false

    /* A helper for returning the ClassTypes object given a ClassT */
    def classFor(c: Type): ClassTypes = classTable get c match {
      case Some(t) => t
      case None => throw Illtyped(s"Class name $c is not declared.")   }
  }

  case class inScope(ρ: TypeEnv) {
    implicit val env = ρ

    def formatType(t: Type) = t match {
      case ClassT(cn) => cn
      case BoolT      => "bool"
      case IntT       => "int"
      case StrT       => "string"
      case NullT      => "null"
    }

    def check(term: AST): Type = term match {
      case Program(classes) =>
        for (clazz <- classes) yield check(clazz)
        NullT
      case clazz @ Class(className, superClass, fields, methods) =>
        /* check that superClass is declared */
        if (!ρ.isDeclared(ClassT(superClass)))
          throw Illtyped(s"Super class: $superClass not declared.")

        if (ρ.isDeclared(ClassT(className))) {
          throw Illtyped(s"Class name: $className already declared.")
        } else {
          ρ.classTable += ((ClassT(className), ClassTypes(clazz)))
        }

        val self = "self" -> ClassT(className)
        val fs = for (f @ Decl(k, v) <- fields) yield k.name -> check(f)

        methods map { m => inScope(ρ ++ (self :: fs.toList)) check m }

        NullT
      case Method(methodName, args, retT, body, rete) =>
        /* check that arguments exist, and expand scope with method */
        val paramsT = for (d @ Decl(x, t) <- args) yield x.name -> check(d)
        val methodScope = inScope(ρ ++ paramsT.toList)

        /* check that each stmt in correct */
        for (stmt <- body) { methodScope.check(stmt) }

        /* check return type */
        val rtype = methodScope.check(rete)
        if (retT subtypeOf rtype) NullT else throw Illtyped(s"Declared return type: $retT does match actual return type of $rtype. ${ρ.classTable}")
      case Decl(x, t) =>
        if (ρ.isDeclared(t)) t else throw Illtyped(s"Type for $x: $t is not declared. ${ρ.classTable}")
      /* Statements */
      case Assign(x, e) =>
        val t1 = check(x)
        val t2 = check(e)
        if (t2 subtypeOf t1) NullT else
          throw Illtyped(
            s"Attempting to assign value of type ${formatType(t1)} to a variable of type ${formatType(t2)}"
          )
      case Update(e1, x, e2) =>
        val receiverT = check(e1)
        val clazz = ρ.classFor(receiverT)
        val fieldT = clazz.field(x)
        if (!(fieldT subtypeOf check(e2)))
          throw Illtyped(s"Does not match obj field type.")
        NullT
      case Call(x, e, mn, args) =>
        val fieldT = check(x)
        val receiver = ρ.classFor(check(e))
        val method = receiver.method(Var(mn))
        for ((d, a) <- method.argTs.zip(args)) {
          if (!(check(a).subtypeOf(d)))
            throw Illtyped(s"$a is not subtype $d")
        }

        if (!method.returnT.subtypeOf(fieldT))
          throw Illtyped(s"${ method.returnT } is not subtype $fieldT")
        NullT

      case New(x, cn, args) =>
        val fieldT = check(x)
        val classT = ρ.classFor(ClassT(cn))
        val declTypes = classT.constructor.argTs

        for ((d, a) <- declTypes.zip(args)) {
          if (!d.subtypeOf(check(a)))
            throw Illtyped(s"$d $a")
        }

        if (!ClassT(cn).subtypeOf(fieldT))
          throw Illtyped(s"$cn not subtype of ${formatType(fieldT)}")

        ClassT(cn)

      case If(e, tb, fb) =>
        val guardT = check(e)
        /* see below section
        val tT = ???
        val fT = ???
        */
        if (guardT != BoolT) throw Illtyped(s"$e must be of type bool.")
        /* if *if* returns a value */
        /* (tT.subtypeOf(fT), fT.subtypeOf(tT)) match {
          case (true, true) => tT
          case (false, true) => tT
          case (true, false) => tF
          case (false, false) => throw Illtyped(s"No LUB on $t1 $t2")
        } */
        for (stmt <- tb ++ fb) yield check(stmt)
        NullT

      case While( e, body ) =>
        /* check guard */
        if (check(e) == BoolT)
          throw Illtyped(s"$e must be of type bool.")
        /* check body */
        for (stmt <- body) yield check(stmt)
        /* unit */
        NullT

      case Print(e) =>
        check(e)
        NullT
      /* Expressions */
      case Nums(_)   =>
        IntT

      case Bools(_)  =>
        BoolT

      case Strs(_)   =>
        StrT

      case Nulls()   =>
        NullT

      case Var(name) =>
        ρ(name)

      case Access(e, x) =>
        val receiver = ρ.classFor(check(e))
        receiver.field(x)

      case Binop(op, e1, e2) =>
        val types = (check(e1), check(e2))
        op match {
          case ⌜+⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜−⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜×⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜÷⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜<⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜≤⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜∧⌝ => types match {
            case (BoolT, BoolT) => BoolT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜∨⌝ => types match {
            case (BoolT, BoolT) => BoolT
            case (t1, t2)     => throw Illtyped(s"$t1 $t2")
          }

          case ⌜≈⌝ => types match {
            case (t1, t2) if t1.subtypeOf(t2) => BoolT
            case (t1, t2) => throw Illtyped(s"$t1 $t2")
          }

          case ⌜≠⌝ => types match {
            case (t1, t2) if t1.subtypeOf(t2) => BoolT
            case (t1, t2) => throw Illtyped(s"$t1 $t2")
          }
        }

    }
  }
}

