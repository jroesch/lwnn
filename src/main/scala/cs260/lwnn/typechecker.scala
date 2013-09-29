package cs260.lwnn

import cs260.lwnn.syntax._
import scala.collection.mutable.{ Map => MMap }

case class Illtyped(msg: String) extends Exception(msg)

object typechecker {
  def pp(t: AST) = PrettyPrinter.print(t)
  def pt(t: Type) = PrettyPrinter.printType(t)

  case class ClassTable(table: Map[Type, ClassTableEntry]) {
    def apply(c: Type): ClassTableEntry = table get c match {
      case Some(t) => t
      case None => throw Illtyped(s"Class name $c is not declared. ${this}")
    }

    /* Field lookup that supports super classes. */
    def field(t: Type, x: Var): Type = {
      val clazz = apply(t)
      clazz.field(x) match {
        case Some(ft) => ft
        case None if clazz.superType == ClassT("TopClass") =>
          throw Illtyped(s"Class $t does not have a field $x.")
        case None => field(clazz.superType, x)
      }
    }

    /* Method lookup that supports super classes. */
    def method(t: Type, x: Var): MethodType = {
      val clazz = apply(t)
      clazz.method(x) match {
        case Some(mt) => mt
        case None if clazz.superType == ClassT("TopClass") =>
          throw Illtyped(s"Class $t does not have a method $x.")
        case None => method(clazz.superType, x)
      }
    }

    /* Constructor for a class */
    def constructor(t: Type): MethodType = {
      val clazz = apply(t)
      clazz.constructor match {
        case Some(cons) => cons
        case None if clazz.superType == ClassT("TopClass") =>
          MethodType(t, Nil)
        case None =>
          val MethodType(_, argT) = constructor(clazz.superType)
          MethodType(t, argT) /* downcast the constructor to the derived class */
      }
    }

    /* Built-In types */
    val builtins = Set[Type](BoolT, IntT, StrT, NullT)
  }

  case class MethodType(returnT: Type, argTs: Seq[Type])

  /* A helper container for types related to classes (constructor, field, method, super type) */
  case class ClassTableEntry(clazz: Class) {

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
            Var(method.mn) -> MethodType(method.τret, method.params.map { _.τ })
          }
      }).toSeq: _ *
    )

    val constructor = methodTable.get(Var(clazz.cn))

    def field(x: Var): Option[Type] =
      fieldTable.get(x)

    def method(x: Var): Option[MethodType] =
      methodTable.get(x)

    def superType = ClassT(clazz.supercn)
    def selfType  = ClassT(clazz.cn)
  }

  def typecheck(ast: AST, classTable: ClassTable): Type = {
    inScope(TypeEnv(Map()))(classTable).check(ast)
  }

  // type environment
  case class TypeEnv(env: Map[String, Type] = Map()) {
    def apply( x:String ): Type =
      env get x match {
        case Some(τ) ⇒ τ
        case None => throw Illtyped(s"UndeclaredName: $x not found.")
      }

    def +(binding: (String, Type)): TypeEnv =
      TypeEnv(env + binding)

    /* Extend the environment leaving the mutable classTable the same */
    def ++( bindings:Seq[(String, Type)] ): TypeEnv =
      TypeEnv(env ++ bindings)
  }

  case class inScope(ρ: TypeEnv)(implicit classTable: ClassTable) {
    def check(term: AST): Type = term match {
      case Program(classes) =>
        for (clazz <- classes) yield check(clazz)
        NullT
      case clazz @ Class(className, superClass, fields, methods) =>
        /* check that superClass is declared */
        val sc = classTable(ClassT(superClass))

        val self = "self" -> ClassT(className)

        /* Check that it's fields have valid types */
        for (f @ Decl(k, v) <- fields) yield check(f)

        methods map { m => inScope(ρ + self) check m }

        NullT
      case Method(methodName, args, retT, body, rete) =>
        /* check that arguments exist, and expand scope with method */
        val paramsT = for (d @ Decl(x, t) <- args) yield x.name -> check(d)
        val methodScope = inScope(ρ ++ paramsT.toList)

        /* check that each stmt in correct */
        for (stmt <- body) { methodScope.check(stmt) }

        val rtype = methodScope.check(rete)
        if (retT ⊑ rtype) NullT
        else throw Illtyped(s"Declared return type: $retT does match actual return type of $rtype.")

      case Decl(x, t) => t match {
        /* make sure that the type exists in the ClassTable */
        case c: ClassT => classTable(t).selfType
        case _         => t
      }
      /* Statements */
      case Assign(x, e) =>
        val t1 = check(x)
        val t2 = check(e)
        if (t2 ⊑ t1) NullT else
          throw Illtyped(
            s"Attempting to assign value of type ${pt(t1)} to a variable of type ${pt(t2)}"
          )

      case Update(e1, x, e2) =>
        val receiverT = check(e1)
        val clazz = classTable(receiverT)
        val fieldT = classTable.field(clazz.selfType, x)
        if (fieldT ⊑ check(e2)) NullT
        else throw Illtyped(s"In `${pp(term)}` field has type ${pt(fieldT)} and `${pp(e2)}` has type ${pt(check(e2))}")

      case Call(x, e, mn, args) =>
        val fieldT = check(x)
        val method = classTable.method(check(e), Var(mn))
        for ((d, a) <- method.argTs.zip(args)) {
          if (!(check(a) ⊑ d))
            throw Illtyped(s"$a is not subtype $d")
        }

        if (!(method.returnT ⊑ fieldT))
          throw Illtyped(s"${ method.returnT } is not subtype $fieldT")
        NullT

      case New(x, cn, args) =>
        val fieldT = check(x)
        val classT = ClassT(cn)
        val declTypes = classTable.constructor(classT).argTs

        for ((d, a) <- declTypes.zip(args)) {
          if (!(check(a) ⊑ d))
            throw Illtyped(s"Argument type ${pt(check(a))} does not match declared type ${pt(d)}")
        }

        if (classT ⊑ fieldT) classT
        else throw Illtyped(s"$cn not subtype of ${pt(fieldT)}")


      case If(e, tb, fb) =>
        val guardT = check(e)
        if (guardT != BoolT) throw Illtyped(s"`${pp(e)}` must be of type bool.")
        for (stmt <- tb ++ fb) yield check(stmt)
        NullT

      case w @ While( e, body ) =>
        /* check guard */
        if (check(e) != BoolT)
          throw Illtyped(s"`${pp(e)}` must be of type ${pt(BoolT)} in\n\n${pp(w)}")
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
        classTable.field(check(e), x)

      case Binop(op, e1, e2) =>
        val types = (check(e1), check(e2))
        op match {
          case ⌜+⌝ | ⌜−⌝ | ⌜×⌝ | ⌜÷⌝ => types match {
            case (IntT, IntT) => IntT
            case (t1, t2)     =>
              throw Illtyped(s"${pt(t1)} ${pt(t2)}  must both be int.")
          }

          case ⌜<⌝ | ⌜≤⌝ => types match {
            case (IntT, IntT) => IntT
            case (StrT, StrT) => StrT
            case (t1, t2)     =>
              throw Illtyped(s"${pt(t1)} ${pt(t2)}  must both be either int or string.")
          }

          case ⌜∧⌝ | ⌜∨⌝ => types match {
            case (BoolT, BoolT) => BoolT
            case (t1, t2)       =>
              throw Illtyped(s"${pt(t1)} ${pt(t2)} must both be bool.")
          }

          case ⌜≈⌝ | ⌜≠⌝ => types match {
            case (t1, t2) => BoolT
          }
        }
    }
  }
}

