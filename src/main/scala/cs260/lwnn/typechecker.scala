package cs260.lwnn

import cs260.lwnn.syntax._
import scala.collection.mutable.{ Map => MMap }

case class Illtyped(msg: String) extends Exception(msg)

object typechecker {
  def typecheck(ast: AST): Type = {
    inScope(Env(Map(), MMap(ClassT("TopClass") -> null))).check(ast)
  }

  // type environment
  case class Env(env: Map[String, Type] = Map(), classTable: MMap[Type, ClassTypes] = MMap()) {
    val builtins = Set[Type](BoolT, IntT, StrT, NullT)

    // retrieve a variable's type or throw exception if variable
    // isn't in the environment
    def apply( x:String ): Type =
      env get x match {
        case Some(τ) ⇒ τ
        case None => throw Illtyped(s"UndeclaredName: $x not found. ${env}")
      }

    def ++( bindings:Seq[(String, Type)] ): Env =
      Env(env ++ bindings, classTable)

    def isDeclared(t: Type): Boolean =
      if (builtins.contains(t) || classTable.contains(t)) true else false

    def classFor(c: Type): ClassTypes = classTable get c match {
      case Some(t) => t
      case None => throw Illtyped(s"Class name $c is not declared.")   }
  }


  // every term is typechecked inside a scope corresponding to a
  // particular environment

    case class inScope(ρ: Env) {
      implicit val env = ρ

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
          if (retT == rtype) NullT else throw Illtyped(s"Declared return type: $retT does match actual return type of $rtype. ${ρ.classTable}")
        case Decl(x, t) =>
          if (ρ.isDeclared(t)) t else throw Illtyped(s"Type for $x: $t is not declared. ${ρ.classTable}")
        /* Statements */
        case Assign(x, e) =>
          val t1 = check(x)
          val t2 = check(e)
          if (t1 == t2) NullT else throw Illtyped(s"Variable/Exp mismatch")
        case Update(e1, x, e2) =>
          val receiverT = check(e1)
          val clazz = ρ.classFor(receiverT)
          val fieldT = clazz.field(x)
          if (fieldT != check(e2))
            throw Illtyped(s"Doesnt match obj field type.")
          NullT
        case Call(x, e, mn, args) => ???

        case New(x, cn, args) =>
          //TODO: check args
          val fieldT = check(x)
          val classT = ρ.classFor(ClassT(cn))
          /* val argsT = classT.constructor
          for ((arg, t) <- args zip argsT) {
            if (!check(arg).subtypeOf(t)) {
              throw Illtyped(s"not subtype of")
            }
          } */
         ClassT(cn)
            
        case If(e, tb, fb) => ???
        case While( e, body ) => ???
        case Print(e) =>
          check(e)
          NullT
        /* Expressions */
        case Nums(_)   => IntT
        case Bools(_)  => BoolT
        case Strs(_)   => StrT
        case Nulls()   => NullT
        case Var(name) => ρ(name)
        case Access(e, x) => ???
        case Binop(op, e1, e2) => throw new Exception(s"not yet implmented binops")

        case x @ _ => throw new Exception(s"$x")
      }
    }
}

