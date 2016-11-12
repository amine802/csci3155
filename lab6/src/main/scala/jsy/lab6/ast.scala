package jsy.lab6

import scala.util.parsing.input.{Position, Positional}
import jsy.util.DoWith

/**
 * @author Bor-Yuh Evan Chang
 */
object ast {
  sealed abstract class Expr extends Positional

  /* Variables */
  case class Var(x: String) extends Expr

  /* Declarations */
  case class Decl(mut: Mutability, x: String, e1: Expr, e2: Expr) extends Expr
  case class InterfaceDecl(tvar: String, tobj: Typ, e: Expr) extends Expr

  /* Literals and Values*/
  case class N(n: Double) extends Expr
  case class B(b: Boolean) extends Expr
  case object Undefined extends Expr
  case class S(s: String) extends Expr

  /* Unary and Binary Operators */
  case class Unary(uop: Uop, e1: Expr) extends Expr
  case class Binary(bop: Bop, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Uop

  case object Neg extends Uop /* -e1 */
  case object Not extends Uop /* !e1 */

  sealed abstract class Bop

  case object Plus extends Bop /* e1 + e2 */
  case object Minus extends Bop /* e1 - e2 */
  case object Times extends Bop /* e1 * e2 */
  case object Div extends Bop /* e1 / e2 */
  case object Eq extends Bop /* e1 === e2 */
  case object Ne extends Bop /* e1 !=== e2 */
  case object Lt extends Bop /* e1 < e2 */
  case object Le extends Bop /* e1 <= e2 */
  case object Gt extends Bop /* e1 > e2 */
  case object Ge extends Bop /* e1 >= e2 */

  case object And extends Bop /* e1 && e2 */
  case object Or extends Bop /* e1 || e2 */

  case object Seq extends Bop /* , */

  /* Intraprocedural Control */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr

  /* Functions */
  type Params = Either[ List[(String,Typ)], (PMode,String,Typ) ]
  case class Function(p: Option[String], paramse: Params, tann: Option[Typ], e1: Expr) extends Expr
  case class Call(e1: Expr, args: List[Expr]) extends Expr

  /* I/O */
  case class Print(e1: Expr) extends Expr

  /* Objects */
  case class Obj(fields: Map[String, Expr]) extends Expr
  case class GetField(e1: Expr, f: String) extends Expr

  /* Addresses and Mutation */
  case class Assign(e1: Expr, e2: Expr) extends Expr
  case object Null extends Expr
  case class A private[ast] (addr: Int) extends Expr

  case object Deref extends Uop /* *e1 */

  sealed abstract class Mutability
  case object MConst extends Mutability
  case object MVar extends Mutability

  /* Parameter Passing */
  sealed abstract class PMode
  case object PName extends PMode
  case object PVar extends PMode
  case object PRef extends PMode

  /* Casting */
  case class Cast(t: Typ) extends Uop

  /* Types */
  sealed abstract class Typ
  case object TNumber extends Typ
  case object TBool extends Typ
  case object TString extends Typ
  case object TUndefined extends Typ
  case object TNull extends Typ
  case class TFunction(paramse: Params, tret: Typ) extends Typ {
    override def equals(other: Any) = other.isInstanceOf[TFunction] && {
      other match {
        case TFunction(oparamse, otret) if otret == tret =>
          def proj(pe: Params) = pe.fold(
          { params => Left(params map { case (_,t) => t }) },
          { case (mode,_,t) => Right((mode, t)) }
          )
          proj(oparamse) == proj(paramse)
        case _ => false
      }
    }
  }
  case class TObj(tfields: Map[String, Typ]) extends Typ
  case class TVar(tvar: String) extends Typ
  case class TInterface(tvar: String, t: Typ) extends Typ

  /* Regular Expressions */
  case class RE(re: RegExpr) extends Expr
  case object TRegExp extends Typ

  sealed abstract class RegExpr
  case object RNoString extends RegExpr
  case object REmptyString extends RegExpr
  case class RSingle(c: Char) extends RegExpr
  case class RConcat(re1: RegExpr, re2: RegExpr) extends RegExpr
  case class RUnion(re1: RegExpr, re2: RegExpr) extends RegExpr
  case class RStar(re1: RegExpr) extends RegExpr
  case object RAnyChar extends RegExpr
  case class RPlus(re1: RegExpr) extends RegExpr
  case class ROption(re1: RegExpr) extends RegExpr
  case class RIntersect(re1: RegExpr, re2: RegExpr) extends RegExpr
  case class RNeg(re1: RegExpr) extends RegExpr

  /* Generic Run-Time Call */
  case class RuntimeCall(f: String, args: List[Expr]) extends Expr

  /*
   * Memory
   *
   * Note that the memempty and memalloc functions would idiomatically be in
   * the companion object to this class, but we do not have a companion object to
   * avoid introducing this feature.
   */
  class Mem private[ast] (map: Map[A, Expr], nextAddr: Int) {
    def apply(key: A): Expr = map(key)
    def get(key: A): Option[Expr] = map.get(key)
    def +(kv: (A, Expr)): Mem = new Mem(map + kv, nextAddr)
    def contains(key: A): Boolean = map.contains(key)

    private[ast] def alloc(v: Expr): (Mem, A) = {
      val fresha = A(nextAddr)
      (new Mem(map + (fresha -> v), nextAddr + 1), fresha)
    }

    override def toString: String = map.toString
  }

  def memempty: Mem = new Mem(Map.empty, 1)
  def memalloc(v: Expr): DoWith[Mem, A] = DoWith.doget flatMap { m =>
    val (mp, a) = m.alloc(v)
    DoWith.doput(mp) map { _ => a }
  }

  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case N(_) | B(_) | Undefined | S(_) | Function(_, _, _, _) | A(_) | Null | RE(_) => true
    case _ => false
  }

  def isLExpr(e: Expr): Boolean = e match {
    case Var(_) | GetField(_, _) => true
    case _ => false
  }

  def isLValue(e: Expr): Boolean = e match {
    case Unary(Deref, A(_)) | GetField(A(_), _) => true
    case _ => false
  }

  def isBaseType(t: Typ): Boolean = t match {
    case TNumber | TBool | TString | TUndefined | TNull => true
    case _ => false
  }

  /*
   * Pretty-print values.
   *
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(v: Expr): String = {
    (v: @unchecked) match {
      case N(n) => n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(p, _, _, _) =>
        "[Function%s]".format(p match { case None => "" case Some(s) => ": " + s })
      case Obj(fields) =>
        val pretty_fields =
          fields map {
            case (f, S(s)) => f + ": '" + s + "'"
            case (f, v) => f + ": " + pretty(v)
          } reduceRight {
            (s, acc) => s + ",\n  " + acc
          }
        "{ %s }".format(pretty_fields)
      case Null => "null"
      case A(i) => "0x%x".format(i)
      case RE(re) => "/^" + pretty(re) + "$/"
    }
  }

  def pretty(re: RegExpr): String = re match {
    case RNoString => "!"
    case REmptyString => "#"
    case RSingle(c) => c.toString
    case RConcat(re1,re2) => pretty(re1) + pretty(re2)
    case RUnion(re1,re2) => s"(${pretty(re1)}|${pretty(re2)})"
    case RStar(re1) => s"(${pretty(re1)})*"
    case RAnyChar => "."
    case RPlus(re1) => s"(${pretty(re1)})+"
    case ROption(re1) => s"(${pretty(re1)})?"
    case RIntersect(re1,re2) => s"(${pretty(re1)}&${pretty(re2)})"
    case RNeg(re1) => s"~(${pretty(re1)})"
  }

  def pretty(m: Mem, v: Expr): String = {
    (v: @unchecked) match {
      case a @ A(_) if m contains a => pretty(m, m(a))
      case Obj(fields) =>
        val pretty_fields =
          fields map {
            case (f, S(s)) => f + ": '" + s + "'"
            case (f, v) => f + ": " + pretty(m, v)
          } reduceRight {
            (s, acc) => s + ",\n  " + acc
          }
        "{ %s }".format(pretty_fields)
      case _ => pretty(v)
    }
  }

  def pretty(m: Mutability): String = m match {
    case MConst => "const"
    case MVar => "var"
  }

  /*
   * Pretty-print types.
   *
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(t: Typ): String = t match {
    case TNumber => "number"
    case TBool => "bool"
    case TString => "string"
    case TUndefined => "Undefined"
    case TFunction(paramse, tret) => {
      val pretty_params: Option[String] = paramse match {
        case Left(params) => params map { case (x,t) => "%s: %s".format(x, pretty(t)) } reduceRightOption {
          (s, acc) => s + ", " + acc
        }
        case Right((mode, x, t)) => Some("%s %s: %s".format(pretty(mode), x, pretty(t)))
      }
      "(%s) => %s".format(pretty_params.getOrElse(""), pretty(tret))
    }
    case TObj(tfields) =>
      val pretty_fields: Option[String] =
        tfields map { case (f,t) => "%s: %s".format(f, pretty(t)) } reduceRightOption {
          (s, acc) => s + "; " + acc
        }
      "{ %s }".format(pretty_fields.getOrElse(""))
    case TNull => "Null"
    case TVar(tvar) => tvar
    case TInterface(tvar, t1) => "Interface %s %s".format(tvar, pretty(t1))
    case TRegExp => "RegExp"
  }

  def pretty(m: PMode): String = m match {
    case PName => "name"
    case PVar => "var"
    case PRef => "ref"
  }

  /* Get the free variables of e. */
  def freeVarsVar(e: Expr): Set[Var] = e match {
    case vr @ Var(x) => Set(vr)
    case Decl(_, x, e1, e2) => freeVarsVar(e1) | (freeVarsVar(e2) - Var(x))
    case Function(p, paramse, _, e1) =>
      freeVarsVar(e1) --
        (paramse match {
          case Left(params) => params map { case (x, _) => Var(x) }
          case Right((_, x, _)) => Some(Var(x))
        }) --
        (p map { x => Var(x) })
    case N(_) | B(_) | Undefined | S(_) | Null | A(_) => Set.empty
    case Unary(_, e1) => freeVarsVar(e1)
    case Binary(_, e1, e2) => freeVarsVar(e1) | freeVarsVar(e2)
    case If (e1, e2, e3) => freeVarsVar(e1) | freeVarsVar(e2) | freeVarsVar(e3)
    case Call(e1, args) => freeVarsVar(e1) | args.foldLeft(Set.empty: Set[Var]){ ((acc: Set[Var], ei) => acc | freeVarsVar(ei)) }
    case Print(e1) => freeVarsVar(e1)
    case Obj(fields) => fields.foldLeft(Set.empty: Set[Var])({ case (acc, (_, ei)) => acc | freeVarsVar(ei) })
    case GetField(e1, _) => freeVarsVar(e1)
    case Assign(e1, e2) => freeVarsVar(e1) | freeVarsVar(e2)
    case InterfaceDecl(_, _, e1) => freeVarsVar(e1)
    case RE(_) => Set.empty
    case RuntimeCall(f, args) => args.foldLeft(Set.empty: Set[Var]){ ((acc: Set[Var], ei) => acc | freeVarsVar(ei)) }
  }
  def freeVars(e: Expr): Set[String] = freeVarsVar(e) map { case Var(x) => x }

  /* Check closed expressions. */
  class UnboundVariableError(x: Var) extends Exception {
    override def toString =
      JsyParser.formatErrorMessage(x.pos, "UnboundVariableError", "unbound variable %s".format(x.x))
  }

  def closed(e: Expr): Boolean = freeVarsVar(e).isEmpty
  def checkClosed(e: Expr): Unit = {
    freeVarsVar(e).headOption.foreach { x => throw new UnboundVariableError(x) }
  }

  /* Rename bound variables in e to avoid capturing free variables in esub. */
  def avoidCapture(avoidVars: Set[String], e: Expr): Expr = {
    def renameVar(x: String): String = if (avoidVars contains x) renameVar(x + "$") else x

    def rename(env: Map[String,String], e: Expr): Expr = {
      def ren(e: Expr): Expr = rename(env, e)
      e match {
        case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
        case Print(e1) => Print(ren(e1))
        case Unary(uop, e1) => Unary(uop, ren(e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(e1), ren(e2))
        case If(e1, e2, e3) => If(ren(e1), ren(e2), ren(e3))
        case Var(y) => Var(env.getOrElse(y, y))
        case Decl(mut, y, e1, e2) =>
          val yrenamed = renameVar(y)
          Decl(mut, yrenamed, ren(e1), rename(env + (y -> yrenamed), e2))
        case Function(p, paramse, retty, e1) =>
          val (env1, prenamed) = p match {
            case None => (env, None)
            case Some(y) =>
              val yrenamed = renameVar(y)
              (env + (y -> yrenamed), Some(yrenamed))
          }
          val (env2, paramserenamed) = paramse match {
            case Left(params) =>
              val (envnew, revparamsrenamed) = params.foldLeft((env1, Nil: List[(String, Typ)])) {
                case ((envacc, renamedacc), (y, t)) =>
                  val yrenamed = renameVar(y)
                  (envacc + (y -> yrenamed), (yrenamed, t) :: renamedacc)
              }
              (envnew, Left(revparamsrenamed.reverse))
            case Right((mode,y,t)) =>
              val yrenamed = renameVar(y)
              (env1 + (y -> yrenamed), Right((mode,yrenamed, t)))
          }
          Function(prenamed, paramserenamed, retty, rename(env2, e1))
        case Call(e1, args) => Call(ren(e1), args map ren)
        case Obj(fields) => Obj(fields map { case (f,e) => (f, ren(e)) })
        case GetField(e1, f) => GetField(ren(e1), f)
        case Assign(e1, e2) => Assign(ren(e1), ren(e2))

        case RE(_) => e
        case RuntimeCall(f, args) => RuntimeCall(f, args map ren)

        /* Should not match: should have been removed. */
        case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
      }
    }
    rename(Map.empty, e)
  }


  /*
   * Dynamic Type Error exception.  Throw this exception to signal a dynamic
   * type error.
   *
   *   throw DynamicTypeError(e)
   *
   */
  case class DynamicTypeError(e: Expr) extends Exception {
    override def toString = JsyParser.formatErrorMessage(e.pos, "DynamicTypeError", "in evaluating " + e)
  }

  /*
   * Null Dereference Error exception.  Throw this exception to signal a null
   * pointer dereference error.
   *
   *   throw NullDereferenceError(e)
   *
   */
  case class NullDereferenceError(e: Expr) extends Exception {
    override def toString = JsyParser.formatErrorMessage(e.pos, "NullDereferenceError", "in evaluating " + e)
  }

  /*
   * Static Type Error exception.  Throw this exception to signal a static
   * type error.
   *
   *   throw StaticTypeError(tbad, esub, e)
   *
   */
  case class StaticTypeError(tbad: Typ, esub: Expr, e: Expr) extends Exception {
    override def toString =
      JsyParser.formatErrorMessage(esub.pos, "StaticTypeError", "invalid type %s for sub-expression %s in %s".format(pretty(tbad), esub, e))
  }

  /*
   * Stuck Type Error exception.  Throw this exception to signal getting
   * stuck in evaluation.  This exception should not get raised if
   * evaluating a well-typed expression.
   *
   *   throw StuckError(e)
   *
   */
  case class StuckError(e: Expr) extends Exception {
    override def toString = JsyParser.formatErrorMessage(e.pos, "StuckError", "in evaluating " + e)
  }

  /*
 * Syntax Error exception.  Throw this exception to signal an error
 * in parsing.
 *
 *   throw SyntaxError(msg, pos)
 *
 */
  case class SyntaxError(msg: String, pos: Position) extends Exception {
    override def toString = JsyParser.formatErrorMessage(pos, "SyntaxError", msg)
  }
}
