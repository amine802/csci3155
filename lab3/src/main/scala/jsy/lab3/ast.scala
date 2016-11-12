/**
 * Version History:
 *   0.3 (02/09/2014): Position information.
 *   0.2 (09/18/2012): Changed printing of AST nodes.
 *   0.1 (09/07/2012): Initial release.
 */
package jsy.lab3

import scala.util.parsing.input.Positional

/**
 * @author Bor-Yuh Evan Chang
 */
object ast {
  sealed abstract class Expr extends Positional
  
  /* Variables */
  case class Var(x: String) extends Expr
  
  /* Declarations */
  case class ConstDecl(x: String, e1: Expr, e2: Expr) extends Expr
  
  /* Literals and Values*/
  case class N(n: Double) extends Expr
  case class B(b: Boolean) extends Expr
  case class S(s: String) extends Expr
  case object Undefined extends Expr
  
  /* Unary and Binary Operators */
  case class Unary(uop: Uop, e1: Expr) extends Expr
  case class Binary(bop: Bop, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Uop
  
  case object Neg extends Uop /* - */
  case object Not extends Uop /* ! */

  sealed abstract class Bop
  
  case object Plus extends Bop /* + */
  case object Minus extends Bop /* - */
  case object Times extends Bop /* * */
  case object Div extends Bop /* / */
  case object Eq extends Bop /* === */
  case object Ne extends Bop /* !=== */
  case object Lt extends Bop /* < */
  case object Le extends Bop /* <= */
  case object Gt extends Bop /* > */
  case object Ge extends Bop /* >= */
  
  case object And extends Bop /* && */
  case object Or extends Bop /* || */
  
  case object Seq extends Bop /* , */
  
  /* Intraprocedural Control */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr
  
  /* I/O */
  case class Print(e1: Expr) extends Expr 
  
  /* Functions */
  case class Function(p: Option[String], x: String, e1: Expr) extends Expr
  case class Call(e1: Expr, e2: Expr) extends Expr
  
  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => true
    case _ => false
  }
  
  /*
   * Pretty-print values.
   * 
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => prettyNumber(n)
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(p, _, _) =>
        "[Function%s]".format(p match { case None => "" case Some(s) => ": " + s })
    }
  }

  def prettyNumber(n: Double): String =
    if (n.isWhole) "%.0f" format n else n.toString
  
  /* Get the free variables of e. */
  def freeVars(e: Expr): Set[String] = e match {
    case Var(x) => Set(x)
    case ConstDecl(x, e1, e2) => freeVars(e1) | (freeVars(e2) - x)
    case Function(None, x, e1) => freeVars(e1) - x
    case Function(Some(x1), x2, e1) => freeVars(e1) - x2 - x1
    case N(_) | B(_) | Undefined | S(_) => Set.empty
    case Unary(_, e1) => freeVars(e1)
    case Binary(_, e1, e2) => freeVars(e1) | freeVars(e2)
    case If (e1, e2, e3) => freeVars(e1) | freeVars(e2) | freeVars(e3)
    case Call(e1, e2) => freeVars(e1) | freeVars(e2)
    case Print(e1) => freeVars(e1)
  }
  
  /* Check closed expressions. */
  def closed(e: Expr): Boolean = freeVars(e).isEmpty
  
  /*
   * Dynamic Type Error exception.  Throw this exception to signal a dynamic type error.
   * 
   *   throw DynamicTypeError(e)
   * 
   */
  case class DynamicTypeError(e: Expr) extends Exception {
    override def toString = "TypeError: in expression " + e
  }
  
}