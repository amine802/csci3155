package jsy.student

object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Sami Meharzi>
   * 
   * Partner: <Tyler Rostenbach>
   * Collaborators: <Nolawee Mengist, Chad>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace 'YourIdentiKey' in the object name above with your IdentiKey.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The ???
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */

  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }

  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case Undefined => Double.NaN
      case S(s) => try {s.toDouble} catch {case _: NumberFormatException => Double.NaN}
      case B(b) => b match {
        case true => 1
        case false => 0
      }
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => {
        return b;
      }
      case N(n) => {
        if (n.isNaN() || n == 0) {
          return false;
        }
        return true;
      }
      case S(s) => {
        if (s.equals("")) {
          return false
        } else {
          return true;
        }
      }
      case Undefined => {
        return false;
      }
    }
  }


  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      case Unary(Neg, e) => N(0-toNumber(eToVal(e)))
      case Unary(Not, e) => B(!toBoolean(eToVal(e)))
      case Binary(bop, e1, e2) => {
        bop match
        {
          case Plus =>  {
            (e1, e2) match{
              case (S(_),_)|(_,S(_)) => S(toStr(eToVal(e1))+ toStr(eToVal(e2)))
              case (_,_) => N(toNumber(eToVal(e1))+toNumber(eToVal(e2)))
            }
          }
          case Minus => N(toNumber(eToVal(e1))-toNumber(eToVal(e2)))
          case Times =>N(toNumber(eToVal(e1))*toNumber(eToVal(e2)))
          case Div => {
            require(toNumber(eToVal(e1))!=0)
            N(toNumber(eToVal(e1))/toNumber(eToVal(e2)))
          }
          case Eq => B(eToVal(e1)==eToVal(e2))
          case Ne => B(eToVal(e1)!=eToVal(e2))
          case Lt => (e1, e2) match{
            case (S(s1),S(s2)) => B((s1)<(s2))
            case (_,_) => B(toNumber(eToVal(e1))<toNumber(eToVal(e2)))
          }

          case Le => (e1, e2) match{
            case (S(s1),S(s2)) => B((s1)<=(s2))
            case (_,_) =>B(toNumber(eToVal(e1))<=toNumber(eToVal(e2)))
          }

          case Gt =>(e1, e2) match{
            case (S(s1),S(s2)) => B((s1)>(s2))
            case (_,_) =>B(toNumber(eToVal(e1))>toNumber(eToVal(e2)))
          }

          case Ge =>(e1, e2) match{
            case (S(s1),S(s2)) => B((s1)>=(s2))
            case (_,_) =>B(toNumber(eToVal(e1))>=toNumber(eToVal(e2)))
          }

          case And =>
            if(!toBoolean(eToVal(e1))) eToVal(e1)
            else eToVal(e2)

          case Or =>
            if(toBoolean(eToVal(e1))) eToVal(e1)
            else eToVal(e2)
          case Seq => {
            eToVal(e1)
            eToVal(e2)
          }
        }
      }
      case If(e1,e2,e3) =>
        if(toBoolean(eToVal(e1))) eToVal(e2)
        else eToVal(e3)

      case ConstDecl(x, e1, e2) => {
        val v1 =eToVal(e1)
        val envp=extend(env,x,v1)
        eval(envp,e2)
      }
      case Var(x) => get(env,x)
      case Print(e1) => println(pretty(eToVal(e1))); Undefined
      case N(_) | B(_) | S(_) | Undefined => e
    }
  }


  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}