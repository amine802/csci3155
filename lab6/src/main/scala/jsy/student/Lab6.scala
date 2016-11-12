package jsy.student

import jsy.util.options.SetBool

object Lab6 extends jsy.util.JsyApplication {
  import jsy.lab6.ast._
  import jsy.lab6._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 6
   * <Sami Meharzi>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
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
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */


  /** * Regular Expression Parsing ***/

  import scala.util.parsing.combinator.Parsers
  import scala.util.parsing.input.CharSequenceReader

  /* We define a recursive decent parser for regular expressions in
   * RegExprParser.
   *
   * We derive RegExprParser from Parsers in the Scala library to make
   * use of it's handing of input (Input) and parsing results
   * (ParseResult).
   *
   * The Parsers trait is actually a general purpose combinator parser
   * library that we will not use.
   *
   * Grammar
   *
   *   re ::= union
   *   union ::= intersect {| intersect}
   *   intersect ::= concat {& concat}
   *   concat ::= not {not}
   *   not ::= ~ not | star
   *   star ::= atom {*|+|?}
   *   atom ::= ! | # | c | . | ( re )
   *
   */
  object REParser extends Parsers {
    type Elem = Char

    /* The following items are the relevant pieces inherited from Parsers
     *
     * type Input = Reader[Elem]
     * sealed abstract class ParseResult[+T] {
     *   val next: Input
     *   def map[U](f: T => U): ParseResult[U]
     * }
     * case class Success[+T](result: T, next: Input) extends ParseResult[T]
     * case class Failure(next: Input) extends ParseResult[Nothing]
     */

    def re(next: Input): ParseResult[RegExpr] = union(next)

    def union(next: Input): ParseResult[RegExpr] = intersect(next) match {
      case Success(r, next) => {
        def unions(acc: RegExpr, next: Input): ParseResult[RegExpr] =
          if (next.atEnd) Success(acc, next)
          else (next.first, next.rest) match {
            case ('|', next) => intersect(next) match {
              case Success(r, next) => unions(RUnion(acc, r), next)
              case _ => Failure("expected intersect", next)
            }
            case _ => Success(acc, next)
          }
        unions(r, next)
      }
      case _ => Failure("expected intersect", next)
    }

    def intersect(next: Input): ParseResult[RegExpr] = concat(next) match {
      case Success(r, next) => {
        def intersects(acc: RegExpr, next: Input): ParseResult[RegExpr] =
          if (next.atEnd) Success(acc, next)
          else (next.first, next.rest) match {
            case ('&', next) => concat(next) match {
              case Success(r, next) => intersects(RIntersect(acc, r), next)
              case _ => Failure("expected concat", next)
            }
            case _ => Success(acc, next)
          }
        intersects(r, next)
      }
      case _ => Failure("expected concat", next)
    }

    def concat(next: Input): ParseResult[RegExpr] = not(next) match {
      case Success(r, next) => {
        def concats(acc: RegExpr, next: Input): ParseResult[RegExpr] =
          if (next.atEnd) Success(acc, next)
          else not(next) match {
            case Success(r, next) => concats(RConcat(acc, r), next)
            case _ => Success(acc, next)
          }
        concats(r, next)
      }
      case fail => fail
    }

    def not(next: Input): ParseResult[RegExpr] = (next.first, next.rest) match {
      case ('~', nextrest) => not(nextrest) match{
        case Success(r, next) => Success(RNeg(r), next)
        case fail => fail
      }
      case _ => star(next) match {
        case Success(r, next) => Success(r, next)
        case fail => fail
      }
    }

    def star(next: Input): ParseResult[RegExpr] = atom(next) match {
      //we take the atom of the next
      case Success(r, next) => {
        def stars(acc: RegExpr, next: Input): ParseResult[RegExpr] =
        //define a new function which takes the acc and next
          if (next.atEnd) Success(acc, next)
          //check if next is the end
          else (next.first, next.rest) match {
            //elese, check if there are any of the symbols we should have
            case ('+', next) => stars(RPlus(acc), next)
            case ('*', next) => stars(RStar(acc),next)
            case ('?', next) => stars(ROption(acc), next)
            //run stars on the modified version
            case _ => Success(acc, next)
            //otherwise we are successfull
          }
        stars(r, next)
        //we need to run stars on
      }
      case _ => Failure("no atom", next)
    }

    /* This set is useful to check if a Char is/is not a regular expression
       meta-language character.  Use delimiters.contains(c) for a Char c. */
    val delimiters = Set('|', '&', '~', '*', '+', '?', '!', '#', '.', '(', ')')

    def atom(next: Input): ParseResult[RegExpr] = {
      if (next.atEnd) Failure("empty, not able to match", next)
      else (next.first, next.rest) match {
        case ('!', next) => Success(RNoString, next)
        case ('#', next) => Success(REmptyString, next)
        case ('.', next) => Success(RAnyChar, next)
        case ('(', next) => re(next) match {
          case Success(reast, next) => (next.first, next.rest) match {
            case (')', next) => Success(reast, next)
            case _ => Failure("expected )", next)
          }
          case fail => fail
        }
        case (c, next) if !delimiters.contains(c) => Success(RSingle(c), next)
        case _ => Failure("expected an atom", next)
      }
    }


    /* External Interface */

    def parse(next: Input): RegExpr = re(next) match {
      case Success(r, next) if (next.atEnd) => r
      case Success(_, next) => throw new SyntaxError("remaining input", next.pos)
      case Failure(msg, next) => throw new SyntaxError(msg, next.pos)
    }

    def parse(s: String): RegExpr = parse(new CharSequenceReader(s))
  }


  /** * Regular Expression Matching ***/

  def retest(re: RegExpr, s: String): Boolean = {
    def test(re: RegExpr, chars: List[Char], sc: List[Char] => Boolean): Boolean = (re, chars) match {
      /* Basic Operators */
      case (RNoString, _) => false
      case (REmptyString, _) => sc(chars)
      case (RSingle(_), Nil) => false
      case (RSingle(c1), c2 :: t) => if (c1 == c2) sc(t) else false
      case (RConcat(re1, re2), _) => test(re1, chars, { next => test(re2, next, sc) })
      case (RUnion(re1, re2), _) => test(re1, chars, sc) || test(re2, chars, sc)
      case (RStar(re1), _) => sc(chars) || test(re1, chars, { next => if (next.size < chars.size) test(RStar(re1), next, sc) else false })

      /* Extended Operators */
      case (RAnyChar, Nil) => false
      case (RAnyChar, _ :: t) => sc(t)
      case (RPlus(re1), _) => test(RConcat(re1, RStar(re1)), chars, sc)
      case (ROption(re1), _) => if (chars.isEmpty) true else test(re1, chars, sc)

      /***** Extra Credit Cases *****/
      // case (RIntersect(re1, re2), _) => test(re1, chars, sc) && test(re2, chars, sc)
      // case (RNeg(re1), _) => test(re1, chars, sc)
    }
    test(re, s.toList, { chars => chars.isEmpty })
  }


  /*******************************/
  /*** JavaScripty Interpreter ***/
  /*******************************/

  /* This part is optional and only for fun.
   *
   * If you want your own complete JavaScripty interpreter, you can copy your
   * Lab 5 interpreter here and extend it for the Lab 6 constructs.
   *
   * The template below is the same template as for Lab 5, except with a
   * few additions to call the regular expression tester. These additions are
   * labeled with "Begin Regular Expression Extension."
   */

  /*** Helper: mapFirst to DoWith ***/

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](f: A => Option[DoWith[W,A]])(l: List[A]): DoWith[W,List[A]] = l match {
    case Nil => ???
    case h :: t => ???
  }

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    /***** Make sure to replace the case _ => ???. */
    //case _ => ???
    /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ???
    case (_, TInterface(tvar, t2p)) => ???
    /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  type TEnv = Map[String, (Mutability,Typ)]
  val emp: TEnv = Map()
  def get(env: TEnv, x: String): (Mutability,Typ) = env(x)
  def extend(env: TEnv, x: String, mt: (Mutability,Typ)): TEnv = env + (x -> mt)

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  // A helper function to translate parameter passing mode to the mutability of
  // the variable.
  def mut(m: PMode): Mutability = m match {
    case PName => MConst
    case PVar | PRef => MVar
  }

  def typeInfer(env: TEnv, e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      /***** Begin Regular Expressions Extension *****/
      case RE(_) => TRegExp
      case Call(GetField(e1, "test"), List(e2)) => typ(e1) match {
        case TRegExp => typ(e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      /***** End Regular Expressions Extension *****/

      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) =>
        val (_, t) = env(x)
        t
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) =>
        ???
      case Binary(Plus, e1, e2) =>
        ???
      case Binary(Minus|Times|Div, e1, e2) =>
        ???
      case Binary(Eq|Ne, e1, e2) =>
        ???
      case Binary(Lt|Le|Gt|Ge, e1, e2) =>
        ???
      case Binary(And|Or, e1, e2) =>
        ???
      case Binary(Seq, e1, e2) =>
        ???
      case If(e1, e2, e3) =>
        ???

      case Obj(fields) =>
        ???
      case GetField(e1, f) =>
        ???

      /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(mut, x, e1, e2) =>
        ???
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            ???
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = ???
        // Match on whether the return type is specified.
        tann match {
          case None => ???
          case Some(tret) => ???
        }
      }
      case Call(e1, args) => typ(e1) match {
        case TFunction(Left(params), tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            ???
          };
          tret
        case tgot @ TFunction(Right((mode,_,tparam)), tret) =>
          ???
        case tgot => err(tgot, e1)
      }

      /***** New cases for Lab 5. ***/
      case Assign(Var(x), e1) =>
        ???
      case Assign(GetField(e1, f), e2) =>
        ???
      case Assign(_, _) => err(TUndefined, e)

      case Null =>
        ???

      case Unary(Cast(t), e1) => typ(e1) match {
        case tgot if ??? => ???
        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), "v1 in inequalityVal is not a value")
    require(isValue(v2), "v2 in inqualityVal is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case _ => ???
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    /* We removed the requirement that esub is a value to support call-by-name. */
    //require(isValue(esub), "Expr to substitute is not a value")
    /* We suggest that you add support for call-by-name last. */
    def subst(e: Expr): Expr = substitute(e, esub, x)
    val ep: Expr = ???
    ep match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
      /***** Cases from Lab 3 */
      case Unary(uop, e1) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Var(y) => ???
      /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      /***** Cases needing adapting from Lab 4 */
      case Function(p, paramse, retty, e1) =>
        ???
      /***** Cases directly from Lab 4 */
      case Call(e1, args) => ???
      case Obj(fields) => ???
      case GetField(e1, f) => ???
      /***** New case for Lab 5 */
      case Assign(e1, e2) => ???
      /***** Extra credit case for Lab 5 */
      case InterfaceDecl(tvar, t, e1) => ???

      /***** Begin Regular Expressions Extension *****/
      case RE(_) => e
      case RuntimeCall(f, args) => RuntimeCall(f, args map subst)
      /***** End Regular Expression Extension *****/
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))

    /*** Helpers for Call ***/

    def stepIfNotValue(e: Expr): Option[DoWith[Mem,Expr]] = if (isValue(e)) None else Some(step(e))

    /* Check whether or not the argument expression is ready to be applied. */
    def argApplyable(mode: PMode, arg: Expr): Boolean = mode match {
      case PVar => isValue(arg)
      case PName => true
      case PRef => isLValue(arg)
    }

    /*** Body ***/
    e match {
      /***** Begin Regular Expressions Extension *****/
      /* Do Rule */
      case Call(GetField(RE(re), "test"), List(S(s))) => doreturn( B(retest(re,s)) )
      /* Search Rule */
      case Call(e1 @ GetField(RE(re), "test"), List(e2)) =>
        step(e2) map { e2p => Call(e1, List(e2p)) }
      /* RuntimeCall Search Rule. */
      case RuntimeCall(f, args) =>
        mapFirstWith(stepIfNotValue)(args) map { argsp => RuntimeCall(f, argsp) }
      /***** End Regular Expressions Extension *****/

      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
      /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Unary(Neg, v1) if isValue(v1) => ???
      //case _ => ???
      /***** Cases needing adapting from Lab 4. Make sure to replace the case _ => ???. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        ???
      case GetField(a @ A(_), f) =>
        ???

      case Call(v1, args) if isValue(v1) =>
        def substfun(e1: Expr, p: Option[String]): Expr = p match {
          case None => e1
          case Some(x) => substitute(e1, v1, x)
        }
        (v1, args) match {
          /*** Fill-in the DoCall cases, the SearchCall2, the SearchCallVar, the SearchCallRef  ***/
          case _ => throw StuckError(e)
        }

      case Decl(MConst, x, v1, e2) if isValue(v1) =>
        ???
      case Decl(MVar, x, v1, e2) if isValue(v1) =>
        ???

      /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) =>
        ???

      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => ??? } map { _ => ??? }

      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        ???

      /* Base Cases: Error Rules */
      /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)

      /* Inductive Cases: Search Rules */
      /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) =>
        ???
      //case _ => ???
      case Call(e1, args) =>
        ???
      case Decl(mut, x, e1, e2) =>
        ???
      /***** Cases needing adapting from Lab 4 */
      case GetField(e1, f) =>
        ???
      case Obj(fields) =>
        ???

      /***** New cases for Lab 5.  */
      case Assign(e1, e2) if ??? =>
        ???
      case Assign(e1, e2) =>
        ???

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def removeInterfaceDecl(e: Expr): Expr =
  /* Do nothing by default. Change to attempt extra credit. */
    e

  /***************************/
  /*** External Interfaces ***/
  /***************************/

  var useReferenceRegExprParser = false // set to true to use the reference parser

  this.flagOptions = this.flagOptions ++ List(
    ("ref-reparser", SetBool(b => useReferenceRegExprParser = b, Some(b => useReferenceRegExprParser == b)), "using the reference regular expression parser")
  )

  // Select the parser to use based on the useReferenceRegExprParser flag
  def parser: JsyParser =
    if (useReferenceRegExprParser) new JsyParser else new JsyParser(REParser.parse)

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.

  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    }
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("Type: " + pretty(t))
    }
    t
  }

  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging.

  case class TerminationError(e: Expr) extends Exception {
    override def toString = JsyParser.formatErrorMessage(e.pos, "TerminationError", "run out of steps in evaluating " + e)
  }

  def iterateStep(e: Expr): Expr = {
    require(closed(e), "input Expr to iterateStep is not closed: free variables: %s".format(freeVars(e)))
    def loop(e: Expr, n: Int): DoWith[Mem, Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e)
      else if (isValue(e)) doreturn(e)
      else {
        for {
          m <- doget[Mem]
          _ = if (debug) {
            println("Step %s:%n  %s%n  %s".format(n, m, e))
          }
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield
          epp
      }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val (m, v) = loop(e, 0)(memempty)
    if (debug) {
      println("Result:%n  %s%n  %s".format(m, v))
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(removeInterfaceDecl(parser.parse(s)))

  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }

    val expr =
      handle(None: Option[Expr]) {
        Some {
          parser.parseFile(file)
        }
      } getOrElse {
        return
      }

    val exprlowered =
      handle(None: Option[Expr]) {
        Some {
          removeInterfaceDecl(expr)
        }
      } getOrElse {
        return
      }

    val welltyped = handle(false) {
      println("# Type checking ...")
      val t = inferType(exprlowered)
      println("## " + pretty(t))
      true
    }
    if (!welltyped) return

    handle() {
      println("# Stepping ...")
      def loop(e: Expr, n: Int): DoWith[Mem, Expr] =
        if (Some(n) == maxSteps) throw TerminationError(e)
        else if (isValue(e)) doreturn(e)
        else {
          for {
            m <- doget[Mem]
            _ = println("## %4d:%n##  %s%n##  %s".format(n, m, e))
            ep <- step(e)
            epp <- loop(ep, n + 1)
          } yield
            epp
        }
      val (m, v) = loop(exprlowered, 0)(memempty)
      println("## %s%n%s".format(m, pretty(v)))
    }
  }

} vk