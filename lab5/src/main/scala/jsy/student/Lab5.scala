package jsy.student

object Lab5 extends jsy.util.JsyApplication {
  import jsy.lab5.ast._
  import jsy.lab5._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * Sami Meharzi
   *
   * Partner: <Nolawee Mengist>
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

  /*** Exercise with DoWith ***/

  def fresh: DoWith[Int,String] = doget flatMap { i =>
    val xp = "x" + i
    doput(i + 1) map { _ => xp }
  }

  def rename(env: Map[String, String], e: Expr): DoWith[Int,Expr] = {
    def ren(e: Expr): DoWith[Int,Expr] = rename(env, e)
    e match {
      case N(_) => doreturn(e)
      case Binary(Plus, e1, e2) => ren(e1) flatMap(e1p => ren(e2) map(e2p => Binary(Plus, e1p, e2p)))
      case Var(x) => doreturn(Var(env(x)))
      case Decl(MConst, x, e1, e2) => fresh flatMap{
        xp => rename(env + (x->xp), e1) flatMap{
          e1p => rename(env + (x->xp), e2) map {
            e2p => Decl(MConst, xp, e1p, e2p)
          }
        }
      }
      /* For this exercise, no need to implement any more cases than the ones above.
       * Leave the following default case. */
      case _ => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  def rename(e: Expr): Expr = {
    val (_, r) = rename(Map.empty, e)(0)
    r
  }

  def rename(s: String): Expr = rename(Parser.parse(s))

  /*** Helper: mapFirst to DoWith ***/

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](f: A => Option[DoWith[W,A]])(l: List[A]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(l)
    case h :: t => f(h) match {
      case Some(a) => a map {
        a2 => a2 :: t
      }
      case None => mapFirstWith(f)(t) map {
        l => h :: l
      }
    }
  }

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    /***** Make sure to replace the case _ => ???. */
    //case _ => ???
    case (t1, t2) if(t1 == t2) => true
    case (TNull, TObj(_)) => true
    case (TObj(f1), TObj(f2)) => {    //may be issue!!!!
    val v1 = f2 forall {case (fieldName, ty) =>
        f1.get(fieldName) match {
          case Some(t2) => if(ty == t2) true else false
          case None => false
        }
      }
      val v2 = f1 forall { case (fieldName, ty) =>
        f2.get(fieldName) match {
          case Some(t2) => if(ty ==t2) true else false
          case None => v1
        }
      }
      v1 || v2
    }


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

      /** *** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }

      case Binary(Plus, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case ((TString | TNumber), tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }

      case Binary(Minus | Times | Div, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }

      case Binary(Eq | Ne, e1, e2) => {
        val t1 = typ(e1)
        val t2 = typ(e2)
        if (hasFunctionTyp(t1)) {
          err(t1, e1)
        }
        if (hasFunctionTyp(t2)) {
          err(t2, e2)
        }
        if (t1 != t2) {
          err(t2, e2)
        }
        TBool
      }

      case Binary(Lt | Le | Gt | Ge, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case ((TNumber | TString), tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }

      case Binary(And | Or, e1, e2) => typ(e1) match {
        case TBool => typ(e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }

      case Binary(Seq, e1, e2) => {
        val t1 = typ(e1)
        val t2 = typ(e2)
        t2
      }

      case If(e1, e2, e3) => typ(e1) match {
        case TBool => {
          val t2 = typ(e2)
          val t3 = typ(e3)
          if (t2 == t3) {
            t2
          }
          else {
            err(t3, e3)
          }
        }
        case tgot => err(tgot, e1)
      }

      case Obj(fields) => {
        TObj(fields.map({ case (str, e1) => (str, typ(e1)) }))
      }

      case GetField(e1, f) => {
        val t = typ(e1)
        t match {
          case TObj(fields) => fields.get(f) match {
            case None => err(t, e1)
            case Some(v) => v
          }
          case _ => err(t, e1)
        }
      }

      /** *** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(mut, x, e1, e2) => {
        typeInfer(extend(env, x, (mut, typ(e1))), e2)
      }

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            extend(env, f, (MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params match {
          case Left(paramsList) => paramsList.foldLeft(env1) {
            case (new_env, (param_name, param_type)) => extend(new_env, param_name, (MConst, param_type))
          }

          case Right((mode, param_name, param_t)) => mode match {
            //mode is pass by reference, variable, or name
            case PName => extend(env1, param_name, (MConst, param_t))
            case _ => extend(env1, param_name, (MVar, param_t))
          }
        }
        // Match on whether the return type is specified.
        tann match {
          case None => TFunction(params, typeInfer(env2, e1))
          case Some(tret) => {
            val inferred = typeInfer(env2, e1)
            if (inferred == tret) TFunction(params, typeInfer(env2, e1)) else err(inferred, e1) //!!!!!!!!!!
          }
        }
      }
      case Call(e1, args) => typ(e1) match {
        case TFunction(Left(params), tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            case ((_, param_type), argument) => if (param_type != typ(argument)) err(param_type, argument) else typ(argument)
          };
          tret
        case tgot@TFunction(Right((mode, _, tparam)), tret) if(args.length == 1) => mode match {
          case PName | PVar => {
            val headType = typ(args.head)
            if (headType != tparam) err(headType, args.head) else tret
          }
          case PRef if isLExpr(args.head) => {
            val headType = typ(args.head)
            if (headType != tparam) err(headType, args.head) else tret
          }
          case _ => err(typ(args.head), args.head)
        }

        case tgot => err(tgot, e1)
      }

      /** *** New cases for Lab 5. ***/
      case Assign(Var(x), e1) => {
        env.get(x) match {
          case Some((m, t)) if m == MVar && t == typ(e1) => t
          case _ => err(typ(e1), e1)
        }
      }

      case Assign(GetField(e1, f), e2) => {
        val t1 = typ(e1)
        t1 match {
          case TObj(field_types) => field_types.get(f) match {
            case Some(field_type) if (field_type == typ(e2)) => field_type
            case _ => err(typ(e1), e1)
          }
          case _ => err(typ(e), e)
        }
      }

      case Assign(_, _) => err(TUndefined, e)

      case Null => TNull


      case Unary(Cast(t), e1) => typ(e1) match {
        case tgot if castOk(tgot, t) => t
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
    val ep: Expr = avoidCapture(freeVars(esub), e)
    ep match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
      /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (y == x) esub else e
      /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      /***** Cases needing adapting from Lab 4 */
      case Function(p, paramse, retty, e1) => paramse match {
        case Left(params) => {
          val body = params.foldLeft(e1) {
            (e1, param) => param match {
              case (param_name, param_type) => if(param_name != x && p != Some(x)) subst(e1) else e1
            }
          }
          Function(p, Left(params), retty, body)
        }
        case Right((param_mode, param_name, param_type)) => {
          val body = if(param_name != x && p != Some(x)) subst(e1) else e1
          Function(p, Right((param_mode, param_name, param_type)), retty, body)
        }
      }
      /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(subst(e1), args.map(arg => subst(arg))) //applying the function to every argument in the map
      case Obj(fields) => {
        Obj(fields.foldLeft(Map(): Map[String, Expr]){
          (acc: Map[String, Expr], m1: (String, Expr)) => m1 match {
            case (s1, e2) => acc + (s1 -> subst(e2))
          }
        })
      }
      case GetField(e1, f) => GetField(subst(e1), f)
      /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))
      /***** Extra credit case for Lab 5 */
      //case InterfaceDecl(tvar, t, e1) => ???
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
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
      /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Unary(Neg, N(n)) => doreturn(N(-n))
      case Unary(Not, B(b)) => doreturn(B(!b))
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn(e2)

      case Binary(Plus, N(n1), N(n2)) => doreturn(N(n1 + n2))
      case Binary(Plus, S(s1), S(s2)) => doreturn(S(s1 + s2))
      case Binary(Minus, N(n1), N(n2)) => doreturn(N(n1 - n2))
      case Binary(Times, N(n1), N(n2)) => doreturn(N(n1 * n2))
      case Binary(Div, N(n1), N(n2)) => doreturn(N(n1 / n2))

      case Binary(Lt, N(n1), N(n2)) => doreturn(B(n1 < n2))
      case Binary(Le, N(n1), N(n2)) => doreturn(B(n1 <= n2))
      case Binary(Gt, N(n1), N(n2)) => doreturn(B(n1 > n2))
      case Binary(Ge, N(n1), N(n2)) => doreturn(B(n1 >= n2))

      case Binary(Lt, S(s1), S(s2)) => doreturn(B(s1 < s2))
      case Binary(Le, S(s1), S(s2)) => doreturn(B(s1 <= s2))
      case Binary(Gt, S(s1), S(s2)) => doreturn(B(s1 > s2))
      case Binary(Ge, S(s1), S(s2)) => doreturn(B(s1 >= s2))

      case Binary(Eq, v1, v2) if(isValue(v1) && isValue(v2)) => doreturn(B(v1 == v2))
      case Binary(Ne, v1, v2) if(isValue(v1) && isValue(v2)) => doreturn(B(v1 != v2))

      case Binary(And, B(true), e2) => doreturn(e2)
      case Binary(And, B(false), e2) => doreturn(B(false))
      case Binary(Or, B(true), e2) => doreturn(B(true))
      case Binary(Or, B(false), e2) => doreturn(e2)

      case If(B(true), e2, e3) => doreturn(e2)
      case If(B(false), e2, e3) => doreturn(e3)



      //case _ => ???
      /***** Cases needing adapting from Lab 4. Make sure to replace the case _ => ???. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) => memalloc(e)

      case GetField(a @ A(_), f) => {
        doget.map {
          m => m.get(a) match {
            case Some(Obj(fields)) => fields.get(f) match {
              case Some(field) => field
              case _ => throw StuckError(e)
            }
            case _ => throw StuckError(e)
          }
        }
      }

      case Call(v1, args) if isValue(v1) =>
        def substfun(e1: Expr, p: Option[String]): Expr = p match {
          case None => e1
          case Some(x) => substitute(e1, v1, x)
        }
        (v1, args) match {
          /*** Fill-in the DoCall cases, the SearchCall2, the SearchCallVar, the SearchCallRef  ***/
          case (Function(name, Left(params), _, e1), args) if (params.length == args.length) && (args forall isValue) => {
            val e1p = (params, args).zipped.foldRight(e1){
              (param, acc) => param match {
                case ((param_name, _), value) => substitute(acc, value, param_name)
              }
            }
            name match {
              case Some(xp) => doreturn(substitute(e1p, v1, xp))
              case None => doreturn(e1p)
            }
            doreturn(substfun(e1p, name))
          }
          case (Function(name, Right((PName, x1, _)), tann, e1), arg :: Nil) =>  {
            doreturn(substfun(substitute(e1, arg, x1), name))
          }

          case (Function(name, Right((PVar, x1, _)), tann, e1), arg :: Nil) if isValue(arg) => {
            memalloc(arg) map {
              address => substfun(substitute(e1, Unary(Deref, address), x1), name)
            }
          }

          case (Function(name, Right((PRef, x1, _)), tann, e1), lv2 :: Nil) if isLValue(lv2) => {
            doreturn(substfun(substitute(e1, lv2, x1), name))
          }

          //Search Call
          case (Function(name,  Right((PVar, x1, _)), tann, e1), e2 :: Nil) => {
            step(e2) map {
              e2p => Call(v1, e2p :: Nil)
            }
          }

          case (Function(name, Right((PRef, x1, _)), tann, e1), e2 :: Nil) if(!isLValue(e2)) => {
            step(e2) map {
              e2p => Call(v1, e2p :: Nil)
            }
          }

          case (Function(name, Left(params), tann, e1), e2) => {
            mapFirstWith(stepIfNotValue)(e2) map {
              e2p => Call(v1, e2p)
            }
          }
          case _ => throw StuckError(e)
        }

      //map you get a expression
      //flatmap you get a dowith

      case Decl(MConst, x, v1, e2) if isValue(v1) => doreturn(substitute(e2, v1, x))
      case Decl(MVar, x, v1, e2) if isValue(v1) => memalloc(v1) map (address => substitute(e2, Unary(Deref, address), x))

      /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) => {
        doget map {
          m => m.get(a) match {
            case Some(v1) => v1
            case _ => ???
          }
        }
      }

      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m + (a -> v) } map { _ => v }

      // assign an address to an object
      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        domodify[Mem] {
          m => m.get(a) match {
            case Some(Obj(fields)) => fields.get(f) match {
              case Some(field) => m + (a -> Obj(fields + (f -> v)))
              case _ => ???
            }
            case Some(Null) => throw new NullDereferenceError(e)
            case _ => ???
          }
        } map { _ => v }


      /* Base Cases: Error Rules */
      /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)
      case Unary(Cast(TObj(_)), Null)  => doreturn(Null)
      case Unary(Cast(t), a @ A(_)) => {
        t match {
          case TObj(fields) => {
            doget map {
              m => m.get(a) match {
                case Some(Obj(fields2)) =>
                  val fi = fields forall {
                    case (fieldname, _) => fields2.get(fieldname) match {
                      case Some(f) => true
                      case None => false
                    }
                  }
                  if (fi) a
                  else {
                    throw new DynamicTypeError(e)
                  }
                case _ => ???
              }
            }
          }
          case _ => ???
        }
      }
      case Unary(Cast(t), v1) if(isValue(v1)) => doreturn(v1)
      /* Inductive Cases: Search Rules */
      /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) => step(e1) map { e1p => Unary(uop, e1p)}
      case Binary(bop, v1, e2) if(isValue(v1)) => step(e2) map { e2p => Binary(bop, v1, e2p)}
      case Binary(bop, e1, e2) => step(e1) map { e1p => Binary(bop, e1p, e2)}
      case If(e1, e2, e3) => step(e1) map { e1p => If(e1p, e2, e3)}
      //case _ => ???
      case Call(e1, args) => step(e1) map { e1p => Call(e1p, args)}

      case Decl(mut, x, e1, e2) => step(e1) map { e1p =>  Decl(mut, x, e1p, e2)}

      /***** Cases needing adapting from Lab 4 */
      case GetField(e1, f) => {
        if (e1 == Null) throw new NullDereferenceError(e)
        step(e1) map { e1p => GetField(e1p, f)}
      }

      case Obj(fields) => fields find {
        case (_, ei) => !isValue(ei) } match {
        case Some((fi, ei)) => step(ei) map { eip => Obj(fields + (fi -> eip)) }
      }

      /***** New cases for Lab 5.  */
      case Assign(e1, e2) if(!isLValue(e1))  => step(e1) map { e1p => Assign(e1p, e2) }
      case Assign(e1, e2) => step(e2) map { e2p => Assign(e1, e2p) }

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def removeInterfaceDecl(e: Expr): Expr =
  /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

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
    override def toString = Parser.formatErrorMessage(e.pos, "TerminationError", "run out of steps in evaluating " + e)
  }

  def iterateStep(e: Expr): Expr = {
    require(closed(e), "input Expr to iterateStep is not closed: free variables: %s".format(freeVars(e)) )
    def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e)
      else if (isValue(e)) doreturn( e )
      else {
        for {
          m <- doget[Mem]
          _ = if (debug) { println("Step %s:%n  %s%n  %s".format(n, m, e)) }
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield
          epp
      }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val (m,v) = loop(e, 0)(memempty)
    if (debug) {
      println("Result:%n  %s%n  %s".format(m,v))
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(removeInterfaceDecl(jsy.lab5.Parser.parse(s)))

  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }

    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab5.Parser.parseFile(file)
      }} getOrElse {
        return
      }

    val exprlowered =
      handle(None: Option[Expr]) {Some{
        removeInterfaceDecl(expr)
      }} getOrElse {
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
      def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
        if (Some(n) == maxSteps) throw TerminationError(e)
        else if (isValue(e)) doreturn( e )
        else {
          for {
            m <- doget[Mem]
            _ = println("## %4d:%n##  %s%n##  %s".format(n, m, e))
            ep <- step(e)
            epp <- loop(ep, n + 1)
          } yield
            epp
        }
      val (m,v) = loop(exprlowered, 0)(memempty)
      println("## %s%n%s".format(m,pretty(v)))
    }
  }

}