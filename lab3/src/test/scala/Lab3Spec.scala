import org.scalatest._
import jsy.tester.JavascriptyTester
import jsy.lab3.ast._
import jsy.lab3.Parser.parse


import jsy.student.Lab3
import Lab3._

class FunctionCallSpec extends FlatSpec {
  "Functions" should "be considered values" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Var(x))
    val e2 = Function(Some(f), x, Var(x))
    assert(evaluate(e1) == e1)
    assert(evaluate(e2) == e2)
  }

  "Call" should "evaluate a function using big-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = evaluate(Call(e1, e2))
    assert(e3 === N(3))
  }

  "Call" should "handle recursive functions using big-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), Binary(Minus, Var(x), N(1)))))
    val e1 = Function(Some(f), x, fbody)
    val e2 = N(3)
    val e3 = evaluate(Call(e1, e2))
    assert(e3 === N(6))
  }

  "Call" should "evaluate a function using small-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = iterateStep(Call(e1, e2))
    assert(e3 === N(3))
  }

  "Call" should "handle recursive functions using small-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), Binary(Minus, Var(x), N(1)))))
    val e1 = Function(Some(f), x, fbody)
    val e2 = N(3)
    val e3 = iterateStep(Call(e1, e2))
    assert(e3 === N(6))
  }

}

class SubstituteSpec extends FlatSpec {
  "substitute" should "perform syntatic substitution respecting shadowing" in {
    val xplus1 = parse("x + 1")
    val twoplus1 = parse("2 + 1")
    assert(substitute(xplus1, N(2), "x") === twoplus1)
    val constx3 = parse("const x = 3; x")
    val shadowx = Binary(Plus, constx3, Var("x"))
    assert(substitute(shadowx, N(2), "x") === Binary(Plus, constx3, N(2)))
  }
}

class RulesSpec extends FlatSpec {
  /* Tests based on rules */

  val xval = N(2)
  val envx = extend(emp, "x", xval)
  val varx = Var("x")

  val e1 = parse("2 - 1 - 1")
  val e1p = parse("1 - 1")
  val e2 = parse("3 - 1 - 1")
  val e2p = parse("2 - 1")
  val v1 = N(0)
  val v2 = N(1)

  val vidfunction = parse("function (x) { return x }")

  "EvalVar" should "perform EvalVar" in {
    assertResult(xval) {
      eval(envx, varx)
    }
  }

  "EvalNeg" should "perform EvalNeg" in {
    val np = -toNumber(v1)
    assertResult(N(np)) {
      eval(envx, Unary(Neg, e1))
    }
  }

  "EvalTypeErrorEquality1" should "perform EvalTypeErrorEquality1" in {
    intercept[DynamicTypeError] {
      eval(envx, Binary(Eq, vidfunction, e2))
    }
  }

  "DoNeg" should "perform DoNeg" in {
    val np = -toNumber(v1)
    assertResult(N(np)) {
      step(Unary(Neg, v1))
    }
  }

  "SearchUnary" should "perform SearchUnary" in {
    assertResult(Unary(Neg, e1p)) {
      step(Unary(Neg, e1))
    }
  }
}

// The next bit of code runs a test for each .jsy file in src/test/resources/lab3.
// The test expects a corresponding .ans file with the expected result.
class Lab3JsyTests extends JavascriptyTester(None, "lab3", Lab3)

class Lab3Suite extends Suites(
  new FunctionCallSpec,
  new SubstituteSpec,
  new RulesSpec,
  new Lab3JsyTests
)
