import org.scalatest._
import jsy.tester.JavascriptyTester

import jsy.lab6.ast._
import jsy.student.Lab6
import Lab6._

import scala.util.matching.Regex

object Lab6Harness {

  def regExprToRegex(re: RegExpr): Regex = {
    def toR(re: RegExpr): String = re match {
      case REmptyString => ""
      case RSingle(c) => c.toString
      case RConcat(re1, re2) => toR(re1) + toR(re2)
      case RUnion(re1, re2) => s"(${toR(re1)}|${toR(re2)})"
      case RStar (re1) => s"(${toR(re1)})*"
      case RAnyChar => "."
      case RPlus(re1) => s"(${toR(re1)})+"
      case ROption(re1) => s"(${toR(re1)})?"
      case _ => throw new IllegalArgumentException(s"Unsupported RegExpr ${re}")
    }
    new Regex(raw"\A" + toR(re) + raw"\z")
  }

  def retestRegex(re: Regex, str: String): Boolean =
    !re.findPrefixOf(str).isEmpty

}

class RegExprSpec extends FlatSpec {
  import Lab6Harness._

  val strings = List(
     "",
     "a",
     "aa",
     "ab",
     "aaa"
  )

  val respecs = List(
    "a",
    "b",
    ".",
    "aa",
    "(aa)*"
  )
  
  val respecsast = respecs map { s => jsy.lab6.RegExprParser.parse(s) }


  behavior of "parse"

  for ((restr,re) <- (respecs,respecsast).zipped) {
    it should s"on '${restr}' produce a RegExpr AST matching the reference" in {
      assertResult(re) { REParser.parse(restr)}
    }
  }


  behavior of "retest"

  // Note that this testing uses Scala's regular expression matcher, which does not
  // support !, &, or ~.
  for (re <- respecsast) {
    for (s <- strings) {
      it should s"test '${s}' on '${pretty(re)}'" in {
        val regex = regExprToRegex(re)
        assertResult(retestRegex(regex, s)) { retest(re, s) }
      }
    }
  }
  
}

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab6JsyTests extends JavascriptyTester(None, "lab6", Lab6)

class Lab6Suite extends Suites(
  new RegExprSpec,
  new Lab6JsyTests
)