/*
 * CSCI 3155: Lab 5 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab5.scala.
 */

// Imports the ast nodes
import jsy.lab6._
import jsy.lab6.ast._

import scala.util.Success
import scala.util.parsing.combinator.Parsers.ParseResult
import scala.util.parsing.combinator.Parsers.ParseResult

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab6._

// Jsy parser using the reference RegExpr parser.
val refparser = (new JsyParser)

// Jsy parser using the your RegExpr parser.
val yourparser = new JsyParser(REParser.parse)

// Try out the reference RegExpr parser
val re1 = RegExprParser.parse("a")
refparser.parse("/^a$/")

// Try out your RegExpr parser
//REParser.parse("a")
//yourparser.parse("/^a$/")

// Try out your matcher
//retest(re1, "a")

/*def union(next: Input): ParseResult[RegExpr] = intersect(next) match {
  case Success(r, next) => {
    def unions(acc: RegExpr, next: Input): ParseResult[RegExpr] =
      if (next.atEnd) Success(acc, next)
      else (next.first, next.rest) match {
        case('|', next) => intersect(next) match {
          case Success(r, next) => unions(RUnion(acc,r), next)
          case ???
        }
      }
  }
}*/

def append[T](xl: List[T], yl: List[T]): List[T] = xl match {
  case Nil => yl
  case xh :: xt => xh::append(xt,yl)
}

def appendContinuation[T](xl: List[T], yl: List[T]): List[T] = {
  def app(xl: List[T])(sc: List[T] => List[T]): List[T] = xl match {
    case Nil => sc(yl)
    case xh :: xt => app(xt) {r => sc(xh::r)}
  }
  app(xl) {r=>r}
}