package jsy.lab6

import jsy.lab6.ast._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

object RegExprParser extends Parsers {
  override type Elem = Char

  val delimiters = Set('|', '&', '~', '*', '+', '?', '!', '#', '.', '(', ')')

  def re: Parser[RegExpr] =
    union
    
  def union: Parser[RegExpr] =
    intersect * ('|' ^^ (_ => RUnion))
  
  def intersect: Parser[RegExpr] =
    concat * ('&' ^^ (_ => RIntersect))
    
  def concat: Parser[RegExpr] =
    not.+ ^^ { rs => rs.reduceLeft(RConcat) }
        
  def not: Parser[RegExpr] =
    '~' ~> not ^^ RNeg |
    star
    
  def star: Parser[RegExpr] = {
    val op = '*' ^^ (_ => RStar) | '+' ^^ (_ => RPlus) | '?' ^^ (_ => ROption)
    atom ~ rep(op) ^^ { case r ~ ops => ops.foldLeft(r){ (acc,op) => op(acc) } }
  }

  def atom: Parser[RegExpr] = {
    val a =
      '!' ^^ (_ => RNoString) |
      '#' ^^ (_ => REmptyString) |
      '.' ^^ (_ => RAnyChar) |
      '(' ~> re <~ ')' ^^ (r => r) |
      acceptIf(c => !delimiters.contains(c))(_ => "expected atom") ^^ { c => RSingle(c) }
    Parser { next => if (next.atEnd) Failure("expected atom", next) else a(next) }
  }

  /*** External Interface ***/
  
  def regexp: Parser[RegExpr] = re
    
  def parse(next: Input): RegExpr = phrase(regexp)(next) match {
    case Success(r, _) => r
    case NoSuccess(msg, next) => throw SyntaxError(msg, next.pos)
  }

  def parse(s: String): RegExpr = parse(new CharSequenceReader(s))
}