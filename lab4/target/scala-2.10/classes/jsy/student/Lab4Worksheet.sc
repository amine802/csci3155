/*
 * CSCI 3155: Lab 4 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab4.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab4.Parser.parse

// Imports the ast nodes
import jsy.lab4.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab4._

// Try compressRec
//val cr1 = compressRec(List(1, 2, 2, 3, 3, 3))

// Parse functions with possibly multiple parameters and type annotations.
//parse("function fst(x: number, y: number): number { return x }")
//parse("function (x: number) { return x }")
//parse("function (f: (y: number) => number, x: number) { return f(x) }")

// Parse objects
//parse("{ f: 0, g: true }")
//parse("x.f")
List(1,2,3)
val list3 = 1 :: 2 :: 3 :: Nil
def tail[T](xs: List[T]): T = {
  require(xs != Nil)
  xs match{
    case x :: Nil => x
    case _ :: xs1 => tail(xs1)
  }
}
def penultimate[T](xs: List[T]): T = {
  require(xs != Nil)
  xs match {
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
  }
}

def nth[T](n: Int, xs: List[T]): T = {
  require(xs != Nil)
  xs match {
    case x :: _ if(n == 0) => x
    case _ :: xs1 => nth(n-1, xs1)
  }
}

// Mapping ============================================================

// Some fun functions for map!

def timesTwo(num: Int): Int = num * 2
def appendYes(str: String): String = str + "Yes"

// Multiply every value in a list by some factor
def scaleList(list: List[Double], factor: Double): List[Double] = list match {
  case Nil => list
  case head::tail => head*factor::scaleList(tail, factor)
}

// Apply some function to every element in a list
def ourMap[A](f: A=>A)(l: List[A]): List[A] = l match {
  case Nil => Nil
  case head::tail => f(head)::ourMap(f)(tail)
}

// Folding =============================================================

// Some fun functions for fold
def times(num1: Int,num2: Int):Int = {num1*num2}
def concat(num1: Int,str1: String):String = str1 + num1

// Multiply everything in a list together and return sum
def productList(list: List[Double]): Double = list match {
  case Nil => 1
  case head::tail => head * productList(tail)
}

// Tail Recursive Product
def productListTail(list: List[Double], acc: Double): Double = list match {
  case Nil => acc
  case head::tail => productListTail(tail, head * acc)
}

// Accumulate using provided function
//def ourFoldLeft[A,B](list: List[A], acc:B)(f:(A,B)=>B):B = list match {
  //case Nil => acc
  //case head::tail => ourFoldLeft(tail, f(acc, head))(f)
//}

def compressRec[A](l: List[A]): List[A] = l match {
  case Nil | _ :: Nil => return l;
  case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1)
}


//val list = List(1, 1, 2, 2, 2, 3, 4, 5)
//compressRec(list)

val list1 = List(1,2,3)
val list2 = List(("a", "b"), ("c", "d"), ("e", "f"))
val list5 = List(1,2,3,6,7,8)

def f(acc:String, x:Int): String = acc + x
def g(x:Int, acc:String): String = x + acc
list1.foldLeft("")(f)
list1.foldRight("")(g)

val list4 = (list1, list2).zipped
