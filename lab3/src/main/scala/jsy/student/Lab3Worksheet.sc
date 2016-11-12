/*
 * CSCI 3155: Lab 3 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab3.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab3.Parser.parse

// Imports the ast nodes
import jsy.lab3.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab3._

// Parse a function
parse("function (x) { return x }")
parse("function id(x) { return x }")
