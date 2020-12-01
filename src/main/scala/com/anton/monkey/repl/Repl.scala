package com.anton.monkey.repl

import java.io.InputStream
import java.util.Scanner

import com.anton.monkey.ast.Program
import com.anton.monkey.evaluator.Evaluator
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.{Environment, ObjectLiteral}
import com.anton.monkey.parser.Parser

class Repl {
  val PROMPT = ">> "

  def interpret(program: Program, env: Environment): ObjectLiteral = {
    val evaluator = new Evaluator()
    val result = evaluator.eval(program, env)
    result
  }

  def start(in: InputStream): Unit = {
    val scanner = new Scanner(in)
    val env = Environment.NewEnvironment()

    val finished = false
    while (!finished) {
      print(PROMPT)
      val input = scanner.nextLine()
      if("quit" == input) {
        println(" [Quit]")
        return
      }
      val l = Lexer.New(input)
      val p = Parser.New(l)
      val program = p.parseProgram()
      print(program.String())
      if (program.statements.length < 1) {
        println(" [Error]")
        return
      } else {
        print(" [OK] = ")
        val result = interpret(program, env)
        println(s"$result")
      }
    }
  }
}

object Repl {
  def main(args: Array[String]) {
    val repl = new Repl()
    repl.start(System.in)
  }
}