package com.anton.monkey.benchmark

import com.anton.monkey.ast.Program
import com.anton.monkey.compiler.{Bytecode, Compiler}
import com.anton.monkey.evaluator.Evaluator
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.{Environment, ObjectLiteral}
import com.anton.monkey.parser.Parser
import com.anton.monkey.vm.VM
import com.anton.monkey.vm.VM.Null

import scala.concurrent.duration.Duration

class Bench {
  def interpret(program: Program, env: Environment): ObjectLiteral = {
    val evaluator = new Evaluator()
    val result = evaluator.eval(program, env)
    result
  }

  def compile(program: Program): Bytecode = {
    val compiler = Compiler.newCompiler()
    val err1 = compiler.compile(program)
    if (err1 != null) {
      println(s"compiler error $err1")
      return null
    }
    compiler.bytecode()
  }

  def run(bytecode: Bytecode): ObjectLiteral = {
    val machine = VM.newVM(bytecode)
    val err1 = machine.run()
    if (err1 != null) {
      println(s"vm error $err1")
      return null
    }
    machine.lastPoppedStackElem()
  }
}

object Bench {
  private val input = "" +
    "let fibonacci = fn(x) {" +
    "  if (x == 0) {" +
    "    return 0;" +
    "  } else {" +
    "    if (x == 1) {" +
    "      return 1;" +
    "    } else {" +
    "      fibonacci(x - 1) + fibonacci(x - 2);" +
    "    }" +
    "  }" +
    "};" +
    "fibonacci(35);"

  def main(args: Array[String]): Unit = {
    val nanosEval = exec("eval")
    val nanosVm = exec("vm")
    var nMin = math.min(nanosEval.toMillis, nanosVm.toMillis)
    if (nMin == 0) nMin = 1
    val nEval = nanosEval.toMillis / nMin
    val nVm = nanosVm.toMillis / nMin
    println(s"eval: $nEval, vm: $nVm")
  }

  def exec(engine: String): Duration = {
    val l = Lexer.New(input)
    val p = Parser.New(l)
    val program = p.parseProgram()

    var result: ObjectLiteral = Null;
    val bench = new Bench

    var duration: StopWatch = StopWatch()
    if (engine == "vm") {
      val bytecode = bench.compile(program)
      val nanosComp = duration.elapsed()
      println(s"compile, " +
        s"result: $result, duration: ${nanosComp.toMillis}")
      duration = StopWatch()
      result = bench.run(bytecode)
    } else {
      val env = Environment.NewEnvironment()
      result = bench.interpret(program, env)
    }
    val nanos = duration.elapsed()
    println(s"engine: $engine, " +
      s"result: $result, duration: ${nanos.toMillis}")
    nanos
  }
}
