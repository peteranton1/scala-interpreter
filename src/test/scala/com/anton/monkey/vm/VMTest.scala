package com.anton.monkey.vm

import com.anton.monkey.ast.Program
import com.anton.monkey.compiler.Compiler
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.{IntegerObj, ObjectLiteral}
import com.anton.monkey.parser.Parser
import org.scalatest.FunSuite

class VMTest extends FunSuite {

  val NULL_VALUE: Int = -9999

  case class TestStr(input: String, expected: String)

  case class TestInt(input: String, expected: Int)

  case class TestBool(input: String, expected: Boolean)


  test("Integer Arithmetic") {
    val tests = List(
      TestInt("1", 1),
      TestInt("2", 2),
      TestInt("1 + 2", 3),
      TestInt("1 - 2", -1),
      TestInt("1 * 2", 2),
      TestInt("4 / 2", 2),
      TestInt("50 / 2 * 2 + 10 - 5", 55),
      TestInt("5 + 5 + 5 + 5 - 10", 10),
      TestInt("2 * 2 * 2 * 2 * 2", 32),
      TestInt("5 * 2 + 10", 20),
      TestInt("5 + 2 * 10", 25),
      TestInt("5 * (2 + 10)", 60),
      TestInt("-5", -5),
      TestInt("-10", -10),
      TestInt("-50 + 100 + -50", 0),
      TestInt("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
    )

    runVMTestsInt(tests)
  }

  def parse(input: String): Program = {
    val l = Lexer.New(input)
    val p = Parser.New(l)
    p.parseProgram()
  }

  def getLastPopped(input: String): ObjectLiteral = {
    val program = parse(input)
    val compiler = Compiler.newCompiler()
    val err1 = compiler.compile(program)
    assert(err1 == null)
    val vm = VM.newVM(compiler.bytecode())
    val err2 = vm.run()
    assert(err2 == null)
    vm.lastPoppedStackElem()
  }

  def runVMTestsInt(tests: List[TestInt]): Unit = {
    for (tt <- tests) {
      println("Testing: " + tt.input)
      val stackElem = getLastPopped(tt.input)
      testExpectedObjectInt(tt.input, tt.expected, stackElem)
    }
  }

  def testExpectedObjectInt(input: String, expected: Int,
                            actual: ObjectLiteral): Unit = {
    actual match {
      case io: IntegerObj =>
        assert(io.value == expected)
        return
    }
    assert("object not integer: " +
      s"${actual.objType()}" == s"input=$input")
  }

}
