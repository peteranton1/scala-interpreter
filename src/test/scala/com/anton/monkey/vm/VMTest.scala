package com.anton.monkey.vm

import com.anton.monkey.ast.Program
import com.anton.monkey.compiler.Compiler
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.{ArrayObj, BooleanObj, IntegerObj, NullObj, ObjectLiteral}
import com.anton.monkey.parser.Parser
import com.anton.monkey.vm.VM.Null
import org.scalatest.FunSuite

class VMTest extends FunSuite {

  val NULL_VALUE: Int = -9999

  case class TestStr(input: String, expected: String)

  case class TestInt(input: String, expected: Int)

  case class TestIntArr(input: String, expected: List[Int])

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

  test("Boolean Expressions") {
    val tests = List(
      TestBool("true", true),
      TestBool("false", false),
      TestBool("1 < 2", true),
      TestBool("1 > 2", false),
      TestBool("1 < 1", false),
      TestBool("1 > 1", false),
      TestBool("1 == 1", true),
      TestBool("1 != 1", false),
      TestBool("1 == 2", false),
      TestBool("1 != 2", true),
      TestBool("true == true", true),
      TestBool("false == false", true),
      TestBool("true == false", false),
      TestBool("true != false", true),
      TestBool("(1 < 2) == true", true),
      TestBool("(1 < 2) == false", false),
      TestBool("(1 > 2) == true", false),
      TestBool("!true", false),
      TestBool("!false", true),
      TestBool("!5", false),
      TestBool("!!true", true),
      TestBool("!!false", false),
      TestBool("!!5", true),
      TestBool("!(if(false){ 5; })", true)
    )

    runVMTestsBool(tests)
  }

  test("Conditionals") {
    val tests = List(
      TestInt("1", 1),
      TestInt("if (true) { 10 }", 10),
      TestInt("if (true) { 10 } else { 20 }", 10),
      TestInt("if (false) { 10 } else { 20 }", 20),
      TestInt("if (1) { 10 }", 10),
      TestInt("if (1 < 2) { 10 }", 10),
      TestInt("if (1 < 2) { 10 } else { 20 }", 10),
      TestInt("if (1 > 2) { 10 } else { 20 }", 20),
      TestInt("if (1 > 2) { 10 }", NULL_VALUE),
      TestInt("if (false) { 10 }", NULL_VALUE),
      TestInt("if ((if (false) { 10 })) { 10 } else { 20 }", 20)
    )

    runVMTestsInt(tests)
  }

  test("Global let statements") {
    val tests = List(
      TestInt("let one = 1; one", 1),
      TestInt("let one = 1; let two = 2; one + two", 3),
      TestInt("let one = 1; let two = one + one; one + two", 3)
    )

    runVMTestsInt(tests)
  }

  test("String expressions") {
    val tests = List(
      TestStr("\"monkey\"", "monkey"),
      TestStr("\"mon\" + \"key\"", "monkey"),
      TestStr("\"mon\" + \"key\" + \"banana\"", "monkeybanana")
    )

    runVMTestsStr(tests)
  }

  test("Array Literals") {
    val tests = List(
      TestIntArr("[]", List()),
      TestIntArr("[1, 2, 3]", List(1, 2, 3)),
      TestIntArr("[1 + 2, 3 * 4, 5 + 6]", List(3, 12, 11))
    )

    runVMTestsIntArr(tests)
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

  def runVMTestsStr(tests: List[TestStr]): Unit = {
    for (tt <- tests) {
      println("Testing: " + tt.input)
      val stackElem = getLastPopped(tt.input)
      assert (tt.expected == stackElem.inspect())
    }
  }

  def runVMTestsInt(tests: List[TestInt]): Unit = {
    for (tt <- tests) {
      //println("Testing: " + tt.input)
      val stackElem = getLastPopped(tt.input)
      testExpectedObjectInt(tt.input, tt.expected, stackElem)
    }
  }

  def runVMTestsIntArr(tests: List[TestIntArr]): Unit = {
    for (tt <- tests) {
      println("Testing: " + tt.input)
      val stackElem = getLastPopped(tt.input)
      testExpectedObjectIntArr(tt.input, tt.expected, stackElem)
    }
  }

  def runVMTestsBool(tests: List[TestBool]): Unit = {
    for (tt <- tests) {
      val stackElem = getLastPopped(tt.input)
      testExpectedObjectBool(tt.input, tt.expected, stackElem)
    }
  }

  def testExpectedObjectInt(input: String, expected: Int,
                            actual: ObjectLiteral): Unit = {
    actual match {
      case io: IntegerObj =>
        assert(io.value == expected)
      case Null =>
        assert(expected == NULL_VALUE)
      case _ =>
        assert("object not integer: " +
          s"${actual.objType()}" == s"input=$input")
    }
  }

  def testExpectedObjectIntArr(input: String, expected: List[Int],
                            actual: ObjectLiteral): Unit = {
    actual match {
      case ao: ArrayObj =>
        assert(ao.elements.length == expected.length)
        var i = 0
        while(i < expected.length) {
          val act = ao.elements(i)
          val expect = expected(i)
          assert(act.inspect() == expect.toString)
          i += 1
        }
      case io: IntegerObj =>
        println(s"tested integer: $io")
      case _ =>
        assert("object not integer: " +
          s"${actual.objType()}" == s"input=$input")
    }
  }

  def testExpectedObjectBool(input: String, expected: Boolean,
                            actual: ObjectLiteral): Unit = {
    actual match {
      case bo: BooleanObj =>
        assert(bo.value == expected)
        return
    }
    assert("object not boolean: " +
      s"${actual.objType()}" == s"input=$input")
  }

}
