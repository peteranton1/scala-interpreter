package com.anton.monkey.evaluator

import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.{BooleanObj, Environment, IntegerObj, NullObj, ObjectLiteral, ReturnValueObj}
import com.anton.monkey.parser.Parser
import org.scalatest.FunSuite

class EvaluatorTest extends FunSuite {

  case class TestInt(input: String, expected: Int)

  case class TestBool(input: String, expected: Boolean)

  test("Integer Expression") {
    val tests = List(
      TestInt("5", 5)
      , TestInt("10", 10)
      , TestInt("-5", -5)
      , TestInt("-10", -10)
      , TestInt("5 + 5 + 5 + 5 - 10", 10)
      , TestInt("2 * 2 * 2 * 2 * 2", 32)
      , TestInt("-50 + 100 + -50", 0)
      , TestInt("5 * 2 + 10", 20)
      , TestInt("5 + 2 * 10", 25)
      , TestInt("20 + 2 * -10", 0)
      , TestInt("50 / 2 * 2 + 10", 60)
      , TestInt("2 * (5 + 10)", 30)
      , TestInt("3 * 3 * 3 + 10", 37)
      , TestInt("3 * (3 * 3) + 10", 37)
      , TestInt("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
    )

    for (tt <- tests) {
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  test("Boolean Expression") {
    val tests = List(
      TestBool("true", expected = true)
      , TestBool("false", expected = false)
      , TestBool("1 < 2", expected = true)
      , TestBool("1 > 2", expected = false)
      , TestBool("1 < 1", expected = false)
      , TestBool("1 > 1", expected = false)
      , TestBool("1 == 1", expected = true)
      , TestBool("1 != 1", expected = false)
      , TestBool("1 == 2", expected = false)
      , TestBool("1 != 2", expected = true)
      , TestBool("true == true", expected = true)
      , TestBool("false == false", expected = true)
      , TestBool("true == false", expected = false)
      , TestBool("true != false", expected = true)
      , TestBool("false != true", expected = true)
      , TestBool("(1 < 2) == true", expected = true)
      , TestBool("(1 < 2) == false", expected = false)
      , TestBool("(1 > 2) == true", expected = false)
      , TestBool("(1 > 2) == false", expected = true)
    )

    for (tt <- tests) {
      val evaluated = testEval(tt.input)
      testBooleanObject(evaluated, tt.expected)
    }
  }

  test("Bang Operator") {
    val tests = List(
      TestBool("true", expected = true)
      , TestBool("!true", expected = false)
      , TestBool("!false", expected = true)
      , TestBool("!5", expected = false)
      , TestBool("!!true", expected = true)
      , TestBool("!!false", expected = false)
      , TestBool("!!5", expected = true)
      , TestBool("!!", expected = false)
    )

    for (tt <- tests) {
      val evaluated = testEval(tt.input)
      printTTBool(tt)
      testBooleanObject(evaluated, tt.expected)
    }
  }

  val NULL_VALUE: Int = -9999

  test("If Else Expressions") {
    val tests = List(
      TestInt("if (true) { 10 }", 10)
      , TestInt("if (false) { 10 }", NULL_VALUE)
      , TestInt("if (1) { 10 }", 10)
      , TestInt("if (1 < 2) { 10 }", 10)
      , TestInt("if (1 > 2) { 10 }", NULL_VALUE)
      , TestInt("if (1 > 2) { 10 } else { 20 }", 20)
      , TestInt("if (1 < 2) { 10 } else { 20 }", 10)
    )

    for (tt <- tests) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  test("Return Statement") {
    val tests = List(
      TestInt("return 10;", 10)
      , TestInt("return 10; 9;", 10)
      , TestInt("return 2 * 5; 9;", 10)
      , TestInt("9; return 2 * 5; 9;", 10)
      , TestInt("if (10 > 1) {" +
        "if (10 > 1) {" +
        "return 10;" +
        "}" +
        "return 1;" +
        "}", 10)
      , TestInt("if (10 > 1) {" +
        "if (10 < 1) {" +
        "return 10;" +
        "}" +
        "return 1;" +
        "}", 1)
    )

    for (tt <- tests) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  def printTTInt(tt: TestInt): Unit = {
    println(s"input: ${tt.input}, expected: ${tt.expected}")
  }

  def printTTBool(tt: TestBool): Unit = {
    println(s"input: ${tt.input}, expected: ${tt.expected}")
  }

  def testBooleanObject(obj: ObjectLiteral,
                        expected: Boolean) {
    val result = obj.asInstanceOf[BooleanObj]
    assert(result != null)
    assert(result.value == expected)
  }

  def testIntegerObject(obj: ObjectLiteral, expected: Int) {
    obj match {
      case _: NullObj =>
        assert(expected == NULL_VALUE)
      case ret0: ReturnValueObj =>
        testIntegerObject(ret0.value, expected)
      case int0: IntegerObj =>
        assert(int0.value == expected)
    }
  }

  def testEval(input: String): ObjectLiteral = {
    val l = Lexer.New(input)
    val p = Parser.New(l)
    val program = p.parseProgram()
    val env = Environment.NewEnvironment()
    val evaluator = new Evaluator()
    val result = evaluator.eval(program, env)
    result
  }

}
