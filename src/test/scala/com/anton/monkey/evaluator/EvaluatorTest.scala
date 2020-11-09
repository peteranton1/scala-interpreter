package com.anton.monkey.evaluator

import com.anton.monkey.ast.StringLiteral
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.BooleanObj.{FALSE, TRUE}
import com.anton.monkey.objectliteral.{ArrayObj, BooleanObj, Environment, ErrorObj, FunctionObj, HashKey, HashObj, IntegerObj, NullObj, ObjectLiteral, ReturnValueObj, StringObj}
import com.anton.monkey.parser.Parser
import org.scalatest.FunSuite

class EvaluatorTest extends FunSuite {

  val NULL_VALUE: Int = -9999

  case class TestStr(input: String, expected: String)

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

  test("Error Handling") {
    val tests = List(
      TestStr(
        "5 + true;",
        "type mismatch: INTEGER + BOOLEAN"
      )
      , TestStr(
        "5 + true; 5;",
        "type mismatch: INTEGER + BOOLEAN"
      )
      , TestStr(
        "-true",
        "unknown operator: -BOOLEAN"
      )
      , TestStr(
        "true + false;",
        "type mismatch: BOOLEAN + BOOLEAN"
      )
      , TestStr(
        "5; true + false; 5",
        "type mismatch: BOOLEAN + BOOLEAN"
      )
      , TestStr(
        "if (10 > 1) { true + false; }",
        "type mismatch: BOOLEAN + BOOLEAN"
      )
      , TestStr(
        "if (10 > 1) { if (10 > 1) { return true + false; } return 1;",
        "type mismatch: BOOLEAN + BOOLEAN",
      )
      , TestStr(
        "foobar",
        "identifier not found: foobar"
      )
      , TestStr(
        "\"Hello\" - \"World\"",
        "unknown operator: STRING - STRING"
      )
      , TestStr(
        "{\"name\": \"Monkey\"}[fn(x){ x }];",
        "unusable as hash key: FUNCTION"
      )
    )

    for (tt <- tests) {
      printTTStr(tt)
      val evaluated = testEval(tt.input)
      testErrorObject(evaluated, tt.expected)
    }
  }

  test("Let Statements") {
    val tests = List(
      TestInt("let a = 5; a;", 5)
      , TestInt("let a = 5 * 5; a;", 25)
      , TestInt("let a = 5; let b = a; b;", 5)
      , TestInt("let a = 5; let b = a; let c = a + b + 5; c;", 15)
    )

    for (tt <- tests) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  test("Function Object") {
    val input = "fn(x) { x + 2; };"

    val evaluated = testEval(input)
    assert(evaluated.isInstanceOf[FunctionObj])
    val fn = evaluated.asInstanceOf[FunctionObj]
    assert(fn.parameters.length == 1)
    assert(fn.parameters.head.String() == "x")
    assert(fn.body.String() == "(x + 2)")
  }

  test("Function Application") {
    val tests = List(
      TestInt("let identity = fn(x) { x; }; identity(5);", 5)
      , TestInt("let identity = fn(x) { return x; }; identity(5);", 5)
      , TestInt("let double = fn(x) { x * 2; }; double(5);", 10)
      , TestInt("let add = fn(x, y) { x + y; }; add(5, 5);", 10)
      , TestInt("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20)
      , TestInt("fn(x) { x; }(5)", 5)
    )

    for (tt <- tests) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  test("Closures") {
    val input = "let newAdder = fn(x) { " +
      "  fn(y) { x + y };" +
      "};" +
      "let addTwo = newAdder(2);" +
      "addTwo(2);"

    val evaluated = testEval(input)
    testIntegerObject(evaluated, 4)
  }

  test("String Literals") {
    val input = "\"Hello World!\""

    val evaluated = testEval(input)
    assert(evaluated.isInstanceOf[StringObj])
    val str = evaluated.asInstanceOf[StringObj]
    assert(str.value == "Hello World!")
  }

  test("String Concatenation") {
    val input = "\"Hello\" + \" \" + \"World!\""

    val evaluated = testEval(input)
    assert(evaluated.isInstanceOf[StringObj])
    val str = evaluated.asInstanceOf[StringObj]
    assert(str.value == "Hello World!")
  }

  test("Builtin Functions") {
    val testInts = List(
      TestInt("len(\"\")", 0)
      , TestInt("len(\"four\")", 4)
      , TestInt("len(\"hello world\")", 11)
    )
    val testStrs = List(
      TestStr("len(1)", "argument to 'len' not supported, got=1")
      , TestStr("len(\"one\",\"two\")", "wrong number of arguments, got=2, want=1")
    )

    for (tt <- testInts) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
    for (tt <- testStrs) {
      printTTStr(tt)
      val evaluated = testEval(tt.input)
      assert(evaluated.isInstanceOf[ErrorObj])
      val err = evaluated.asInstanceOf[ErrorObj]
      assert(err.message == tt.expected)
    }
  }

  test("Array Literals") {
    val input = "[1, 2 * 2, 3 + 3]"

    val evaluated = testEval(input)
    assert(evaluated.isInstanceOf[ArrayObj])
    val arr = evaluated.asInstanceOf[ArrayObj]
    assert(arr.elements.length == 3)
    testIntegerObject(arr.elements.head, 1)
    testIntegerObject(arr.elements(1), 4)
    testIntegerObject(arr.elements(2), 6)
  }

  test("Array Index Expressions") {
    val tests = List(
      TestInt("let identity = fn(x) { x; }; identity(5);", 5)
      , TestInt("[1, 2, 3][0]", 1)
      , TestInt("[1, 2, 3][1]", 2)
      , TestInt("[1, 2, 3][2]", 3)
      , TestInt("let i = 0; [1][i];", 1)
      , TestInt("[1, 2, 3][1 + 1];", 3)
      , TestInt("let myArray = [1, 2, 3]; myArray[2];", 3)
      , TestInt("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", 6)
      , TestInt("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];", 2)
      , TestInt("[1, 2, 3][3]", NULL_VALUE)
      , TestInt("[1, 2, 3][-1]", NULL_VALUE)
    )

    for (tt <- tests) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  test("Hash Literals") {
    val input = "" +
      "let two = \"two\";" +
      "{" +
      "\"one\": 10 - 9," +
      "two: 1 + 1," +
      "\"thr\" + \"ee\": 6 / 2," +
      "4: 4," +
      "true: 5," +
      "false: 6" +
      "}"

    val evaluated = testEval(input)
    assert(evaluated.isInstanceOf[HashObj])
    val arr = evaluated.asInstanceOf[HashObj]
    val expectedMap: Map[HashKey,Int] = Map(
      StringObj("one").hashKey() -> 1,
      StringObj("two").hashKey() -> 2,
      StringObj("three").hashKey() -> 3,
      IntegerObj(4).hashKey() -> 4,
      TRUE.hashKey() -> 5,
      FALSE.hashKey() -> 6
    )
    arr.pairs.foreachEntry((hashKey, hashPair) => {
      val expected = expectedMap(hashKey)
      println(s"actual: ${hashPair.key}: ${hashPair.value}" +
        s", expected: ${expected}")
      testIntegerObject(hashPair.value, expected)
    })
  }

  test("Hash Index Expressions") {
    val tests = List(
      TestInt("let identity = fn(x) { x; }; identity(5);", 5)
      , TestInt("{\"foo\": 5}[\"foo\"]", 5)
      , TestInt("{\"foo\": 5}[\"bar\"]", NULL_VALUE)
      , TestInt("let key = \"foo\"; {\"foo\": 5}[key]", 5)
      , TestInt("{}[\"bar\"]", NULL_VALUE)
      , TestInt("{5: 5}[5]",5)
      , TestInt("{true: 5}[true]", 5)
      , TestInt("{false: 5}[false]", 5)
    )

    for (tt <- tests) {
      printTTInt(tt)
      val evaluated = testEval(tt.input)
      testIntegerObject(evaluated, tt.expected)
    }
  }

  def printTTStr(tt: TestStr): Unit = {
    println(s"input: ${tt.input}, expected: ${tt.expected}")
  }

  def printTTInt(tt: TestInt): Unit = {
    println(s"input: ${tt.input}, expected: ${tt.expected}")
  }

  def printTTBool(tt: TestBool): Unit = {
    println(s"input: ${tt.input}, expected: ${tt.expected}")
  }

  def testErrorObject(obj: ObjectLiteral,
                      expected: String) {
    val result = obj.asInstanceOf[ErrorObj]
    assert(result != null)
    assert(result.message == expected)
  }

  def testBooleanObject(obj: ObjectLiteral,
                        expected: Boolean) {
    val result = obj.asInstanceOf[BooleanObj]
    assert(result != null)
    assert(result.value == expected)
  }

  def testIntegerObject(obj: ObjectLiteral, expected: Int) {
    obj match {
      case err0: ErrorObj =>
        assert(expected == NULL_VALUE)
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
