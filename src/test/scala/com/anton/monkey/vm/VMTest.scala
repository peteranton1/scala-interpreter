package com.anton.monkey.vm

import com.anton.monkey.ast.Program
import com.anton.monkey.compiler.Compiler
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral._
import com.anton.monkey.parser.Parser
import com.anton.monkey.vm.VM.Null
import org.scalatest.FunSuite

import scala.collection.mutable

class VMTest extends FunSuite {

  val NULL_VALUE: Int = -9999

  case class TestHash(input: String, expected: HashObj)

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
      TestBool("true", expected = true),
      TestBool("false", expected = false),
      TestBool("1 < 2", expected = true),
      TestBool("1 > 2", expected = false),
      TestBool("1 < 1", expected = false),
      TestBool("1 > 1", expected = false),
      TestBool("1 == 1", expected = true),
      TestBool("1 != 1", expected = false),
      TestBool("1 == 2", expected = false),
      TestBool("1 != 2", expected = true),
      TestBool("true == true", expected = true),
      TestBool("false == false", expected = true),
      TestBool("true == false", expected = false),
      TestBool("true != false", expected = true),
      TestBool("(1 < 2) == true", expected = true),
      TestBool("(1 < 2) == false", expected = false),
      TestBool("(1 > 2) == true", expected = false),
      TestBool("!true", expected = false),
      TestBool("!false", expected = true),
      TestBool("!5", expected = false),
      TestBool("!!true", expected = true),
      TestBool("!!false", expected = false),
      TestBool("!!5", expected = true),
      TestBool("!(if(false){ 5; })", expected = true)
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
      TestStr("\"mon\" + \"key\" + \" banana\"", "monkey banana")
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

  test("Hash Literals") {
    val int_1 = IntegerObj(1)
    val int_2 = IntegerObj(2)
    val int_3 = IntegerObj(3)
    val int_4 = IntegerObj(4)
    val int_6 = IntegerObj(6)
    val int_16 = IntegerObj(16)
    val tests = List(
      TestHash("{}",
        HashObj(pairs = mutable.Map[HashKey, HashPair]())),
      TestHash("{1: 2, 2: 3}",
        HashObj(pairs = mutable.Map(
          int_1.hashKey() -> HashPair(int_1, int_2),
          int_2.hashKey() -> HashPair(int_2,int_3)))),
      TestHash("{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
        HashObj(pairs = mutable.Map(
          int_2.hashKey() -> HashPair(int_2,int_4),
          int_6.hashKey() -> HashPair(int_6,int_16))))
    )

    runVMTestsHash(tests)
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
      val stackElem = getLastPopped(tt.input)
      testExpectedObjectIntArr(tt.input, tt.expected, stackElem)
    }
  }

  def runVMTestsHash(tests: List[TestHash]): Unit = {
    for (tt <- tests) {
      println("Testing: " + tt.input)
      val stackElem = getLastPopped(tt.input)
      testExpectedObjectHash(tt.input, tt.expected, stackElem)
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
        assert("object not array: " +
          s"$actual: ${actual.objType()}" == s"input=$input")
    }
  }

  def testExpectedObjectHash(input: String,
                             expected: HashObj,
                             actual: ObjectLiteral): Unit = {
    actual match {
      case ho: HashObj =>
        println(s"\thash obj: input: $input")
        assert(ho.pairs.keys.size == expected.pairs.keys.size)
        for(key <- expected.pairs.keys) {
          val act = ho.pairs(key)
          val expect = expected.pairs(key)
          println(s"\t\thash key: $key, act: $act, expect: $expect")
          assert(act.key == expect.key)
          assert(act.value == expect.value)
        }
      case _ =>
        assert("object not hash: " +
          s"$actual: ${actual.objType()}" == s"input=$input")
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
