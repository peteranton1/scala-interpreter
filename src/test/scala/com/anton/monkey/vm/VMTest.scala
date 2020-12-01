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

  case class TestObj(input: String, expected: ObjectLiteral)

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

  test("Index Expressions") {
    val tests = List(
      TestInt("[1, 2, 3][1]", 2),
      TestInt("[1, 2, 3][0 + 2]", 3),
      TestInt("[[1, 1, 1]][0][0]", 1),
      TestInt("[][0]", NULL_VALUE),
      TestInt("[1, 2, 3][99]", NULL_VALUE),
      TestInt("[1][-1]", NULL_VALUE),
      TestInt("{1: 1, 2: 2}[1]", 1),
      TestInt("{1: 1, 2: 2}[2]", 2),
      TestInt("{1: 1}[0]", NULL_VALUE),
      TestInt("{}[0]", NULL_VALUE)
    )

    runVMTestsInt(tests)
  }

  test("Calling Functions without arguments") {
    val tests = List(
      TestInt("let fivePlusTen = fn() { 5 + 10; }; " +
        "fivePlusTen(); ", 15),
      TestInt(" let one = fn() { 1; }; " +
        "let two = fn() { 2; }; " +
        "one() + two();", 3),
      TestInt("let a = fn() { 1; }; " +
        "let b = fn() { a() + 1; }; " +
        "let c = fn() { b() + 1; }; " +
        "c(); ", 3)
    )

    runVMTestsInt(tests)
  }

  test("Functions with return statement") {
    val tests = List(
      TestInt("let earlyExit = fn() { return 99; 100; };" +
        "earlyExit();", 99),
      TestInt("let earlyExit = fn() { return 99; return 100; };" +
        "earlyExit();", 99)
    )

    runVMTestsInt(tests)
  }

  test("Functions without return value") {
    val tests = List(
      TestInt("let noReturn = fn() { };" +
        "noReturn();", NULL_VALUE),
      TestInt("let noReturn = fn() { };" +
        "let noReturnTwo = fn() { noReturn(); };" +
        "noReturn();" +
        "noReturnTwo();", NULL_VALUE)
    )

    runVMTestsInt(tests)
  }

  test("First Class Functions") {
    val tests = List(
      TestInt("let returnsOne = fn() { 1; };" +
        "let returnsOneReturner = fn() { returnsOne; };" +
        "returnsOneReturner()();", 1),
      TestInt("let returnsOneReturner = fn() { " +
        "  let returnsOne = fn() { 1; };" +
        "  returnsOne; " +
        "};" +
        "returnsOneReturner()();", 1)
    )

    runVMTestsInt(tests)
  }

  test("Calling Functions With Bindings") {
    val tests = List(
      TestInt("let one = fn() { let one = 1; one }; " +
        "one();", 1)
      , TestInt("let oneAndTwo = fn() { " +
        "    let one = 1; " +
        "    let two = 2; " +
        "    one + two; " +
        "};" +
        "oneAndTwo();", 3)
      , TestInt("let oneAndTwo = fn() { " +
        "  let one = 1;" +
        "  let two = 2;" +
        "  one + two;" +
        "};" +
        "let threeAndFour = fn() { " +
        "  let three = 3;" +
        "  let four = 4;" +
        "  three + four;" +
        "};" +
        "oneAndTwo() + threeAndFour();", 10)
      , TestInt("let firstFoobar = fn() { " +
        "  let foobar = 50;" +
        "  foobar;" +
        "};" +
        "let secondFoobar = fn() {" +
        "  let foobar = 100;" +
        "  foobar;" +
        "};" +
        "firstFoobar() + secondFoobar();", 150)
      , TestInt("let globalSeed = 50;" +
        "let minusOne = fn() {" +
        "  let num = 1;" +
        "  globalSeed - num;" +
        "};" +
        "let minusTwo = fn() {" +
        "  let num = 2;" +
        "  globalSeed - num;" +
        "};" +
        "minusOne() + minusTwo();", 97)
    )

    runVMTestsInt(tests)
  }

  test("Calling Functions with arguments and bindings") {
    val tests = List(
      TestInt("let identity = fn(a) { a; };" +
        "identity(4);", 4)
      , TestInt("let sum = fn(a, b) { a + b; };" +
        "sum(1, 2)", 3)
      , TestInt("let sum = fn(a, b) {" +
        "  let c = a + b; " +
        "  c;" +
        "};" +
        "sum(1, 2)", 3)
      , TestInt("let sum = fn(a, b) { " +
        "  let c = a + b;" +
        "  c;" +
        "};" +
        "sum(1, 2) + sum(3, 4);", 10)
      , TestInt("let sum = fn(a, b) {" +
        "  let c = a + b;" +
        "  c;" +
        "};" +
        "let outer = fn() {" +
        "  sum(1, 2) + sum(3, 4);" +
        "};" +
        "outer();", 10)
      , TestInt("let globalNum = 10;" +
        "let sum = fn(a, b) {" +
        "  let c = a + b;" +
        "  c + globalNum;" +
        "};" +
        "let outer = fn() {" +
        "  sum(1, 2) + sum(3, 4) + globalNum;" +
        "};" +
        "outer() + globalNum;", 50)
    )

    runVMTestsInt(tests)
  }

  test("Calling Functions With Wrong Arguments") {
    val tests = List(
      TestStr("fn() { 1; }(1);",
      "wrong number of arguments: want=0, got=1"),
      TestStr("fn(a, b) { a + b; }(1);",
        "wrong number of arguments: want=2, got=1")
    )

    for (tt <- tests) {
      //println("Testing: " + tt.input)
      val vm: VM = setupVM(tt.input)
      val err2: ObjectLiteral = runVM(vm)
      assert(tt.expected == err2.inspect())
    }
  }

  test("Builtin Functions") {
    val tests = List(
      TestObj("len(\"\")", IntegerObj(0))
      ,TestObj("len(\"four\")", IntegerObj(4))
      ,TestObj("len(\"hello world\")", IntegerObj(11))
      ,TestObj("len(1)",
        ErrorObj("argument to 'len' not supported, got=1"))
      ,TestObj("len(\"one\",\"two\")",
        ErrorObj("wrong number of arguments, got=2, want=1"))
      ,TestObj("len([1, 2, 3])", IntegerObj(3))
      ,TestObj("len([])", IntegerObj(0))
      ,TestObj("puts(\"hello\",\" world\")", Null)
    )

    for (tt <- tests) {
      println("Testing: " + tt.input)
      val vm: VM = setupVM(tt.input)
      val err2: ObjectLiteral = runVM(vm)
      assert(tt.expected.toString == vm.lastPopped.inspect())
    }
  }

  test("Closures") {
    val tests = List(
      TestObj("let newClosure = fn(a) {" +
        "  fn() { a; };" +
        "};" +
        "let closure = newClosure(99);" +
        "closure();", IntegerObj(99))
      ,TestObj("let newAdder = fn(a, b) {" +
        "  fn(c) { a + b + c };" +
        "};" +
        "let adder = newAdder(1, 2);" +
        "adder(8);", IntegerObj(11))
      ,TestObj("let newAdder = fn(a, b) {" +
        "  let c = a + b;" +
        "  fn(d) { c + d };" +
        "};" +
        "let adder = newAdder(1, 2);" +
        "adder(8);", IntegerObj(11))
      ,TestObj("let newAdderOuter = fn(a, b) {" +
        "  let c = a + b;" +
        "  fn(d) { " +
        "    let e = d + c;" +
        "    fn(f) { e + f; };" +
        "  };" +
        "};" +
        "let newAdderInner = newAdderOuter(1, 2);" +
        "let adder = newAdderInner(3);" +
        "adder(8);",
        IntegerObj(14))
      ,TestObj("let a = 1;" +
        "let newAdderOuter = fn(b) {" +
        "  fn(c) { " +
        "    fn(d) { a + b + c + d; };" +
        "  };" +
        "};" +
        "let newAdderInner = newAdderOuter(2);" +
        "let adder = newAdderInner(3);" +
        "adder(8);",
        IntegerObj(14))
      ,TestObj("let newClosure = fn(a, b) {" +
        "  let one = fn() { a; };" +
        "  let two = fn() { b; };" +
        "  fn() { " +
        "    one() + two();" +
        "  };" +
        "};" +
        "let closure = newClosure(9, 90);" +
        "closure();", IntegerObj(99))
    )

    for (tt <- tests) {
      println("Testing: " + tt.input)
      val vm: VM = setupVM(tt.input)
      val err2: ObjectLiteral = runVM(vm)
      assert(tt.expected.toString == vm.lastPopped.inspect())
    }
  }

  test("Recursive Functions") {
    val tests = List(
      TestObj("let countDown = fn(x) {" +
        "  if (x == 0) {" +
        "    return 0;" +
        "  } else {" +
        "    countDown(x - 1);" +
        "  }" +
        "};" +
        "countDown(1);", IntegerObj(0))
      ,TestObj("let countDown = fn(x) {" +
        "  if (x == 0) {" +
        "    return 0;" +
        "  } else {" +
        "    countDown(x - 1);" +
        "  }" +
        "};" +
        "let wrapper = fn() {" +
        "  countDown(1);" +
        "};" +
        "wrapper();", IntegerObj(0))
      ,TestObj("let wrapper = fn() {" +
        "  let countDown = fn(x) {" +
        "    if (x == 0) {" +
        "      return 0;" +
        "    } else {" +
        "      countDown(x - 1);" +
        "    }" +
        "  };" +
        "  countDown(1);" +
        "};" +
        "wrapper();", IntegerObj(0))
    )

    for (tt <- tests) {
      println("Testing: " + tt.input)
      val vm: VM = setupVM(tt.input)
      val err2: ObjectLiteral = runVM(vm)
      assert(tt.expected.toString == vm.lastPopped.inspect())
    }
  }

  test("Recursive Fibonacci") {
    val tests = List(
      TestObj("let fibonacci = fn(x) {" +
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
        "fibonacci(15);", IntegerObj(610))
    )

    for (tt <- tests) {
      println("Testing: " + tt.input)
      val vm: VM = setupVM(tt.input)
      val err2: ObjectLiteral = runVM(vm)
      assert(tt.expected.toString == vm.lastPopped.inspect())
    }
  }

  def parse(input: String): Program = {
    val l = Lexer.New(input)
    val p = Parser.New(l)
    p.parseProgram()
  }

  def getLastPopped(input: String): ObjectLiteral = {
    val vm: VM = setupVM(input)
    val err2: ObjectLiteral = runVM(vm)
    assert(err2 == null)
    vm.lastPoppedStackElem()
  }

  private def setupVM(input: String): VM = {
    val program = parse(input)
    val compiler = Compiler.newCompiler()
    val err1 = compiler.compile(program)
    assert(err1 == null)
    VM.newVM(compiler.bytecode())
  }

  private def runVM(vm: VM): ObjectLiteral = {
    vm.run()
  }

  def runVMTestsStr(tests: List[TestStr]): Unit = {
    for (tt <- tests) {
      //println("Testing: " + tt.input)
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
      //println("Testing: " + tt.input)
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
          s"${actual.objType()}: ${actual.inspect()}" == s"input=$input")
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
        assert(ho.pairs.keys.size == expected.pairs.keys.size)
        for(key <- expected.pairs.keys) {
          val act = ho.pairs(key)
          val expect = expected.pairs(key)
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
