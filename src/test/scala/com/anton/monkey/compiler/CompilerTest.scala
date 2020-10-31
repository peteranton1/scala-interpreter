package com.anton.monkey.compiler

import com.anton.monkey.ast.Program
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.objectliteral.{CompiledFunction, IntegerObj, ObjectLiteral, StringObj}
import com.anton.monkey.parser.Parser
import org.scalatest.FunSuite

class CompilerTest extends FunSuite {
  private val byte_0 = 0.toByte
  private val byte_1 = 1.toByte
  private val byte_2 = 2.toByte
  private val byte_3 = 3.toByte
  private val byte_4 = 4.toByte
  private val byte_5 = 5.toByte
  private val byte_6 = 6.toByte
  private val byte_9 = 9.toByte
  private val byte_10 = 10.toByte
  private val byte_11 = 11.toByte
  private val byte_12 = 12.toByte
  private val int_1 = IntegerObj(1)
  private val int_2 = IntegerObj(2)
  private val int_3 = IntegerObj(3)
  private val int_4 = IntegerObj(4)
  private val int_5 = IntegerObj(5)
  private val int_6 = IntegerObj(6)
  private val int_10 = IntegerObj(10)
  private val int_20 = IntegerObj(20)
  private val int_24 = IntegerObj(24)
  private val int_3333 = IntegerObj(3333)

  case class TestInput(input: String,
                       expectedConstants: List[ObjectLiteral],
                       expectedInstructions: Array[Byte])

  test("Integer Arithmetic") {
    val tests = List(
      TestInput("1 + 2", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpAdd
          , Code.OpPop))
      , TestInput("1 - 2", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpSub
          , Code.OpPop))
      , TestInput("1 * 2", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpMul
          , Code.OpPop))
      , TestInput("2 / 1", List(int_2, int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpDiv
          , Code.OpPop))
      , TestInput("- 1", List(int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpMinus
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Boolean Expressions") {
    val tests = List(
      TestInput("true", List(),
        Array(Code.OpTrue
          , Code.OpPop))
      , TestInput("false", List(),
        Array(Code.OpFalse
          , Code.OpPop))
      , TestInput("1 > 2", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpGreaterThan
          , Code.OpPop))
      , TestInput("1 < 2", List(int_2, int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpGreaterThan
          , Code.OpPop))
      , TestInput("1 == 2", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpEqual
          , Code.OpPop))
      , TestInput("1 != 2", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpNotEqual
          , Code.OpPop))
      , TestInput("true == false", List(),
        Array(Code.OpTrue
          , Code.OpFalse
          , Code.OpEqual
          , Code.OpPop))
      , TestInput("true != false", List(),
        Array(Code.OpTrue
          , Code.OpFalse
          , Code.OpNotEqual
          , Code.OpPop))
      , TestInput("!true", List(),
        Array(Code.OpTrue
          , Code.OpBang
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Conditionals") {
    val tests = List(
      TestInput("if (true) { 10 }; 3333;", List(int_10, int_3333),
        Array(Code.OpTrue
          , Code.OpJumpNotTruthy, byte_0, byte_9
          , Code.OpConstant, byte_0, byte_0
          , Code.OpJump, byte_0, byte_10
          , Code.OpNull
          , Code.OpPop
          , Code.OpConstant, byte_0, byte_1
          , Code.OpPop))
      , TestInput("if (true) { 10 } else { 20 }; 3333;",
        List(int_10, int_20, int_3333),
        Array(Code.OpTrue
          , Code.OpJumpNotTruthy, byte_0, byte_9
          , Code.OpConstant, byte_0, byte_0
          , Code.OpJump, byte_0, byte_12
          , Code.OpConstant, byte_0, byte_1
          , Code.OpPop
          , Code.OpConstant, byte_0, byte_2
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Global Let Statements") {
    val tests = List(
      TestInput("let one = 1; let two = 2;", List(int_1, int_2),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpSetGlobal, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpSetGlobal, byte_0, byte_1))
      , TestInput("let one = 1; one;", List(int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpSetGlobal, byte_0, byte_0
          , Code.OpGetGlobal, byte_0, byte_0
          , Code.OpPop))
      , TestInput("let one = 1; let two = one; two;", List(int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpSetGlobal, byte_0, byte_0
          , Code.OpGetGlobal, byte_0, byte_0
          , Code.OpSetGlobal, byte_0, byte_1
          , Code.OpGetGlobal, byte_0, byte_1
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("String Expressions") {
    val tests = List(
      TestInput("\"monkey\";", List(StringObj("monkey")),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpPop))
      , TestInput("\"mon\" + \"key\";",
        List(StringObj("mon"), StringObj("key")),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpAdd
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Array Literals") {
    val tests = List(
      TestInput("[];", List(),
        Array(Code.OpArray, byte_0, byte_0
          , Code.OpPop))
      , TestInput("[1, 2, 3];", List(int_1, int_2, int_3),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpConstant, byte_0, byte_2
          , Code.OpArray, byte_0, byte_3
          , Code.OpPop))
      , TestInput("[1 + 2, 3 - 4, 5 * 6];",
        List(int_1, int_2, int_3, int_4, int_5, int_6),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpAdd
          , Code.OpConstant, byte_0, byte_2
          , Code.OpConstant, byte_0, byte_3
          , Code.OpSub
          , Code.OpConstant, byte_0, byte_4
          , Code.OpConstant, byte_0, byte_5
          , Code.OpMul
          , Code.OpArray, byte_0, byte_3
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Hash Literals") {
    val tests = List(
      TestInput("{};", List(),
        Array(Code.OpHash, byte_0, byte_0
          , Code.OpPop))
      , TestInput("{1: 2, 3: 4, 5: 6};",
        List(int_1, int_2, int_3, int_4, int_5, int_6),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpConstant, byte_0, byte_2
          , Code.OpConstant, byte_0, byte_3
          , Code.OpConstant, byte_0, byte_4
          , Code.OpConstant, byte_0, byte_5
          , Code.OpHash, byte_0, byte_6
          , Code.OpPop))
      , TestInput("{1: 2 + 3, 4: 5 * 6};",
        List(int_1, int_2, int_3, int_4, int_5, int_6),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpConstant, byte_0, byte_2
          , Code.OpAdd
          , Code.OpConstant, byte_0, byte_3
          , Code.OpConstant, byte_0, byte_4
          , Code.OpConstant, byte_0, byte_5
          , Code.OpMul
          , Code.OpHash, byte_0, byte_4
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Index Expressions") {
    val tests = List(
      TestInput("[1, 2, 3][1+1];",
        List(int_1, int_2, int_3, int_1, int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpConstant, byte_0, byte_2
          , Code.OpArray, byte_0, byte_3
          , Code.OpConstant, byte_0, byte_3
          , Code.OpConstant, byte_0, byte_4
          , Code.OpAdd
          , Code.OpIndex
          , Code.OpPop))
      , TestInput("{1: 2}[2 - 1];",
        List(int_1, int_2, int_2, int_1),
        Array(Code.OpConstant, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpHash, byte_0, byte_2
          , Code.OpConstant, byte_0, byte_2
          , Code.OpConstant, byte_0, byte_3
          , Code.OpSub
          , Code.OpIndex
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Functions") {
    val tests = List(
      TestInput("fn() { return 5 + 10 }",
        List(int_5, int_10, CompiledFunction(Instructions(
          Array(Code.OpConstant, byte_0, byte_0
            , Code.OpConstant, byte_0, byte_1
            , Code.OpAdd
            , Code.OpReturnValue
          )), 0, 0)),
        Array(Code.OpClosure, byte_0, byte_2, byte_0
          , Code.OpPop))
      , TestInput("fn() { 5 + 10 }",
        List(int_5, int_10, CompiledFunction(Instructions(
          Array(Code.OpConstant, byte_0, byte_0
            , Code.OpConstant, byte_0, byte_1
            , Code.OpAdd
            , Code.OpReturnValue
          )), 0, 0)),
        Array(Code.OpClosure, byte_0, byte_2, byte_0
          , Code.OpPop))
      ,
      TestInput("fn() { 1; 2 }",
        List(int_1
          , int_2
          , CompiledFunction(Instructions(
            Array(Code.OpConstant, byte_0, byte_0
              , Code.OpPop
              , Code.OpConstant, byte_0, byte_1
              , Code.OpReturnValue
            )), 0, 0)),
        Array(Code.OpClosure, byte_0, byte_2, byte_0
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Functions Without Return Value") {
    val tests = List(
      TestInput("fn() { }",
        List(CompiledFunction(Instructions(
          Array(Code.OpReturn)), 0, 0)),
        Array(Code.OpClosure, byte_0, byte_0, byte_0
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Function Calls") {
    val tests = List(
      TestInput("fn() { 24 }();",
        List(int_24
          , CompiledFunction(Instructions(
            Array(Code.OpConstant, byte_0, byte_0
              , Code.OpReturnValue
            )), 0, 0)),
        Array(Code.OpClosure, byte_0, byte_1, byte_0
          , Code.OpCall, byte_0
          , Code.OpPop))
      , TestInput("let noArg = fn() { 24 }; noArg(); ",
        List(int_24
          , CompiledFunction(Instructions(
            Array(Code.OpConstant, byte_0, byte_0
              , Code.OpReturnValue
            )), 0, 0)),
        Array(Code.OpClosure, byte_0, byte_1, byte_0
          , Code.OpSetGlobal, byte_0, byte_0
          , Code.OpGetGlobal, byte_0, byte_0
          , Code.OpCall, byte_0
          , Code.OpPop))
       , TestInput("let oneArg = fn(a) { a }; oneArg(24); ",
        List(CompiledFunction(Instructions(
            Array(Code.OpGetLocal, byte_0, byte_0
              , Code.OpReturnValue
            )), 0, 0)
          ,int_24
          ),
        Array(Code.OpClosure, byte_0, byte_0, byte_0
          , Code.OpSetGlobal, byte_0, byte_0
          , Code.OpGetGlobal, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpCall, byte_1
          , Code.OpPop))
       , TestInput("let manyArg = fn(a, b, c) { a; b; c }; " +
        "manyArg(24, 25, 26);",
        List(CompiledFunction(Instructions(
            Array(Code.OpGetLocal, byte_0, byte_0
              , Code.OpPop
              , Code.OpGetLocal, byte_0, byte_1
              , Code.OpPop
              , Code.OpGetLocal, byte_0, byte_2
              , Code.OpReturnValue
            )), 0, 0)
          ,int_24
          ),
        Array(Code.OpClosure, byte_0, byte_0, byte_0
          , Code.OpSetGlobal, byte_0, byte_0
          , Code.OpGetGlobal, byte_0, byte_0
          , Code.OpConstant, byte_0, byte_1
          , Code.OpConstant, byte_0, byte_2
          , Code.OpConstant, byte_0, byte_3
          , Code.OpCall, byte_3
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  def runCompilerTests(tests: List[TestInput]) {
    for (tt <- tests) {
      val program = parse(tt.input)
      val compiler = Compiler.newCompiler()
      val errStr = compiler.compile(program)
      assert(errStr == null)
      val bytecode = compiler.bytecode()
      testInstructions(
        tt.input
        , tt.expectedInstructions
        , bytecode.instructions)
      testConstants(
        tt.input
        , tt.expectedConstants
        , bytecode.constants)
    }
  }

  def testInstructions(input: String,
                       expected: Array[Byte],
                       actual: Instructions) {
    var i = 0
    val actualArray = actual.instructionArray
    println(
      s"Instructions ($input) expect = ${expected.toList}, \n" +
        s"             ($input) actual = ${actualArray.toList} ")
    while (i < expected.length) {
      val byteExpect = expected(i)
      val byteActual = actualArray(i)
      assert(byteExpect == byteActual)
      i += 1
    }
  }

  def testConstants(input: String,
                    expected: List[ObjectLiteral],
                    actual: List[ObjectLiteral]) {
    var i = 0
    println(
      s"Constants ($input) expect = \n${expected}, \n" +
        s"          ($input) actual = \n${actual} ")
    while (i < expected.length) {
      val valueExpect = expected(i)
      val valueActual = actual(i)
      assert(valueExpect.inspect() === valueActual.inspect())
      i += 1
    }
  }

  def parse(input: String): Program = {
    val l = Lexer.New(input)
    val p = Parser.New(l)
    p.parseProgram()
  }
}
