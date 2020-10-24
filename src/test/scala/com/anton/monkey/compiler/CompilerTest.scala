package com.anton.monkey.compiler

import com.anton.monkey.ast.{Node, Program}
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.parser.Parser
import org.scalatest.FunSuite

class CompilerTest extends FunSuite {
  private val value_0 = 0.toByte
  private val value_1 = 1.toByte
  private val value_2 = 2.toByte

  case class TestInput(input: String,
                       expectedConstants: Array[Int],
                       expectedInstructions: Array[Byte])

  test("Test Integer Arithmetic") {
    val tests = List(
      TestInput("1 + 2", Array(1, 2),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpAdd
          , Code.OpPop))
      , TestInput("1 - 2", Array(1, 2),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpSub
          , Code.OpPop))
      , TestInput("1 * 2", Array(1, 2),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpMul
          , Code.OpPop))
      , TestInput("2 / 1", Array(2, 1),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpDiv
          , Code.OpPop))
      , TestInput("- 1", Array(1),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpMinus
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  test("Test Boolean Expressions") {
    val tests = List(
       TestInput("true", Array(),
        Array(Code.OpTrue
          , Code.OpPop))
      , TestInput("false", Array(),
        Array(Code.OpFalse
          , Code.OpPop))
      , TestInput("1 > 2", Array(1, 2),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpGreaterThan
          , Code.OpPop))
      , TestInput("1 < 2", Array(2, 1),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpGreaterThan
          , Code.OpPop))
      , TestInput("1 == 2", Array(1, 2),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpEqual
          , Code.OpPop))
      , TestInput("1 != 2", Array(1, 2),
        Array(Code.OpConstant, value_0, value_0
          , Code.OpConstant, value_0, value_1
          , Code.OpNotEqual
          , Code.OpPop))
      , TestInput("true == false", Array(),
        Array(Code.OpTrue
          , Code.OpFalse
          , Code.OpEqual
          , Code.OpPop))
      , TestInput("true != false", Array(),
        Array(Code.OpTrue
          , Code.OpFalse
          , Code.OpNotEqual
          , Code.OpPop))
      , TestInput("!true", Array(),
        Array(Code.OpTrue
          , Code.OpBang
          , Code.OpPop))
    )
    runCompilerTests(tests)
  }

  def runCompilerTests(tests: List[TestInput]) {
    for (tt <- tests) {
      val program = parse(tt.input)
      val compiler = Compiler.newCompiler()
      val errStr = compiler.compile(program.statements.head)
      assert(errStr == null)
      val bytecode = compiler.bytecode()
      testInstructions(
        tt.input
        , tt.expectedInstructions
        , bytecode.instructions)
    }
  }

  def testInstructions(input: String,
                       expected: Array[Byte],
                       actual: Instructions) {
    var i = 0
    val actualArray = actual.instructionArray
    println(s"($input) expect = ${expected.toList}, \n" +
      s"($input) actual = ${actualArray.toList} ")
    while (i < expected.length) {
      val bexpect = expected(i)
      val bactual = actualArray(i)
      assert(bexpect == bactual)
      i += 1
    }
  }

  def parse(input: String): Program = {
    val l = Lexer.New(input)
    val p = Parser.New(l)
    p.parseProgram()
  }

  /*test("should make bytes") {
    val tests = List(
      TestInput(Code.OpConstant, Array(65534),
        Array(
          Code.putUint8(Code.OpConstant)
          , Code.putUint8(255)
          , Code.putUint8(254)
        ))
      , TestInput(Code.OpAdd, Array(),
        Array(
          Code.putUint8(Code.OpAdd)
        ))
      , TestInput(Code.OpGetLocal, Array(255),
        Array(
          Code.putUint8(Code.OpGetLocal)
          , Code.putUint8(255)
        ))
      , TestInput(Code.OpClosure, Array(65534, 255),
        Array(
          Code.putUint8(Code.OpClosure)
          , Code.putUint8(255)
          , Code.putUint8(254)
          , Code.putUint8(255)
        ))
    )
    tests.foreach(input => {
      val actual = Code.make(input.op, input.operands)
      //assert(actual sameElements input.expected)
      assertArray(actual, input.expected)
    })
  }*/

}
