package com.anton.monkey.parser

import com.anton.monkey.ast.{BooleanValue, Expression, Identifier, IntegerLiteral, LetStatement, Program, ReturnStatement, Statement}
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.token.Token
import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  case class TestValue(expr:Expression)
  case class TestInput(input:String,name:String,value:TestValue)

  test("should LetStatement") {
    val tests = List(
      TestInput("let x = 5;", "x",
        TestValue(IntegerLiteral(Token(Token.INT,"5"),5))),
      TestInput("let y = true;", "y",
        TestValue(BooleanValue(Token(Token.TRUE,"true"),value = true))),
      TestInput("let foobar = y;", "foobar",
        TestValue(Identifier(Token(Token.IDENT,"y"),"y"))),
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testLetStatement(tt, stmt)
        val actual = stmt.asInstanceOf[LetStatement].value
        testLiteralExpression(tt,actual)
      }
    )
  }

  test("should ReturnStatement") {
    val tests = List(
      TestInput("return 5;", null,
        TestValue(IntegerLiteral(Token(Token.INT,"5"),5))),
      TestInput("return 10;", null,
        TestValue(IntegerLiteral(Token(Token.INT,"10"),10))),
      TestInput("return 993322;", null,
        TestValue(IntegerLiteral(Token(Token.INT,"993322"),993322))),
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testReturnStatement(tt, stmt)
        val actual = stmt.asInstanceOf[ReturnStatement].returnValue
        testLiteralExpression(tt,actual)
      }
    )
  }

  private def testLetStatement(tt: TestInput, stmt: Statement):Unit = {
    assert(stmt.TokenLiteral() == "let")
    assert(stmt.isInstanceOf[LetStatement])
    assert(stmt.asInstanceOf[LetStatement].name.value == tt.name)
  }

  private def testReturnStatement(tt: TestInput, stmt: Statement):Unit = {
    assert(stmt.TokenLiteral() == "return")
    assert(stmt.isInstanceOf[ReturnStatement])
  }

  private def testLiteralExpression(tt: TestInput, expr: Expression):Unit = {
    assert(expr == tt.value.expr)
  }
}
