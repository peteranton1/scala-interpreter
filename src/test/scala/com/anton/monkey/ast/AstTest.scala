package com.anton.monkey.ast

import com.anton.monkey.token.Token
import org.scalatest.FunSuite

class AstTest extends FunSuite {
  test("should conform to LET statement") {
    val program = Program(List(LetStatement(
      Token(Token.LET,Token.LET.tokenType),
      Identifier(Token(Token.IDENT, "myVar"), "myVar"),
      Identifier(Token(Token.IDENT, "anotherVar"), "anotherVar")
    )))
    val expectedStmt = "let myVar = anotherVar;"
    assert( program.String() != expectedStmt )
  }
}
