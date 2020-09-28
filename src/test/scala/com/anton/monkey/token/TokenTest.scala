package com.anton.monkey.token

case class Expecteds(input:String, tokenType: TokenType)

class TokenTest extends org.scalatest.FunSuite {

  test("should return Ident or Keyword"){
    val tests: List[Expecteds] = List(
      Expecteds("someIdent", Token.IDENT),
      Expecteds("fn", Token.FUNCTION),
      Expecteds("let", Token.LET),
      Expecteds("if", Token.IF),
      Expecteds("else", Token.ELSE),
      Expecteds("return", Token.RETURN),
      Expecteds("true", Token.TRUE),
      Expecteds("false", Token.FALSE)
    )
    for (expected <- tests) {
      val actual = Token.LookupIdent(expected.input)
      //println(expected)
      assert(actual == expected.tokenType)
    }
  }
}
