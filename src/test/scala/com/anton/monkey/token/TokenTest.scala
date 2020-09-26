package com.anton.monkey.token

class TokenTest extends org.scalatest.FunSuite {

  test("should return IDENT"){
    assert(Token.LookupIdent("someIdent") == Token.IDENT)
  }

  test("should return FUNCTION"){
    assert(Token.LookupIdent("fn") == Token.FUNCTION)
  }

  test("should return LET"){
    assert(Token.LookupIdent("let") == Token.LET)
  }

  test("should return IF"){
    assert(Token.LookupIdent("if") == Token.IF)
  }

  test("should return ELSE"){
    assert(Token.LookupIdent("else") == Token.ELSE)
  }

  test("should return RETURN"){
    assert(Token.LookupIdent("return") == Token.RETURN)
  }

  test("should return TRUE"){
    assert(Token.LookupIdent("true") == Token.TRUE)
  }

  test("should return FALSE"){
    assert(Token.LookupIdent("false") == Token.FALSE)
  }
}
