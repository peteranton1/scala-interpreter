package com.anton.monkey.parser

import com.anton.monkey.lexer.Lexer
import com.anton.monkey.token.{Token, TokenType}

class Parser(l: Lexer) {

  val LOWEST = 0
  val EQUALS = 1
  val LESSGREATER = 2
  val SUM = 3
  val PRODUCT = 4
  val PREFIX = 5
  val CALL = 6
  val INDEX = 7

  val precedences: Map[TokenType, Int] = Map(
    Token.EQ -> EQUALS,
    Token.NOT_EQ -> EQUALS,
    Token.LT -> LESSGREATER,
    Token.GT -> LESSGREATER,
    Token.PLUS -> SUM,
    Token.MINUS -> SUM,
    Token.SLASH -> PRODUCT,
    Token.ASTERISK -> PRODUCT,
    Token.LPAREN -> CALL,
    Token.LBRACKET -> INDEX
  )


}
