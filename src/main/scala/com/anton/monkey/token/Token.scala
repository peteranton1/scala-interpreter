package com.anton.monkey.token

case class Token(tokenType: TokenType, literal: String)

object Token {
  // ILLEGAL Control chars
  val ILLEGAL: TokenType = TokenType("ILLEGAL")
  // EOF Control chars
  val EOF: TokenType = TokenType("EOF")

  // IDENT Identifiers
  val IDENT: TokenType = TokenType("IDENT")
  // INT datatype
  val INT: TokenType = TokenType("INT")
  // STRING datatype
  val STRING: TokenType = TokenType("STRING")

  // ASSIGN Operators
  val ASSIGN: TokenType = TokenType("=")
  // PLUS Operator
  val PLUS: TokenType = TokenType("+")
  // MINUS Operator
  val MINUS: TokenType = TokenType("-")
  // SLASH Operator
  val SLASH: TokenType = TokenType("/")
  // ASTERISK Operator
  val ASTERISK: TokenType = TokenType("*")

  // BANG Delimiters
  val BANG: TokenType = TokenType("!")
  // COMMA Delimiters
  val COMMA: TokenType = TokenType(",")
  // COLON Delimiters
  val COLON: TokenType = TokenType(":")
  // SEMICOLON Delimiter
  val SEMICOLON: TokenType = TokenType(";")
  // LPAREN Delimiter
  val LPAREN: TokenType = TokenType("(")
  // RPAREN Delimiter
  val RPAREN: TokenType = TokenType(")")
  // LBRACE Delimiter
  val LBRACE: TokenType = TokenType("{")
  // RBRACE Delimiter
  val RBRACE: TokenType = TokenType("}")
  // LBRACKET Delimiter
  val LBRACKET: TokenType = TokenType("[")
  // RBRACKET Delimiter
  val RBRACKET: TokenType = TokenType("]")
  // LT Delimiter
  val LT: TokenType = TokenType("<")
  // GT Delimiter
  val GT: TokenType = TokenType(">")
  // EQ Delimiter
  val EQ: TokenType = TokenType("==")
  // NOT_EQ Delimiter
  val NOT_EQ: TokenType = TokenType("!=")

  // FUNCTION Keyword
  val FUNCTION: TokenType = TokenType("FUNCTION")
  // LET Keyword
  val LET: TokenType = TokenType("LET")
  // IF Keyword
  val IF: TokenType = TokenType("IF")
  // ELSE Keyword
  val ELSE: TokenType = TokenType("ELSE")
  // RETURN Keyword
  val RETURN: TokenType = TokenType("RETURN")
  // TRUE Keyword
  val TRUE: TokenType = TokenType("TRUE")
  // FALSE Keyword
  val FALSE: TokenType = TokenType("FALSE")

  val keywords: Map[String, TokenType] =
    Map(
      "fn" -> FUNCTION,
      "let" -> LET,
      "if" -> IF,
      "else" -> ELSE,
      "return" -> RETURN,
      "true" -> TRUE,
      "false" -> FALSE
    )

  // LookupIdent returns Ident or Keyword
  def LookupIdent(ident: String):TokenType = {
    val tok = keywords.getOrElse(ident, IDENT)
    tok
  }

}