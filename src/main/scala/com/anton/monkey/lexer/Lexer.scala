package com.anton.monkey.lexer

import com.anton.monkey.token.{Token, TokenType}

class Lexer(input: String) {
  var position = 0
  var readPosition = 0
  var ch: Char = '\u0000'

  def readChar(): Unit = {
    if (readPosition >= input.length()) {
      ch = '\u0000'
    } else {
      ch = input.charAt(readPosition)
    }
    position = readPosition
    readPosition += 1
  }

  def peekChar(): Char = {
    if (readPosition >= input.length()) {
      return '\u0000'
    }
    input.charAt(readPosition)
  }

  def NextToken(): Token = {
    skipWhitespace()
    val tok = ch match {
      case '+' =>
        newToken(Token.PLUS, ch)
      case '-' =>
        newToken(Token.MINUS, ch)
      case '/' =>
        newToken(Token.SLASH, ch)
      case '*' =>
        newToken(Token.ASTERISK, ch)

      case '=' =>
        newTwoByteToken()
      case '!' =>
        newTwoByteToken()

      case ',' =>
        newToken(Token.COMMA, ch)
      case ':' =>
        newToken(Token.COLON, ch)
      case ';' =>
        newToken(Token.SEMICOLON, ch)
      case '(' =>
        newToken(Token.LPAREN, ch)
      case ')' =>
        newToken(Token.RPAREN, ch)
      case '{' =>
        newToken(Token.LBRACE, ch)
      case '}' =>
        newToken(Token.RBRACE, ch)
      case '[' =>
        newToken(Token.LBRACKET, ch)
      case ']' =>
        newToken(Token.RBRACKET, ch)
      case '<' =>
        newToken(Token.LT, ch)
      case '>' =>
        newToken(Token.GT, ch)
      case '"' =>
        Token(Token.STRING, readString())
      case 0 =>
        Token(Token.EOF, "")
      case _ =>
        if (isLetter(ch)) {
          val literal = readIdentifier()
          Token(Token.LookupIdent(literal), literal)
        } else if (isDigit(ch)) {
          val literal = readNumber()
          Token(Token.INT, literal)
        } else {
          newToken(Token.ILLEGAL, ch)
        }
    }
    readChar()
    tok
  }

  def skipWhitespace(): Unit = {
    while (ch == ' ' ||
      ch == '\t' ||
      ch == '\n' ||
      ch == '\r') {
      readChar()
    }
  }

  def readNumber(): String = {
    val pos = position
    while (isDigit(peekChar())) {
      readChar()
    }
    input.substring(pos, position+1)
  }

  def isDigit(ch: Char): Boolean = {
    '0' <= ch && ch <= '9'
  }

  def readIdentifier(): String = {
    val posStart = position
    while (isLetter(peekChar())) {
      readChar()
    }
    input.substring(posStart, position+1)
  }

  def readString(): String = {
    val posStart = position + 1
    var finished = false
    while (!finished) {
      readChar()
      if (ch == '"' || ch == 0) {
        finished = true
      } else if (ch == '\\' && peekChar() == '"') {
        readChar()
      }
    }
    input.substring(posStart, position)
  }

  def isLetter(ch: Char): Boolean = {
    'a' <= ch && ch <= 'z' ||
      'A' <= ch && ch <= 'Z' ||
      ch == '_'
  }

  def newTwoByteToken(): Token = {
    var tokenType = Token.ILLEGAL
    val ch0 = ch
    if (peekChar() == '=') {
      if (ch == '!') {
        tokenType = Token.NOT_EQ
      } else if (ch == '=') {
        tokenType = Token.EQ
      }
      readChar()
      val literal: String = String.valueOf(ch0) + String.valueOf(ch)
      return Token(tokenType, literal)
    }
    if (ch == '!') {
      tokenType = Token.BANG
    } else if (ch == '=') {
      tokenType = Token.ASSIGN
    }
    newToken(tokenType, ch)
  }

  def newToken(tokenType: TokenType, ch: Char): Token = {
    Token(tokenType, String.valueOf(ch))
  }

}

object Lexer {

  def New(input: String): Lexer = {
    val lexer = new Lexer(input)
    lexer.readChar()
    lexer
  }
}
