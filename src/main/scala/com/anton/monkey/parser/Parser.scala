package com.anton.monkey.parser

import com.anton.monkey.ast._
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.token.{Token, TokenType}

import scala.collection.mutable.ListBuffer
import scala.util.Try

class Parser(l: Lexer) {

  // Constants
  val LOWEST = 0
  val EQUALS = 1
  val LESSGREATER = 2
  val SUM = 3
  val PRODUCT = 4
  val PREFIX = 5
  val CALL = 6
  val INDEX = 7

  // Pratt parse map
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

  // Member variables
  var errors: List[String] = List.empty

  private val emptyToken: Token = Token(Token.ILLEGAL, Token.ILLEGAL.tokenType)
  var curToken: Token = emptyToken
  var peekToken: Token = emptyToken

  var prefixParserFns: Map[TokenType, () => Expression] = Map(
    Token.IDENT -> this.parseIdentifier(),
    Token.INT -> this.parseIntegerLiteral(),
    Token.TRUE -> this.parseBoolean(),
    Token.FALSE -> this.parseBoolean(),
    Token.BANG -> this.parsePrefixExpression(),
    Token.MINUS -> this.parsePrefixExpression(),
    Token.LPAREN -> this.parseGroupedExpression(),
    Token.IF -> this.parseIfExpression(),
    Token.FUNCTION -> this.parseFunctionLiteral(),
    Token.STRING -> this.parseStringLiteral(),
    Token.LBRACKET -> this.parseArrayLiteral(),
    Token.LBRACE -> this.parseHashLiteral()
  )
  var infixParserFns: Map[TokenType, Expression => Expression] = Map(
    Token.PLUS -> this.parseInfixExpression(),
    Token.MINUS -> this.parseInfixExpression(),
    Token.SLASH -> this.parseInfixExpression(),
    Token.ASTERISK -> this.parseInfixExpression(),
    Token.EQ -> this.parseInfixExpression(),
    Token.NOT_EQ -> this.parseInfixExpression(),
    Token.LT -> this.parseInfixExpression(),
    Token.GT -> this.parseInfixExpression(),
    Token.LPAREN -> this.parseCallExpression(),
    Token.LBRACKET -> this.parseIndexExpression()
  )

  def parseProgram(): Program = {
    var statements = new ListBuffer[Statement]
    while (curToken.tokenType != Token.EOF) {
      statements += parseStatement()
    }
    Program(statements.toList)
  }

  def parseStatement(): Statement = {
    curToken.tokenType match {
      case Token.LET => parseLetStatement()
      case Token.RETURN => parseReturnStatement()
      case _ => parseExpressionStatement()
    }
  }

  def parseLetStatement(): Statement = {
    val token = curToken
    if (!expectPeek(Token.IDENT)) {
      return null
    }

    val ident = Identifier(curToken, curToken.literal)
    if (!expectPeek(Token.ASSIGN)) {
      return null
    }
    nextToken()
    val value = parseExpression(LOWEST)
    // TODO
    //    if(value.isInstanceOf(FunctionLiteral)){
    //      val fl = value.asInstanceOf(FunctionLiteral)
    //          fl.Name =???
    //    }
    if (peekTokenIs(Token.SEMICOLON)) {
      nextToken()
    }
    LetStatement(token, ident, value)
  }

  def parseReturnStatement(): Statement = {
    val token = curToken
    nextToken()
    val returnValue = parseExpression(LOWEST)
    if (peekTokenIs(Token.SEMICOLON)) {
      nextToken()
    }
    ReturnStatement(token, returnValue)
  }

  def parseExpressionStatement(): Statement = {
    val token = curToken
    val expr = parseExpression(LOWEST)
    if (peekTokenIs(Token.SEMICOLON)) {
      nextToken()
    }
    ExpressionStatement(token, expr)
  }

  def parseExpression(precedence: Int): Expression = {
    val prefix = prefixParserFns(peekToken.tokenType)
    if (prefix == null) {
      noPrefixParseFnError(curToken.tokenType)
      return null
    }
    var leftExpr = prefix()
    while (!peekTokenIs(Token.SEMICOLON) && precedence < peekPrecedence()) {
      val infix = infixParserFns(peekToken.tokenType)
      if (infix == null) {
        return leftExpr
      }
      nextToken()
      leftExpr = infix(leftExpr)
    }
    leftExpr
  }

  def tokenPrecedence(token: Token): Int = {
    precedences(token.tokenType) match {
      case x => x
      case _ => LOWEST
    }
  }

  def peekPrecedence(): Int = {
    tokenPrecedence(peekToken)
  }

  def curPrecedence(): Int = {
    tokenPrecedence(curToken)
  }

  def noPrefixParseFnError(tokenType: TokenType): Unit = {
    val msg = String.format("no prefix parse function for '%s' found",
      tokenType)
    errors += msg
  }

  def peekError(tokenType: TokenType): Unit = {
    val msg = String.format("expected next token to be '%s', got='%s'",
      tokenType, peekToken.tokenType)
    errors += msg
  }

  def expectPeek(tokenType: TokenType): Boolean = {
    if (peekTokenIs(tokenType)) {
      nextToken()
      return true
    }
    peekError(tokenType)
    false
  }

  def curTokenIs(tokenType: TokenType): Boolean =
    curToken.tokenType == tokenType

  def peekTokenIs(tokenType: TokenType): Boolean =
    peekToken.tokenType == tokenType

  def nextToken(): Unit = {
    curToken = peekToken
    peekToken = l.NextToken()
  }

  def parsePrefixExpression(): () => Expression = {
    () => {
      val token = curToken
      nextToken()
      val right = parseExpression(PREFIX)
      PrefixExpression(token, token.literal, right)
    }
  }

  def parseInfixExpression(): Expression => Expression = {
    left => {
      val token = curToken
      val precedence = curPrecedence()
      nextToken()
      val right = parseExpression(precedence)
      InfixExpression(token, left, token.literal, right)
    }
  }

  def parseGroupedExpression(): () => Expression = {
    () => {
      nextToken()
      val expr = parseExpression(LOWEST)
      if (!expectPeek(Token.RPAREN)) {
        return null
      }
      expr
    }
  }

  def parseIfExpression(): () => Expression = {
    () => {
      val token = curToken
      if (!expectPeek(Token.LPAREN)) {
        return null
      }
      nextToken()
      val condition = parseExpression(LOWEST)
      if (!expectPeek(Token.RPAREN)) {
        return null
      }
      if (!expectPeek(Token.LBRACE)) {
        return null
      }
      val consequence = parseBlockStatement()
      var alternative: BlockStatement = null
      if (peekTokenIs(Token.ELSE)) {
        nextToken()
        if (!expectPeek(Token.LBRACE)) {
          return null
        }
        alternative = parseBlockStatement()
      }
      IfExpression(token, condition, consequence, alternative)
    }
  }

  def parseFunctionLiteral(): () => Expression = {
    () => {
      val token = curToken
      val params = parseFunctionParameters()
      if (!expectPeek(Token.LBRACE)) {
        return null
      }
      val body = parseBlockStatement()
      FunctionLiteral(token, params, body, token.literal)
    }
  }

  def parseFunctionParameters(): List[Identifier] = {
    val identifiers = new ListBuffer[Identifier]
    if (peekTokenIs(Token.RPAREN)) {
      return identifiers.toList
    }
    nextToken()
    identifiers += Identifier(curToken, curToken.literal)
    while (peekTokenIs(Token.COMMA)) {
      nextToken() // consume ident
      nextToken() // consumer Comma
      identifiers += Identifier(curToken, curToken.literal)
    }
    if (!expectPeek(Token.RPAREN)) {
      return null
    }
    identifiers.toList
  }

  def parseBlockStatement(): BlockStatement = {
    val token = curToken
    var statements = new ListBuffer[Statement]
    nextToken()
    while (!curTokenIs(Token.RBRACE) && !curTokenIs(Token.EOF)) {
      val stmt = parseStatement()
      if (stmt != null) {
        statements += stmt
      }
      nextToken()
    }
    BlockStatement(token, statements.toList)
  }

  def parseCallExpression(): Expression => Expression = {
    function => {
      val token = curToken
      val arguments = parseExpressionList(Token.RPAREN)
      CallExpression(token, function, arguments)
    }
  }

  def parseIndexExpression(): Expression => Expression = {
    left => {
      val token = curToken
      nextToken()
      val index = parseExpression(LOWEST)
      if (!expectPeek(Token.RBRACKET)) {
        return null
      }
      IndexExpression(token, left, index)
    }
  }

  def parseExpressionList(end: TokenType): List[Expression] = {
    val list = new ListBuffer[Expression]
    if (peekTokenIs(end)) {
      nextToken()
      return list.toList
    }
    nextToken()
    list += parseExpression(LOWEST)
    while (peekTokenIs(Token.COMMA)) {
      nextToken()
      nextToken()
      list += parseExpression(LOWEST)
    }
    if (expectPeek(end)) {
      return null
    }
    list.toList
  }

  def parseIdentifier(): () => Expression = {
    () => {
      Identifier(curToken, curToken.literal)
    }
  }

  def parseBoolean(): () => Expression = {
    () => {
      BooleanValue(curToken, curTokenIs(Token.TRUE))
    }
  }

  def parseIntegerLiteral(): () => Expression = {
    () => {
      val token = curToken
      val value = Try {
        Integer.parseInt(token.literal)
      }
      if (value.isFailure) {
        val msg = String.format("could not parse '%s' as integer",
          token.literal)
        errors += msg
        return null
      }
      IntegerLiteral(token, value.get)
    }
  }

  def parseStringLiteral(): () => Expression = {
    () => {
      StringLiteral(curToken, curToken.literal)
    }
  }

  def parseArrayLiteral(): () => Expression = {
    () => {
      val token = curToken
      val elements = parseExpressionList(Token.RBRACKET)
      ArrayLiteral(token, elements)
    }
  }

  def parseHashLiteral(): () => Expression = {
    () => {
      val token = curToken
      var pairs:Map[Expression, Expression] = Map()
      while (!peekTokenIs(Token.RBRACE)) {
        nextToken()
        val key = parseExpression(LOWEST)
        if (!expectPeek(Token.COLON)) {
          return null
        }
        nextToken()
        val value = parseExpression(LOWEST)
        pairs += ((key, value))
        if (!peekTokenIs(Token.RBRACE) && !expectPeek(Token.COMMA)) {
          return null
        }
      }
      if (!expectPeek(Token.RBRACE)) {
        return null
      }
      HashLiteral(token, pairs)
    }
  }

}

