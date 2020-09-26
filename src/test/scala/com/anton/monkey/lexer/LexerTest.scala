package com.anton.monkey.lexer

import com.anton.monkey.token.Token
import org.scalatest.FunSuite

class LexerTest extends FunSuite {

  test("should NextToken") {
    val input =
      """
        |   let five = 5;
        |   let ten = 10;
        |		let add = fn(x, y) {
        |			x + y;
        |		};
        |		let result = add(five,ten);
        |		!-/*5;
        |		5 < 10 > 5;
        |		if (5 < 10) {
        |			return true;
        |		} else {
        |			return false;
        |		}
        |		10 == 10;
        |		10 != 9;
        |		"foobar"
        |		"foo bar"
        |		"foo \"bar"
        |		"foo \tbar"
        |		[1, 2];
        |		{"foo": "bar"}
        |  """.stripMargin

    val tests: List[Token] = List(
      Token(Token.LET, "let"),
      Token(Token.IDENT, "five"),
      Token(Token.ASSIGN, "="),
      Token(Token.INT, "5"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.LET, "let"),
      Token(Token.IDENT, "ten"),
      Token(Token.ASSIGN, "="),
      Token(Token.INT, "10"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.LET, "let"),
      Token(Token.IDENT, "add"),
      Token(Token.ASSIGN, "="),
      Token(Token.FUNCTION, "fn"),
      Token(Token.LPAREN, "("),
      Token(Token.IDENT, "x"),
      Token(Token.COMMA, ","),
      Token(Token.IDENT, "y"),
      Token(Token.RPAREN, ")"),
      Token(Token.LBRACE, "{"),
      Token(Token.IDENT, "x"),
      Token(Token.PLUS, "+"),
      Token(Token.IDENT, "y"),
      Token(Token.SEMICOLON, ";"),
      Token(Token.RBRACE, "}"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.LET, "let"),
      Token(Token.IDENT, "result"),
      Token(Token.ASSIGN, "="),
      Token(Token.IDENT, "add"),
      Token(Token.LPAREN, "("),
      Token(Token.IDENT, "five"),
      Token(Token.COMMA, ","),
      Token(Token.IDENT, "ten"),
      Token(Token.RPAREN, ")"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.BANG, "!"),
      Token(Token.MINUS, "-"),
      Token(Token.SLASH, "/"),
      Token(Token.ASTERISK, "*"),
      Token(Token.INT, "5"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.INT, "5"),
      Token(Token.LT, "<"),
      Token(Token.INT, "10"),
      Token(Token.GT, ">"),
      Token(Token.INT, "5"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.IF, "if"),
      Token(Token.LPAREN, "("),
      Token(Token.INT, "5"),
      Token(Token.LT, "<"),
      Token(Token.INT, "10"),
      Token(Token.RPAREN, ")"),
      Token(Token.LBRACE, "{"),
      Token(Token.RETURN, "return"),
      Token(Token.TRUE, "true"),
      Token(Token.SEMICOLON, ";"),
      Token(Token.RBRACE, "}"),
      Token(Token.ELSE, "else"),
      Token(Token.LBRACE, "{"),
      Token(Token.RETURN, "return"),
      Token(Token.FALSE, "false"),
      Token(Token.SEMICOLON, ";"),
      Token(Token.RBRACE, "}"),

      Token(Token.INT, "10"),
      Token(Token.EQ, "=="),
      Token(Token.INT, "10"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.INT, "10"),
      Token(Token.NOT_EQ, "!="),
      Token(Token.INT, "9"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.STRING, "foobar"),
      Token(Token.STRING, "foo bar"),
      Token(Token.STRING, "foo \\\"bar"),
      Token(Token.STRING, "foo \\tbar"),

      Token(Token.LBRACKET, "["),
      Token(Token.INT, "1"),
      Token(Token.COMMA, ","),
      Token(Token.INT, "2"),
      Token(Token.RBRACKET, "]"),
      Token(Token.SEMICOLON, ";"),

      Token(Token.LBRACE, "{"),
      Token(Token.STRING, "foo"),
      Token(Token.COLON, ":"),
      Token(Token.STRING, "bar"),
      Token(Token.RBRACE, "}"),

      Token(Token.EOF, ""),
    )

    val lexer = Lexer.New(input)
    for (expected <- tests) {
      val actual = lexer.NextToken()
      //println(actual)
      assert(actual == expected)
    }
  }
}
