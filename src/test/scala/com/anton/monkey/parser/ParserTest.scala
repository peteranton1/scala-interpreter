package com.anton.monkey.parser

import com.anton.monkey.ast._
import com.anton.monkey.lexer.Lexer
import com.anton.monkey.token.Token
import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  case class TestValue(expr: Expression)

  case class TestInput(input: String, name: String, value: TestValue)

  case class TestInputInfix(input: String, name: String, left: TestValue, right: TestValue)

  case class TestInputString(input: String, expected: String)

  private val integer_5: TestValue = TestValue(IntegerLiteral(Token(Token.INT, "5"), value = 5))
  private val boolean_true: TestValue = TestValue(BooleanValue(Token(Token.TRUE, "true"), value = true))
  private val boolean_false: TestValue = TestValue(BooleanValue(Token(Token.FALSE, "false"), value = false))

  test("should LetStatement") {
    val tests = List(
      TestInput("let x = 5;", "x",
        integer_5),
      TestInput("let y = true;", "y",
        boolean_true),
      TestInput("let foobar = y;", "foobar",
        TestValue(Identifier(Token(Token.IDENT, "y"), "y"))),
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
        assert(actual == tt.value.expr)
      }
    )
  }

  test("should ReturnStatement") {
    val tests = List(
      TestInput("return 5;", null,
        TestValue(IntegerLiteral(Token(Token.INT, "5"), 5))),
      TestInput("return 10;", null,
        TestValue(IntegerLiteral(Token(Token.INT, "10"), 10))),
      TestInput("return 993322;", null,
        TestValue(IntegerLiteral(Token(Token.INT, "993322"), 993322))),
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
        assert(actual == tt.value.expr)
      }
    )
  }

  test("should IdentifierExpression") {
    val tests = List(
      TestInput("foobar;", "foobar",
        TestValue(Identifier(Token(Token.IDENT, "foobar"), "foobar")))
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testExpressionStatement(tt, stmt)
        val actual = stmt.asInstanceOf[ExpressionStatement].expression
        assert(actual == tt.value.expr)
      }
    )
  }

  test("should IntegerLiteralExpression") {
    val tests = List(
      TestInput("5;", "5",
        TestValue(IntegerLiteral(Token(Token.INT, "5"), 5)))
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testExpressionStatement(tt, stmt)
        val actual = stmt.asInstanceOf[ExpressionStatement].expression
        assert(actual == tt.value.expr)
      }
    )
  }

  test("should StringLiteralExpression") {
    val tests = List(
      TestInput("\"hello world\";", "hello world",
        TestValue(StringLiteral(Token(Token.STRING, "hello world"), "hello world")))
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testExpressionStatement(tt, stmt)
        val actual = stmt.asInstanceOf[ExpressionStatement].expression
        assert(actual == tt.value.expr)
      }
    )
  }

  test("should BooleanExpression") {
    val tests = List(
      TestInput("true;", "true",
        TestValue(BooleanValue(Token(Token.TRUE, "true"), value = true)))
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testExpressionStatement(tt, stmt)
        val actual = stmt.asInstanceOf[ExpressionStatement].expression
        assert(actual == tt.value.expr)
      }
    )
  }

  test("should Parsing PrefixExpression") {
    val tests = List(
      TestInput("!5;", "!",
        integer_5)
      , TestInput("-15;", "-",
        TestValue(IntegerLiteral(Token(Token.INT, "15"), value = 15)))
      , TestInput("!true;", "!",
        TestValue(BooleanValue(Token(Token.TRUE, "true"), value = true)))
      , TestInput("!false;", "!",
        boolean_false)
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        testExpressionStatement(tt, stmt)
        val expr = stmt.asInstanceOf[ExpressionStatement]
        val prefixexpr = expr.expression.asInstanceOf[PrefixExpression]
        assert(prefixexpr.operator == tt.name)
        assert(prefixexpr.right == tt.value.expr)
      }
    )
  }

  test("should Parsing InfixExpression") {
    val tests = List(
      TestInputInfix("5 + 5;", "+", integer_5, integer_5)
      , TestInputInfix("5 - 5;", "-", integer_5, integer_5)
      , TestInputInfix("5 * 5;", "*", integer_5, integer_5)
      , TestInputInfix("5 / 5;", "/", integer_5, integer_5)
      , TestInputInfix("5 > 5;", ">", integer_5, integer_5)
      , TestInputInfix("5 < 5;", "<", integer_5, integer_5)
      , TestInputInfix("5 == 5;", "==", integer_5, integer_5)
      , TestInputInfix("5 != 5;", "!=", integer_5, integer_5)
      , TestInputInfix("true == true;", "==", boolean_true, boolean_true)
      , TestInputInfix("true != false;", "!=", boolean_true, boolean_false)
      , TestInputInfix("false == false;", "==", boolean_false, boolean_false)
    )
    tests.foreach(
      tt => {
        val l = Lexer.New(tt.input)
        val p = Parser.New(l)
        val program = p.parseProgram()
        assert(p.errors.isEmpty)
        assert(program.statements.length == 1)
        val stmt = program.statements.head
        assert(stmt.isInstanceOf[ExpressionStatement])
        val expr = stmt.asInstanceOf[ExpressionStatement]
        testInfixExpression(expr.expression, tt.left.expr, tt.name, tt.right.expr)
      }
    )
  }

  test("should OperatorPrecedence") {
    val tests = List(
      TestInputString("-a * b", "((-a) * b)")
      , TestInputString("!-a", "(!(-a))")
      , TestInputString("a + b - c", "((a + b) - c)")
      , TestInputString("a * b - c", "((a * b) - c)")
      , TestInputString("a * b * c", "((a * b) * c)")
      , TestInputString("a * b / c", "((a * b) / c)")
      , TestInputString("a + b / c", "(a + (b / c))")
      , TestInputString("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")
      , TestInputString("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
      , TestInputString("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
      , TestInputString("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
      , TestInputString("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
      , TestInputString("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
      , TestInputString("true", "true")
      , TestInputString("false", "false")
      , TestInputString("3 > 5 == false", "((3 > 5) == false)")
      , TestInputString("3 < 5 == true", "((3 < 5) == true)")
      , TestInputString("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")
      , TestInputString("(5 + 5) * 2", "((5 + 5) * 2)")
      , TestInputString("2 / (5 + 5)", "(2 / (5 + 5))")
      , TestInputString("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))")
      , TestInputString("-(5 + 5)", "(-(5 + 5))")
      , TestInputString("!(true == true)", "(!(true == true))")
      , TestInputString(
        "a + add(b * c) + d",
        "((a + add((b * c))) + d)")
      , TestInputString(
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")
      , TestInputString(
        "add(a + b + c * d / f + g)",
        "add((((a + b) + ((c * d) / f)) + g))")
      , TestInputString(
        "a * [1, 2, 3, 4][a * c] * d",
        "((a * ([1, 2, 3, 4][(a * c)])) * d)")
      , TestInputString(
        "add(a * b[2], b[1], 2 * [1, 2][1])",
        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should IfExpression") {
    val tests = List(
      TestInputString("if (x < y) { x }", "if ((x < y)) { x }")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should IfElseExpression") {
    val tests = List(
      TestInputString("if (x < y) { x } else { y }", "if ((x < y)) { x } else { y }")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should FunctionLiteral") {
    val tests = List(
      TestInputString("fn(x, y) { x + y; }", "fn(x, y){ (x + y) }")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should FunctionLiteral with name") {
    val tests = List(
      TestInputString("let myfunction = fn() { }", "let myfunction=fn<myfunction>(){  };")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should FunctionParameterParsing") {
    val tests = List(
      TestInputString("fn() {}", "fn(){  }")
      , TestInputString("fn(x) {}", "fn(x){  }")
      , TestInputString("fn(x,y) {}", "fn(x, y){  }")
      , TestInputString("fn(x,y,z) {}", "fn(x, y, z){  }")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should CallExpressionParsing") {
    val tests = List(
      TestInputString("add();", "add()")
      , TestInputString("add(1, 2);", "add(1, 2)")
      , TestInputString("add(1, 2 + 3, 4+5);", "add(1, (2 + 3), (4 + 5))")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should ParsingArrayLiterals") {
    val tests = List(
      TestInputString("[1]", "[1]")
      , TestInputString("[1, 2 * 2, 3+3]", "[1, (2 * 2), (3 + 3)]")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should ParsingHashLiterals") {
    val tests = List(
      TestInputString("{}", "{}")
      , TestInputString("{\"one\": 1, \"two\": 2, \"three\": 3}",
        "{one: 1, two: 2, three: 3}")
      , TestInputString("{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}",
        "{one: (0 + 1), two: (10 - 8), three: (15 / 5)}")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  test("should ParseStatements") {
    val tests = List(
      TestInputString("let x = 1; return 1; 5 + 5; let myfn = fn(x){x}; if(1){add(5)}",
        "let x=1;return 1;(5 + 5)let myfn=fn<myfn>(x){ x };if (1) { add(5) }")
    )
    tests.foreach(
      tt => testStringInputOutputFn(tt)
    )
  }

  private def testStringInputOutputFn(tt: TestInputString): Unit = {
    val l = Lexer.New(tt.input)
    val p = Parser.New(l)
    val program = p.parseProgram()
    assert(p.errors.isEmpty)
    val actual = program.String()
    assert(actual == tt.expected)
  }

  private def testExpressionStatement(tt: TestInput, stmt: Statement): Unit = {
    assert(stmt.TokenLiteral() == tt.name)
    assert(stmt.isInstanceOf[ExpressionStatement])
  }

  private def testExpressionStatement(tt: TestInputInfix, stmt: Statement): Unit = {
    assert(stmt.TokenLiteral() == tt.name)
    assert(stmt.isInstanceOf[ExpressionStatement])
  }

  private def testLetStatement(tt: TestInput, stmt: Statement): Unit = {
    assert(stmt.TokenLiteral() == "let")
    assert(stmt.isInstanceOf[LetStatement])
    assert(stmt.asInstanceOf[LetStatement].name.value == tt.name)
  }

  private def testReturnStatement(tt: TestInput, stmt: Statement): Unit = {
    assert(stmt.TokenLiteral() == "return")
    assert(stmt.isInstanceOf[ReturnStatement])
  }

  private def testLiteralExpression(tt: TestInput, expr: Expression): Unit = {


  }

  private def testInfixExpression(expr: Expression,
                                  t_left: Expression,
                                  t_operator: String,
                                  t_right: Expression): Boolean = {
    val infixExpr = expr.asInstanceOf[InfixExpression]
    assert(infixExpr.left == t_left)
    assert(infixExpr.operator == t_operator)
    assert(infixExpr.right == t_right)

    true
  }
}
