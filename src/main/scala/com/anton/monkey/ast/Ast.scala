package com.anton.monkey.ast

import com.anton.monkey.token.Token

import scala.collection.mutable.ListBuffer

trait Node {
  def TokenLiteral(): String

  def String(): String
}

trait Statement extends Node {
  def statementNode(): Unit
}

trait Expression extends Node {
  def expressionNode(): Unit
}

case class Program(statements: List[Statement]) {
  def TokenLiteral(): String = {
    if (statements.nonEmpty) {
      return statements.head.TokenLiteral()
    }
    ""
  }

  def String(): String = {
    val buf = new StringBuilder()
    statements.foreach { s =>
      buf.append(s.String())
    }
    buf.toString()
  }
}

case class LetStatement(token: Token, name: Identifier, value: Expression) extends Statement {
  override def statementNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    token.literal + " " + name.String() + " = " + value.String() + ";"
  }
}

case class ReturnStatement(token: Token, returnValue: Expression) extends Statement {
  override def statementNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    token.literal + " " + returnValue.String() + ";"
  }
}

case class ExpressionStatement(token: Token, expression: Expression) extends Statement {
  override def statementNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    if (expression != null) {
      return expression.String()
    }
    ""
  }
}

case class Identifier(token: Token, value: String) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = value
}

case class IntegerLiteral(token: Token, value: Int) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = token.literal
}

case class StringLiteral(token: Token, value: String) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = token.literal
}

case class PrefixExpression(token: Token,
                            operator: String, right: Expression) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    "(" + operator + right.String() + ")"
  }
}

case class InfixExpression(token: Token, left: Expression,
                           operator: String, right: Expression) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    "(" + left.String() + " " + operator + " " + right.String() + ")"
  }
}

case class BooleanValue(token: Token, value: Boolean) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = token.literal
}

case class IfExpression(token: Token, condition: Expression,
                        consequence: BlockStatement,
                        alternative: BlockStatement) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    val frag = if (alternative != null) " else " + alternative.String() else ""
    "if (" + condition.String() + ") " + consequence.String() + frag
  }
}

case class BlockStatement(token: Token, statements: List[Statement]) extends Statement {
  override def statementNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = "{ " + statements.map(_.String()).mkString(" ") + " }"
}

case class FunctionLiteral(token: Token, parameters: List[Identifier],
                           body: BlockStatement, name: String) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    val fnname = if(name != null) "<" + name + ">" else ""
    TokenLiteral() + fnname + "(" +
      parameters.map(_.String()).mkString(", ") +
      ")" + body.String()
  }
}

case class CallExpression(token: Token, function: Expression, arguments: List[Expression]) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    val str = arguments.map(_.String()).mkString(", ")
    function.String() + "(" + str + ")"
  }
}

case class ArrayLiteral(token: Token, elements: List[Expression]) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    "[" +
      elements.map(_.String()).mkString(", ") +
      "]"
  }
}

case class IndexExpression(token: Token, left: Expression, index: Expression) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    "(" + left.String() +
      "[" +
      index.String() +
      "])"
  }
}

case class HashLiteral(token: Token, pairs: Map[Expression, Expression]) extends Expression {
  override def expressionNode(): Unit = {}

  override def TokenLiteral(): String = token.literal

  override def String(): String = {
    val list = new ListBuffer[String]
    pairs.foreachEntry((k, v) => list += k.String() + ": " + v.String())
    "{" + list.toList.mkString(", ") + "}"
  }
}
