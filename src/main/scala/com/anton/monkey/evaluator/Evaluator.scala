package com.anton.monkey.evaluator

import com.anton.monkey.ast.{ArrayLiteral, BlockStatement, BooleanValue, CallExpression, ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, StringLiteral}
import com.anton.monkey.objectliteral.ObjectType.{INTEGER_OBJ, STRING_OBJ}
import com.anton.monkey.objectliteral.{BooleanObj, Environment, ErrorObj, IntegerObj, NullObj, ObjectLiteral, ReturnValueObj}

class Evaluator {

  def eval(node: Node, env: Environment): ObjectLiteral = {
    node match {
      case p: Program =>
        return evalProgram(p, env)

      case bs: BlockStatement =>
        return evalBlockStatement(bs, env)

      case es: ExpressionStatement =>
        return evalExpressionStatement(es, env)

      case rs: ReturnStatement =>
        return evalReturnStatement(rs, env)

      case ls: LetStatement =>
        return evalLetStatement(ls, env)

      case il: IntegerLiteral =>
        return evalIntegerLiteral(il, env)

      case bv: BooleanValue =>
        return evalBooleanValue(bv.value)

      case pe: PrefixExpression =>
        return evalPrefixExpression(pe, env)

      case ie: InfixExpression =>
        return evalInfixExpression(ie, env)

      case ife: IfExpression =>
        return evalIfExpression(ife, env)

      case id: Identifier =>
        return evalIdentifier(id, env)

      case fl: FunctionLiteral =>
        return evalFunctionLiteral(fl, env)

      case ce: CallExpression =>
        return evalCallExpression(ce, env)

      case sl: StringLiteral =>
        return evalStringLiteral(sl, env)

      case al: ArrayLiteral =>
        return evalArrayLiteral(al, env)

      case ie: IndexExpression =>
        return evalIndexExpression(ie, env)

      case hl: HashLiteral =>
        return evalHashLiteral(hl, env)

    }
    null
  }

  def evalProgram(p: Program, env: Environment): ObjectLiteral = {
    p.statements.map(stmt => {
      val result = eval(stmt, env)
      result match {
        case ret: ReturnValueObj =>
          return ret
        case err: ErrorObj =>
          return err
      }
      result
    }).last
  }

  def evalBlockStatement(bs: BlockStatement, env: Environment): ObjectLiteral = {
    bs.statements.map(stmt => {
      val result = eval(stmt, env)
      result match {
        case ret: ReturnValueObj =>
          return ret
        case err: ErrorObj =>
          return err
      }
      result
    }).last
  }

  def evalExpressionStatement(es: ExpressionStatement, env: Environment): ObjectLiteral = {
    eval(es, env)
  }

  def evalReturnStatement(rs: ReturnStatement, env: Environment): ObjectLiteral = {
    val result = eval(rs.returnValue, env)
    result match {
      case ret: ReturnValueObj =>
        return ret
      case err: ErrorObj =>
        return err
    }
    result
  }

  def evalLetStatement(ls: LetStatement, env: Environment): ObjectLiteral = {
    val result = eval(ls.value, env)
    result match {
      case err: ErrorObj =>
        return err
    }
    result
  }

  def evalIntegerLiteral(io: IntegerLiteral, env: Environment): ObjectLiteral = {
    IntegerObj(io.value)
  }

  def evalBooleanValue(value: Boolean): ObjectLiteral = {
    if (value) {
      BooleanObj.TRUE
    }
    BooleanObj.FALSE
  }

  def evalBangOperatorExpression(right: ObjectLiteral): ObjectLiteral =
    right match {
      case BooleanObj.TRUE => BooleanObj.FALSE
      case BooleanObj.FALSE => BooleanObj.TRUE
      case NullObj() => BooleanObj.TRUE
      case _ => BooleanObj.FALSE
    }

  def evalMinusPrefixOperatorExpression(right: ObjectLiteral): ObjectLiteral =
    right match {
      case io: IntegerObj =>
        IntegerObj(-io.value)
      case _ =>
        ErrorObj("unknown operator: -%s"
          .format(right.objType()))
    }

  def evalPrefixExpression(pe: PrefixExpression, env: Environment): ObjectLiteral = {
    val right = eval(pe.right, env)
    right match {
      case err: ErrorObj =>
        return err
    }
    pe.operator {
      case "!" =>
        return evalBangOperatorExpression(right)
      case "-" =>
        return evalMinusPrefixOperatorExpression(right)
    }
    ErrorObj("unknown operator: %s%s"
      .format(pe.operator, right.objType()))
  }

  def evalInfixExpression(ie: InfixExpression, env: Environment): ObjectLiteral = {
    val left = eval(ie.left, env)
    left match {
      case err: ErrorObj => return err
    }
    val right = eval(ie.right, env)
    right match {
      case err: ErrorObj => return err
    }
    if (left.objType() == INTEGER_OBJ && right.objType() == INTEGER_OBJ) {
      return evalIntegerInfixExpression(ie.operator, left, right)
    } else if (ie.operator == "==") {
      return evalBooleanValue(left == right)
    } else if (ie.operator == "!=") {
      return evalBooleanValue(left != right)
    } else if (left.objType() == STRING_OBJ && right.objType() == STRING_OBJ) {
      return evalStringInfixExpression(ie.operator, left, right)
    }
    ErrorObj("unknown operator: %s %s %s"
      .format(left.objType(), ie.operator, right.objType()))
  }

  def evalIntegerInfixExpression(operator: String,
                                 left: ObjectLiteral,
                                 right: ObjectLiteral): ObjectLiteral = ???

  def evalStringInfixExpression(operator: String,
                                left: ObjectLiteral,
                                right: ObjectLiteral): ObjectLiteral = ???

  def evalIfExpression(ife: IfExpression, env: Environment): ObjectLiteral = {
    ???
  }

  def evalIdentifier(id: Identifier, env: Environment): ObjectLiteral = {
    ???
  }

  def evalFunctionLiteral(fl: FunctionLiteral, env: Environment): ObjectLiteral = {
    ???
  }

  def evalCallExpression(ce: CallExpression, env: Environment): ObjectLiteral = {
    ???
  }

  def evalStringLiteral(sl: StringLiteral, env: Environment): ObjectLiteral = {
    ???
  }

  def evalArrayLiteral(al: ArrayLiteral, env: Environment): ObjectLiteral = {
    ???
  }

  def evalIndexExpression(ie: IndexExpression, env: Environment): ObjectLiteral = {
    ???
  }

  def evalHashLiteral(hl: HashLiteral, env: Environment): ObjectLiteral = {
    ???
  }
}
