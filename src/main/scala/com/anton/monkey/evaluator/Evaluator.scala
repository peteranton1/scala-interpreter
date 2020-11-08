package com.anton.monkey.evaluator

import com.anton.monkey.ast._
import com.anton.monkey.objectliteral.BooleanObj.{FALSE, TRUE}
import com.anton.monkey.objectliteral.Builtin.builtins
import com.anton.monkey.objectliteral.NullObj.NULL
import com.anton.monkey.objectliteral.ObjectType._
import com.anton.monkey.objectliteral._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Evaluator {

  def eval(node: Node, env: Environment): ObjectLiteral = {
    if(node == null) return NULL
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
        return evalIntegerLiteral(il)

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
        return evalStringLiteral(sl)

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
    var result: ObjectLiteral = NULL
    for (stmt <- p.statements) {
      result = eval(stmt, env)
      if (result.isInstanceOf[ReturnValueObj]) {
        return result
      }
      if (result.isInstanceOf[ErrorObj]) {
        return result
      }
    }
    result
  }

  def evalBlockStatement(bs: BlockStatement, env: Environment): ObjectLiteral = {
    var result: ObjectLiteral = NULL
    for (stmt <- bs.statements) {
      result = eval(stmt, env)
      if (result.isInstanceOf[ReturnValueObj]) {
        return result
      }
      if (result.isInstanceOf[ErrorObj]) {
        return result
      }
    }
    result
  }

  def evalExpressionStatement(es: ExpressionStatement, env: Environment): ObjectLiteral = {
    eval(es.expression, env)
  }

  def evalReturnStatement(rs: ReturnStatement, env: Environment): ObjectLiteral = {
    val result = eval(rs.returnValue, env)
    if(isError(result)) {
      return result
    }
    ReturnValueObj(result)
  }

  def evalLetStatement(ls: LetStatement, env: Environment): ObjectLiteral = {
    val result = eval(ls.value, env)
    if(result.isInstanceOf[ErrorObj]) {
        return result
    }
    env.set(ls.name.value, result)
    result
  }

  def evalIntegerLiteral(io: IntegerLiteral): ObjectLiteral = {
    IntegerObj(io.value)
  }

  def evalBooleanValue(value: Boolean): ObjectLiteral = {
    if (value) {
      return BooleanObj.TRUE
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
    if (isError(right)) {
      return right
    }
    pe.operator match {
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
    if (isError(left)) {
      return left
    }
    val right = eval(ie.right, env)
    if (isError(right)) {
      return right
    }
    if (left.objType() == INTEGER_OBJ && right.objType() == INTEGER_OBJ) {
      return evalIntegerInfixExpression(ie.operator,
        left.asInstanceOf[IntegerObj],
        right.asInstanceOf[IntegerObj])
    } else if (ie.operator == "==") {
      return evalBooleanValue(left == right)
    } else if (ie.operator == "!=") {
      return evalBooleanValue(left != right)
    } else if (left.objType() == STRING_OBJ && right.objType() == STRING_OBJ) {
      return evalStringInfixExpression(ie.operator,
        left.asInstanceOf[StringObj],
        right.asInstanceOf[StringObj])
    }
    ErrorObj("type mismatch: %s %s %s"
      .format(left.objType(), ie.operator, right.objType()))
  }

  def isTruthy(obj: ObjectLiteral): Boolean = {
    if(obj == null) return false;
    obj match {
      case NULL => false
      case TRUE => true
      case FALSE => false
      case _ => true
    }
  }

  def isError(obj: ObjectLiteral): Boolean = {
    if (obj != null) {
      return obj.objType() == ERROR_OBJ
    }
    false
  }

  def evalIntegerInfixExpression(operator: String,
                                 left: IntegerObj,
                                 right: IntegerObj): ObjectLiteral = {
    val leftVal = left.value
    val rightVal = right.value
    operator match {
      case "+" => return IntegerObj(leftVal + rightVal)
      case "-" => return IntegerObj(leftVal - rightVal)
      case "*" => return IntegerObj(leftVal * rightVal)
      case "/" => return IntegerObj(leftVal / rightVal)
      case "<" => return evalBooleanValue(leftVal < rightVal)
      case ">" => return evalBooleanValue(leftVal > rightVal)
      case "==" => return evalBooleanValue(leftVal == rightVal)
      case "!=" => return evalBooleanValue(leftVal != rightVal)
      case _ => null
    }
    ErrorObj("unknown operator: %s %s %s"
      .format(left.objType(), operator, right.objType()))
  }

  def evalStringInfixExpression(operator: String,
                                left: StringObj,
                                right: StringObj): ObjectLiteral = {
    val leftVal = left.value
    val rightVal = right.value
    operator match {
      case "+" => return StringObj(leftVal + rightVal)
      case _ => null
    }
    ErrorObj("unknown operator: %s %s %s"
      .format(left.objType(), operator, right.objType()))
  }

  def evalIfExpression(ife: IfExpression, env: Environment): ObjectLiteral = {
    val condition = eval(ife.condition, env)
    if (isError(condition)) {
      return condition
    }
    if (isTruthy(condition)) {
      eval(ife.consequence, env)
    } else if (ife.alternative != null) {
      eval(ife.alternative, env)
    } else {
      NULL
    }
  }

  def findBuiltin(builtins: List[Builtin],
                  idValue: String): BuiltinObj = {
    val builtin = builtins
      .find(_.name equals idValue) match {
      case Some(b) => b.builtin
      case None => null
    }
    builtin
  }

  def evalIdentifier(id: Identifier, env: Environment): ObjectLiteral = {
    val (value, ok1) = env.get(id.value)
    if (ok1) return value

    val builtin = findBuiltin(builtins, id.value)
    if (builtin != null) {
      return builtin
    }
    ErrorObj("identifier not found: %s".format(id.value))
  }

  def evalFunctionLiteral(fl: FunctionLiteral, env: Environment): ObjectLiteral = {
    FunctionObj(fl.parameters, fl.body, env)
  }

  def evalExpressions(arguments: List[Expression],
                      env: Environment): List[ObjectLiteral] = {
    val result = new ListBuffer[ObjectLiteral]()
    for (arg <- arguments) {
      val evaluated = eval(arg, env)
      if (isError(evaluated)) return List(evaluated)
      result.addOne(evaluated)
    }
    result.toList
  }

  def extendFunctionEnv(fn: FunctionObj,
                        args: List[ObjectLiteral]): Environment = {
    val env = Environment.NewEnclosedEnvironment(fn.env)
    var i = 0
    while (i < fn.parameters.length) {
      val param = fn.parameters(i)
      env.set(param.value, args(i))
      i += 1
    }
    env
  }

  def unwrapReturnValue(obj: ObjectLiteral): ObjectLiteral = {
    val result = obj.asInstanceOf[ReturnValueObj]
    if (result != null) {
      return result.value
    }
    obj
  }

  def applyFunction(func: ObjectLiteral,
                    args: List[ObjectLiteral]): ObjectLiteral = {
    func match {
      case fn: FunctionObj =>
        val extendedEnv = extendFunctionEnv(fn, args)
        val evaluated = eval(fn.body, extendedEnv)
        return unwrapReturnValue(evaluated)
      case bi: BuiltinObj =>
        val result = bi.fn(args)
        if (result != null) {
          return result
        }
        return NULL
    }
    ErrorObj("Not a function: %s".format(func.objType()))
  }

  def evalCallExpression(ce: CallExpression, env: Environment): ObjectLiteral = {
    val func = eval(ce.function, env)
    if (isError(func)) return func
    val args = evalExpressions(ce.arguments, env)
    if (args.length == 1 && isError(args.head)) return args.head
    applyFunction(func, args)
  }

  def evalStringLiteral(sl: StringLiteral): ObjectLiteral = {
    StringObj(sl.value)
  }

  def evalArrayLiteral(al: ArrayLiteral, env: Environment): ObjectLiteral = {
    val elements = evalExpressions(al.elements, env)
    if (elements.length == 1 && isError(elements.head)) {
      return elements.head
    }
    ArrayObj(elements)
  }

  def evalArrayIndexExpression(array: ObjectLiteral,
                               index: ObjectLiteral): ObjectLiteral = {
    val arrayObject = array.asInstanceOf[ArrayObj]
    val idx = index.asInstanceOf[IntegerObj].value
    val max = arrayObject.elements.length - 1
    if (idx < 0 || idx > max) {
      return NULL
    }
    arrayObject.elements(idx)
  }

  def evalHashIndexExpression(hash: ObjectLiteral,
                              index: ObjectLiteral): ObjectLiteral = {
    val hashObject = hash.asInstanceOf[HashObj]
    if(!index.isInstanceOf[Hashable]) {
      return ErrorObj("unusable as hash key: %s"
        .format(index.objType()))
    }
    val key = index.asInstanceOf[Hashable]
    if (key == null) {
      ErrorObj("Unusable as a hash key: %s"
        .format(index.objType()))
    }
    val pair = hashObject.pairs(key.hashKey())
    if (pair == null) {
      return NULL
    }
    pair.value
  }

  def evalIndexExpression(ie: IndexExpression, env: Environment): ObjectLiteral = {

    val left = eval(ie.left, env)
    if (isError(left)) return left

    val index = eval(ie.index, env)
    if (isError(index)) return index

    if (left.objType() == ARRAY_OBJ && index.objType() == INTEGER_OBJ) {
      return evalArrayIndexExpression(left, index)

    } else if (left.objType() == HASH_OBJ) {
      return evalHashIndexExpression(left, index)
    }

    ErrorObj("Index operator not supported: %s"
      .format(left.objType()))
  }

  def evalHashLiteral(hl: HashLiteral, env: Environment): ObjectLiteral = {
    val pairs = mutable.Map[HashKey, HashPair]()
    for (pair <- hl.pairs) {
      val keyNode = pair._1
      val valueNode = pair._2
      val key = eval(keyNode, env)
      if (isError(key)) {
        return key
      }
      val hashKey = key.asInstanceOf[Hashable]
      if (hashKey == null) {
        ErrorObj("Unusable as hashkey: %s"
          .format(key.objType()))
      }
      val value = eval(valueNode, env)
      if (isError(value)) {
        return value
      }
      val hashed = hashKey.hashKey()
      pairs(hashed) = HashPair(key, value)
    }
    HashObj(pairs)
  }
}
