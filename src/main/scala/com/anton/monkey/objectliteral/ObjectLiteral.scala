package com.anton.monkey.objectliteral

import com.anton.monkey.ast.{BlockStatement, Expression, Identifier}
import com.anton.monkey.code.Instructions
import com.anton.monkey.objectliteral.ObjectType._

import scala.collection.mutable

case class ObjectType(name: String)

trait ObjectLiteral {
  def objType(): ObjectType

  def inspect(): String
}


case class IntegerObj(value: Int) extends ObjectLiteral with Hashable {
  override def objType(): ObjectType = INTEGER_OBJ

  override def inspect(): String = String.format("%d", value)

  override def hashKey(): HashKey = {
    HashKey(INTEGER_OBJ, value)
  }

  override def toString: String = inspect()
}

case class BooleanObj(value: Boolean) extends ObjectLiteral with Hashable {
  override def objType(): ObjectType = BOOLEAN_OBJ

  override def inspect(): String = String.format("%s", value)

  override def hashKey(): HashKey = {
    HashKey(BOOLEAN_OBJ,if (value) 1 else 0)
  }
}

object BooleanObj {
  val TRUE: BooleanObj = BooleanObj(true)
  val FALSE: BooleanObj = BooleanObj(false)
}

case class NullObj() extends ObjectLiteral {
  override def objType(): ObjectType = NULL_OBJ

  override def inspect(): String = "NullObj"

  override def toString: String = inspect()
}

object NullObj {
  val NULL: NullObj = NullObj()
}

case class ReturnValueObj(value: ObjectLiteral) extends ObjectLiteral {
  override def objType(): ObjectType = RETURN_VALUE_OBJ

  override def inspect(): String = {
    val buf = new StringBuilder()
    buf append "returnValue("
    buf append value.inspect()
    buf append ")"
    buf.toString()
  }

  override def toString: String = inspect()
}

case class ErrorObj(message: String) extends ObjectLiteral {
  override def objType(): ObjectType = ERROR_OBJ

  override def inspect(): String = message

  override def toString: String = inspect()
}

case class FunctionObj(parameters: List[Identifier],
                       body: BlockStatement,
                       env: Environment) extends ObjectLiteral {
  override def objType(): ObjectType = FUNCTION_OBJ

  override def inspect(): String = {
    val buf = new StringBuilder()
    buf append "fn("
    buf append stringFromExpressionList(parameters, ", ")
    buf append ") {\n"
    buf append body.String()
    buf append "\n}"
    buf.toString()
  }

  override def toString: String = inspect()
}

case class StringObj(value: String) extends ObjectLiteral with Hashable {
  override def objType(): ObjectType = STRING_OBJ

  override def inspect(): String = value

  override def hashKey(): HashKey = HashKey(STRING_OBJ, md5HashString(value))

  override def toString: String = inspect()
}

case class BuiltinObj(fn: List[ObjectLiteral] => ObjectLiteral) extends ObjectLiteral {
  override def objType(): ObjectType = BUILTIN_OBJ

  override def inspect(): String = "builtin function"

  override def toString: String = inspect()
}

case class ArrayObj(elements: List[ObjectLiteral]) extends ObjectLiteral {
  override def objType(): ObjectType = ARRAY_OBJ

  override def inspect(): String = {
    val buf = new StringBuilder()
    buf append "["
    buf append stringFromObjectList(elements, ", ")
    buf append "]"
    buf.toString()
  }

  override def toString: String = inspect()
}

trait Hashable {
  def hashKey(): HashKey
}

case class HashKey(objType: ObjectType, value: Int)

case class HashPair(key: ObjectLiteral, value: ObjectLiteral)

case class HashObj(pairs: mutable.Map[HashKey, HashPair]) extends ObjectLiteral {
  override def objType(): ObjectType = HASH_OBJ

  override def inspect(): String = {
    val buf = new StringBuilder()
    buf append "{"
    buf append stringFromObjectMap(pairs, ", ")
    buf append "}"
    buf.toString()
  }

  override def toString: String = inspect()
}

case class CompiledFunction(
                             instructions: Instructions,
                             numLocals: Int,
                             numParameters: Int
                           ) extends ObjectLiteral {
  override def objType(): ObjectType = COMPILED_FUNCTION_OBJ

  override def inspect(): String = s"CompiledFunction[$instructions]"
}

object ObjectType {
  def md5HashString(s: String): Int = {
    import java.security.MessageDigest
    import java.math.BigInteger
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16).hashCode
    hashedString
  }

  def stringFromObjectMap(pairs: mutable.Map[HashKey, HashPair], sep: String): String = {
    val buf = new StringBuilder()
    pairs.foreachEntry((k, v) => {
      if (buf.nonEmpty) {
        buf append sep
      }
      buf.append(String.format("%s: %s", v.key.inspect(), v.value.inspect()))
    })
    buf.toString
  }

  def stringFromObjectList(values: List[ObjectLiteral], sep: String): String = {
    val buf = new StringBuilder()
    values.foreach(v => {
      if (buf.nonEmpty) {
        buf append sep
      }
      buf append v.inspect()
    })
    buf.toString
  }

  def stringFromExpressionList(values: List[Expression], sep: String): String = {
    val buf = new StringBuilder()
    values.foreach(v => {
      if (buf.nonEmpty) {
        buf append sep
      }
      buf append v.String()
    })
    buf.toString
  }

  // INTEGER_OBJ const
  val INTEGER_OBJ: ObjectType = ObjectType("INTEGER")
  // BOOLEAN_OBJ const
  val BOOLEAN_OBJ: ObjectType = ObjectType("BOOLEAN")
  // NULL_OBJ const
  val NULL_OBJ: ObjectType = ObjectType("NULL")
  // RETURN_VALUE_OBJ const
  val RETURN_VALUE_OBJ: ObjectType = ObjectType("RETURN_VALUE")
  // ERROR_OBJ const
  val ERROR_OBJ: ObjectType = ObjectType("ERROR")
  // FUNCTION_OBJ const
  val FUNCTION_OBJ: ObjectType = ObjectType("FUNCTION")
  // STRING_OBJ const
  val STRING_OBJ: ObjectType = ObjectType("STRING")
  // BUILTIN_OBJ const
  val BUILTIN_OBJ: ObjectType = ObjectType("BUILTIN")
  // ARRAY_OBJ const
  val ARRAY_OBJ: ObjectType = ObjectType("ARRAY")
  // HASH_OBJ const
  val HASH_OBJ: ObjectType = ObjectType("HASH")
  // COMPILED_FUNCTION_OBJ const
  val COMPILED_FUNCTION_OBJ: ObjectType = ObjectType("COMPILED_FUNCTION_OBJ")
  // CLOSURE_OBJ const
  val CLOSURE_OBJ: ObjectType = ObjectType("CLOSURE")
}