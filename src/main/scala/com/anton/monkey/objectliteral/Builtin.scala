package com.anton.monkey.objectliteral

import com.anton.monkey.objectliteral.ObjectType.ARRAY_OBJ

case class Builtin(name: String, builtin: BuiltinObj)

object Builtin {
  def lenFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args.head
    arg match {
      case value: ArrayObj =>
        IntegerObj(value.elements.length)
      case value: StringObj =>
        IntegerObj(value.value.length)
      case _ => ErrorObj("argument to 'len' not supported, " +
        "got=%s".format(arg))
    }
  }

  def putsFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    for (arg <- args) {
      println(arg)
    }
    NullObj()
  }

  def firstFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args.head
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'first' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    if(arr.elements.nonEmpty){
      return arr.elements.head
    }
    ErrorObj("argument to 'first' must not be empty, " +
      "got=%s".format(arr.elements.length))
  }

  def lastFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args.head
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'last' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    if(arr.elements.nonEmpty){
      return arr.elements.last
    }
    ErrorObj("argument to 'last' must not be empty, " +
      "got=%s".format(arr.elements.length))
  }

  def restFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args.head
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'rest' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    if(arr.elements.nonEmpty){
      return ArrayObj(arr.elements.tail)
    }
    ErrorObj("argument to 'rest' must not be empty, " +
    "got=%s".format(arr.elements.length))
  }

  def pushFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 2) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=2".format(args.length))
    }
    val arg = args.head
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'push' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    ArrayObj(arr.elements.appended(args(1)))
  }

  val builtins = List(
    Builtin(name = "len", builtin = BuiltinObj(fn = lenFunc)),
    Builtin(name = "puts", builtin = BuiltinObj(fn = putsFunc)),
    Builtin(name = "first", builtin = BuiltinObj(fn = firstFunc)),
    Builtin(name = "last", builtin = BuiltinObj(fn = lastFunc)),
    Builtin(name = "rest", builtin = BuiltinObj(fn = restFunc)),
    Builtin(name = "push", builtin = BuiltinObj(fn = pushFunc))
  )

  def getBuiltinByName(name: String): BuiltinObj = {
    for (builtin <- builtins) {
      if (builtin.name == name) {
        return builtin.builtin
      }
    }
    null
  }
}
