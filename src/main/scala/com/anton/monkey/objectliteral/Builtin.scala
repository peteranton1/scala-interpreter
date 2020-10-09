package com.anton.monkey.objectliteral

import com.anton.monkey.objectliteral.ObjectType.ARRAY_OBJ

case class Builtin(name: String, builtin: BuiltinObj)

object Builtin {
  private def letFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args[0]
    arg match {
      case value: ArrayObj =>
        IntegerObj(value.elements.length)
      case value: StringObj =>
        IntegerObj(value.value.length)
      case _ => ErrorObj("argument to 'len' not supported, " +
        "got=%s".format(arg))
    }
  }

  private def putsFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    for (arg <- args) {
      println(arg)
    }
    NullObj()
  }

  private def firstFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args[0].asInstanceOf[ObjectLiteral]
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'first' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    arr.elements.head
  }

  private def lastFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args[0].asInstanceOf[ObjectLiteral]
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'last' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    arr.elements.last
  }

  private def restFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 1) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=1".format(args.length))
    }
    val arg = args[0].asInstanceOf[ObjectLiteral]
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'rest' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    ArrayObj(arr.elements.tail)
  }

  private def pushFunc(args: List[ObjectLiteral]): ObjectLiteral = {
    if (args.length != 2) {
      return ErrorObj("wrong number of arguments, " +
        "got=%d, want=2".format(args.length))
    }
    val arg = args[0].asInstanceOf[ObjectLiteral]
    if (arg.objType() != ARRAY_OBJ) {
      return ErrorObj("argument to 'push' must be ARRAY, " +
        "got=%s".format(arg.objType()))
    }
    val arr = arg.asInstanceOf[ArrayObj]
    ArrayObj(arr.elements.appended(args[1]))
  }

  val builtins = List(
    Builtin(name = "len", builtin = BuiltinObj(fn = letFunc)),
    Builtin(name = "puts", builtin = BuiltinObj(fn = putsFunc)),
    Builtin(name = "first", builtin = BuiltinObj(fn = firstFunc)),
    Builtin(name = "last", builtin = BuiltinObj(fn = lastFunc)),
    Builtin(name = "rest", builtin = BuiltinObj(fn = restFunc)),
    Builtin(name = "push", builtin = BuiltinObj(fn = pushFunc))
  )

  //  // GetBuiltinByName func
  //  func GetBuiltinByName(name string) *Builtin {
  //    for _, def := range Builtins {
  //      if def.Name == name {
  //        return def.Builtin
  //      }
  //    }
  //    return nil
  //  }
}
