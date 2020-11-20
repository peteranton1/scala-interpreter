package com.anton.monkey.objectliteral

import org.scalatest.FunSuite

class BuiltinTest extends FunSuite {

  case class TestData(args: List[ObjectLiteral], expected: ObjectLiteral)

  test("should lenFunc") {
    val obj = IntegerObj(1)
    val tests = List(
      TestData(List(ArrayObj(List())),IntegerObj(0))
      ,TestData(List(ArrayObj(List(obj))),IntegerObj(1))
      ,TestData(List(ArrayObj(List(obj,obj))),IntegerObj(2))
      ,TestData(List(ArrayObj(List(obj,obj,obj))),IntegerObj(3))
      ,TestData(List(StringObj("")),IntegerObj(0))
      ,TestData(List(StringObj("a")),IntegerObj(1))
      ,TestData(List(StringObj("ab")),IntegerObj(2))
      ,TestData(List(StringObj("abc")),IntegerObj(3))
      ,TestData(List(IntegerObj(1)),ErrorObj(
        "argument to 'len' not supported, got=1"
      ))
    )
    for (tt <- tests) {
      val actual = Builtin.lenFunc(tt.args)
      assert (actual == tt.expected)
      val builtin = Builtin.getBuiltinByName("len")
      val actual2 = builtin.fn(tt.args)
      assert (actual2 == tt.expected)
    }
  }

  test("should putsFunc") {
    val obj = IntegerObj(1)
    val tests = List(
      TestData(List(ArrayObj(List())),NullObj())
      ,TestData(List(ArrayObj(List(obj))),NullObj())
      ,TestData(List(ArrayObj(List(obj,obj))),NullObj())
      ,TestData(List(StringObj("")),NullObj())
      ,TestData(List(StringObj("a")),NullObj())
      ,TestData(List(StringObj("ab")),NullObj())
      ,TestData(List(IntegerObj(1)),NullObj())
    )
    for (tt <- tests) {
      val actual = Builtin.putsFunc(tt.args)
      assert (actual == tt.expected)
      val builtin = Builtin.getBuiltinByName("puts")
      val actual2 = builtin.fn(tt.args)
      assert (actual2 == tt.expected)
    }
  }

  test("should firstFunc") {
    val obj1 = IntegerObj(1)
    val obj2 = IntegerObj(2)
    val obj3 = IntegerObj(3)
    val tests = List(
      TestData(List(ArrayObj(List())),ErrorObj(
        "argument to 'first' must not be empty, got=0"
      ))
      ,TestData(List(ArrayObj(List(obj1))),obj1)
      ,TestData(List(ArrayObj(List(obj1,obj2))),obj1)
      ,TestData(List(ArrayObj(List(obj1,obj2,obj3))),obj1)
      ,TestData(List(StringObj("")),ErrorObj(
        "argument to 'first' must be ARRAY, got=STRING"
        ))
      ,TestData(List(IntegerObj(1)),ErrorObj(
        "argument to 'first' must be ARRAY, got=INTEGER"
      )))
    for (tt <- tests) {
      val actual = Builtin.firstFunc(tt.args)
      assert (actual == tt.expected)
      val builtin = Builtin.getBuiltinByName("first")
      val actual2 = builtin.fn(tt.args)
      assert (actual2 == tt.expected)
    }
  }

  test("should lastFunc") {
    val obj1 = IntegerObj(1)
    val obj2 = IntegerObj(2)
    val obj3 = IntegerObj(3)
    val tests = List(
      TestData(List(ArrayObj(List())),ErrorObj(
        "argument to 'last' must not be empty, got=0"
      ))
      ,TestData(List(ArrayObj(List(obj1))),obj1)
      ,TestData(List(ArrayObj(List(obj1,obj2))),obj2)
      ,TestData(List(ArrayObj(List(obj1,obj2,obj3))),obj3)
      ,TestData(List(StringObj("")),ErrorObj(
        "argument to 'last' must be ARRAY, got=STRING"
        ))
      ,TestData(List(IntegerObj(1)),ErrorObj(
        "argument to 'last' must be ARRAY, got=INTEGER"
      )))
    for (tt <- tests) {
      val actual = Builtin.lastFunc(tt.args)
      assert (actual == tt.expected)
      val builtin = Builtin.getBuiltinByName("last")
      val actual2 = builtin.fn(tt.args)
      assert (actual2 == tt.expected)
    }
  }

  test("should restFunc") {
    val obj1 = IntegerObj(1)
    val obj2 = IntegerObj(2)
    val obj3 = IntegerObj(3)
    val tests = List(
      TestData(List(ArrayObj(List())),ErrorObj(
        "argument to 'rest' must not be empty, got=0"
      ))
      ,TestData(List(ArrayObj(List(obj1))),ArrayObj(List()))
      ,TestData(List(ArrayObj(List(obj1,obj2))),ArrayObj(List(obj2)))
      ,TestData(List(ArrayObj(List(obj1,obj2,obj3))),ArrayObj(List(obj2,obj3)))
      ,TestData(List(StringObj("")),ErrorObj(
        "argument to 'rest' must be ARRAY, got=STRING"
        ))
      ,TestData(List(IntegerObj(1)),ErrorObj(
        "argument to 'rest' must be ARRAY, got=INTEGER"
      )))
    for (tt <- tests) {
      val actual = Builtin.restFunc(tt.args)
      assert (actual == tt.expected)
      val builtin = Builtin.getBuiltinByName("rest")
      val actual2 = builtin.fn(tt.args)
      assert (actual2 == tt.expected)
    }
  }

  test("should pushFunc") {
    val obj1 = IntegerObj(1)
    val obj2 = IntegerObj(2)
    val obj3 = IntegerObj(3)
    val tests = List(
      TestData(List(ArrayObj(List())),ErrorObj(
        "wrong number of arguments, got=1, want=2"
      ))
      ,TestData(List(ArrayObj(List()),obj1),ArrayObj(List(obj1)))
      ,TestData(List(ArrayObj(List(obj1)),obj2),ArrayObj(List(obj1,obj2)))
      ,TestData(List(ArrayObj(List(obj1,obj2)),obj3),ArrayObj(List(obj1,obj2,obj3)))
      ,TestData(List(StringObj(""),obj1),ErrorObj(
        "argument to 'push' must be ARRAY, got=STRING"
        ))
      ,TestData(List(IntegerObj(1),obj1),ErrorObj(
        "argument to 'push' must be ARRAY, got=INTEGER"
      )))
    for (tt <- tests) {
      val actual = Builtin.pushFunc(tt.args)
      assert (actual == tt.expected)
      val builtin = Builtin.getBuiltinByName("push")
      val actual2 = builtin.fn(tt.args)
      assert (actual2 == tt.expected)
    }
  }

  test("should get builtins") {
    val tests = List(
      "len","puts","first","last","rest","push"
    )
    for (tt <- tests) {
      val builtin = Builtin.getBuiltinByName(tt)
      assert (builtin != null)
    }
    val builtin = Builtin.getBuiltinByName("unknown")
    assert (builtin == null)
  }
}
