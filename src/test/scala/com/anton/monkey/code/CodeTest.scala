package com.anton.monkey.code

import com.anton.monkey.code.CodeTest.convertBytesToHex
import org.scalatest.FunSuite

class CodeTest extends FunSuite {

  case class TestInput(op: Byte, operands: Array[Int], expected: Array[Byte])

  test("should make bytes") {
    val tests = List(
      TestInput(Code.OpConstant, Array(65534),
        Array(
          Code.putUint8(Code.OpConstant)
          , Code.putUint8(255)
          , Code.putUint8(254)
        ))
      , TestInput(Code.OpAdd, Array(),
        Array(
          Code.putUint8(Code.OpAdd)
        ))
      , TestInput(Code.OpGetLocal, Array(255),
        Array(
          Code.putUint8(Code.OpGetLocal)
          , Code.putUint8(255)
        ))
      , TestInput(Code.OpClosure, Array(65534, 255),
        Array(
          Code.putUint8(Code.OpClosure)
          , Code.putUint8(255)
          , Code.putUint8(254)
          , Code.putUint8(255)
        ))
    )
    tests.foreach(input => {
      val actual = Code.make(input.op, input.operands)
      //assert(actual sameElements input.expected)
      assertArray(actual, input.expected)
    })
  }

  def assertArray(left: Array[Byte], right: Array[Byte]): Unit = {
    if (left.isEmpty || right.isEmpty ||
      left.length != right.length) {
      assert(left sameElements right)
      return
    }

    var index = 0
    val leftStr = convertBytesToHex(left)
    val rightStr = convertBytesToHex(right)
    val msg = String.format("assertArray(%d)(%s = %s)",
      left.length, leftStr, rightStr)
    //println(msg)
    left.foreach(leftElem => {
      val rightElem = right(index)
      assert(leftElem == rightElem)
      index += 1
    })
  }

  test("Should put Uint8,16,32") {
    val expecteds = Map(
      1 -> ("FF01", "FF0001", "FF00000001")
      , 127 -> ("FF7F", "FF007F", "FF0000007F")
      , 128 -> ("FF80", "FF0080", "FF00000080")
      , 255 -> ("FFFF", "FF00FF", "FF000000FF")
      , 256 -> ("FF00", "FF0100", "FF00000100")
      , 65285 -> ("FF05", "FFFF05", "FF0000FF05")
    )
    expecteds.foreachEntry((k, v) => {
      val bytes8: Array[Byte] = Array[Byte](-1, 0)
      val bytes16: Array[Byte] = Array[Byte](-1, 0, 0)
      val bytes32: Array[Byte] = Array[Byte](-1, 0, 0, 0, 0)

      // convert to bytes at position of len(1 or 2)
      Code.putUint8(bytes8, 1, k)
      Code.putUint16(bytes16, 1, k)
      Code.putUint32(bytes32, 1, k)

      val expected8 = v._1
      val expected16 = v._2
      val expected32 = v._3
      val actual8 = convertBytesToHex(bytes8)
      val actual16 = convertBytesToHex(bytes16)
      val actual32 = convertBytesToHex(bytes32)
      assert(actual8 == expected8)
      assert(actual16 == expected16)
      assert(actual32 == expected32)

      // read back and compare to original
      val ins8 = Code.readUint8(bytes8, 1)
      val ins16 = Code.readUint16(bytes16, 1)
      val ins32 = Code.readUint32(bytes32, 1)

      if (k < 256)
        assert(ins8 == k)

      if (k < 65535)
        assert(ins16 == k)

      assert(ins32 == k)
    })
  }

  test("should make instructions") {
    case class Expect(op: Byte, operands: Array[Int], expected: Array[Byte])
    val tests: List[Expect] = List(
      Expect(Code.OpConstant, Array(65534),
        Array(Code.OpConstant, CodeTest.B255, CodeTest.B254))
      , Expect(Code.OpAdd, Array(),
        Array(Code.OpAdd))
      , Expect(Code.OpGetLocal, Array(255),
        Array(Code.OpGetLocal, CodeTest.B255))
      , Expect(Code.OpClosure, Array(65534, 255),
        Array(Code.OpClosure, CodeTest.B255, CodeTest.B254, CodeTest.B255))
    )
    tests.foreach(expect => {
      val instruction = Code.make(expect.op, expect.operands)
      assert(instruction.length == expect.expected.length)
      assert(instruction sameElements expect.expected)
    })
  }

  test("should String instructions") {
    val instructions = Instructions(
      Code.make(Code.OpAdd, Array()) ++
        Code.make(Code.OpGetLocal, Array(1)) ++
        Code.make(Code.OpConstant, Array(2)) ++
        Code.make(Code.OpConstant, Array(65535)) ++
        Code.make(Code.OpClosure, Array(65535, 255))
    )
    val expected = "0000 OpAdd\n" +
      "0001 OpGetLocal 1\n" +
      "0003 OpConstant 2\n" +
      "0006 OpConstant 65535\n" +
      "0009 OpClosure 65535 255\n"
    val actual = instructions.toString()
    assert(actual == expected)
  }


}

object CodeTest {
  val B255: Byte = (-1).toByte
  val B254: Byte = (-2).toByte

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)).toUpperCase())
    }
    sb.toString
  }
}
