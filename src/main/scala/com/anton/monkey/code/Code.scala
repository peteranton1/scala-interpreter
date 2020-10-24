package com.anton.monkey.code

import java.nio.ByteBuffer

import scala.util.control.Breaks.break

case class Instructions(instructionArray: Array[Byte]) {
  def String(): String = {
    val buf = new StringBuilder
    var bp = 0
    while (bp < instructionArray.length) {
      val opCode: Byte = instructionArray(bp)
      val (defn, err) = Code.lookup(opCode)
      if (err != null) {
        buf.append(err)
        break
      }
      bp += 1 // skip opcode
      val (operands, bytesRead) = readOperands(defn, instructionArray, bp)
      val formatted = fmtInstruction(defn, operands)
      buf.append("%04d %s\n".format(bp-1, formatted))
      bp += bytesRead
    }
    buf.toString()
  }

  private def fmtInstruction(defn: Definition, operands: Array[Int]): String = {
    val operandCount = defn.operandWidths.length

    if (operands.length != operandCount) {
      return "ERROR: operand len %d does not match defined %d\n"
        .format(operands.length, operandCount)
    }
    operandCount match {
      case 0 =>
        return defn.name
      case 1 =>
        return "%s %d".format(defn.name, operands(0))
      case 2 =>
        return "%s %d %d".format(defn.name, operands(0), operands(1))
    }
    "ERROR: unhandled operandCount for %s\n".format(defn.name)
  }

  // readOperands func
  private def readOperands(defn: Definition, value: Array[Byte], bp: Int): (Array[Int], Int) = {
    var offsetBytes = bp
    var offsetInts = 0
    var bytesRead = 0
    val operands = new Array[Int](defn.operandWidths.length)
    for (width <- defn.operandWidths) {
      operands(offsetInts) = width match {
        case 2 => Code.readUint16(value, offsetBytes)
        case 1 => Code.readUint8(value, offsetBytes)
      }
      offsetBytes += width
      bytesRead += width
      offsetInts += 1
    }
    (operands, bytesRead)
  }
}

case class Definition(name: String,
                      operandWidths: List[Int])

object Code {

  def lookup(op: OpCode): (Definition, String) =
    definitions.get(op) match {
      case Some(value) => (value, null)
      case None => (null, String.format("opcode %d undefined", op))
    }

  def make(op: OpCode, operands: Array[Int]): Array[Byte] = {
    val definition = definitions.get(op) match {
      case None => return Array[Byte]()
      case Some(value) => value
    }
    var instructionLen = 1
    definition.operandWidths.foreach(i => instructionLen += i)
    val instruction = new Array[Byte](instructionLen)
    instruction(0) = op
    var offset = 1
    var n = 0
    operands.foreach(o => {
      val width = definition.operandWidths(n)
      width match {
        case 2 => putUint16(instruction, offset, o)
        case 1 => putUint8(instruction, offset, o)
      }
      n += 1
      offset += width
    })
    instruction
  }

  def putUint32(bytes: Array[Byte], index: Int, value: Int): Array[Byte] = {
    val intbytes = intToByteArray(value)
    bytes(index) = intbytes(0)
    bytes(index + 1) = intbytes(1)
    bytes(index + 2) = intbytes(2)
    bytes(index + 3) = intbytes(3)
    bytes
  }

  def putUint16(bytes: Array[Byte], index: Int, value: Int): Array[Byte] = {
    val intbytes = intToByteArray(value)
    bytes(index) = intbytes(2)
    bytes(index + 1) = intbytes(3)
    bytes
  }

  def putUint8(bytes: Array[Byte], index: Int, value: Int): Array[Byte] = {
    val intbytes = intToByteArray(value)
    bytes(index) = intbytes(3)
    bytes
  }

  def readUint32(bytes: Array[Byte], bp: Int): Int = readInt(bytes, bp, 4)

  def readUint16(bytes: Array[Byte], bp: Int): Int = readInt(bytes, bp, 2)

  def readUint8(bytes: Array[Byte], bp: Int): Int = readInt(bytes, bp, 1)

  def putUint8(value: Int): Byte = {
    val intbytes = intToByteArray(value)
    intbytes(3)
  }

  /** extract an integer at position bp from buf using len bytes
   */
  private def readInt(bytes: Array[Byte], bp: Int, len: Int): Int = {
    len match {
      case 4 =>
        ((bytes(bp) & 0xff) << 24) +
          ((bytes(bp + 1) & 0xff) << 16) +
          ((bytes(bp + 2) & 0xff) << 8) +
          (bytes(bp + 3) & 0xff)
      case 2 =>
        ((bytes(bp) & 0xff) << 8) +
          (bytes(bp + 1) & 0xff)
      case 1 =>
        bytes(bp) & 0xff
    }
  }

  private def intToByteArray(value: Int): Array[Byte] = {
    ByteBuffer.allocate(4).putInt(value).array()
  }

  type OpCode = Byte
  val OpConstant: OpCode = 51.toByte
  val OpPop: OpCode = 52.toByte
  val OpAdd: OpCode = 53.toByte
  val OpSub: OpCode = 54.toByte
  val OpMul: OpCode = 55.toByte
  val OpDiv: OpCode = 56.toByte
  val OpTrue: OpCode = 57.toByte
  val OpFalse: OpCode = 58.toByte
  val OpEqual: OpCode = 59.toByte
  val OpNotEqual: OpCode = 60.toByte
  val OpGreaterThan: OpCode = 61.toByte
  val OpMinus: OpCode = 62.toByte
  val OpBang: OpCode = 63.toByte
  val OpJumpNotTruthy: OpCode = 64.toByte
  val OpJump: OpCode = 65.toByte
  val OpNull: OpCode = 66.toByte
  val OpGetGlobal: OpCode = 67.toByte
  val OpSetGlobal: OpCode = 68.toByte
  val OpArray: OpCode = 69.toByte
  val OpHash: OpCode = 70.toByte
  val OpIndex: OpCode = 71.toByte
  val OpCall: OpCode = 72.toByte
  val OpReturnValue: OpCode = 73.toByte
  val OpReturn: OpCode = 74.toByte
  val OpGetLocal: OpCode = 75.toByte
  val OpSetLocal: OpCode = 76.toByte
  val OpGetBuiltin: OpCode = 77.toByte
  val OpClosure: OpCode = 78.toByte
  val OpGetFree: OpCode = 79.toByte
  val OpCurrentClosure: OpCode = 80.toByte

  val definitions: Map[OpCode, Definition] = Map(
    OpConstant -> Definition("OpConstant", List(2)),
    OpPop -> Definition("OpPop", List()),
    OpAdd -> Definition("OpAdd", List()),
    OpSub -> Definition("OpSub", List()),
    OpMul -> Definition("OpMul", List()),
    OpDiv -> Definition("OpDiv", List()),
    OpTrue -> Definition("OpTrue", List()),
    OpFalse -> Definition("OpFalse", List()),
    OpEqual -> Definition("OpEqual", List()),
    OpNotEqual -> Definition("OpNotEqual", List()),
    OpGreaterThan -> Definition("OpGreaterThan", List()),
    OpMinus -> Definition("OpMinus", List()),
    OpBang -> Definition("OpBang", List()),
    OpJumpNotTruthy -> Definition("OpJumpNotTruthy", List(2)),
    OpJump -> Definition("OpJump", List(2)),
    OpNull -> Definition("OpNull", List()),
    OpGetGlobal -> Definition("OpGetGlobal", List(2)),
    OpSetGlobal -> Definition("OpSetGlobal", List(2)),
    OpArray -> Definition("OpArray", List(2)),
    OpHash -> Definition("OpHash", List(2)),
    OpIndex -> Definition("OpIndex", List()),
    OpCall -> Definition("OpCall", List(1)),
    OpReturnValue -> Definition("OpReturnValue", List()),
    OpReturn -> Definition("OpReturn", List()),
    OpGetLocal -> Definition("OpGetLocal", List(1)),
    OpSetLocal -> Definition("OpSetLocal", List(1)),
    OpGetBuiltin -> Definition("OpGetBuiltin", List(1)),
    OpClosure -> Definition("OpClosure", List(2, 1)),
    OpGetFree -> Definition("OpGetFree", List(1)),
    OpCurrentClosure -> Definition("OpCurrentClosure", List())
  )
}