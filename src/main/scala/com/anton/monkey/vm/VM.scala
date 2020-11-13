package com.anton.monkey.vm

import com.anton.monkey.code.Code.{OpAdd, OpCode, OpConstant, OpDiv, OpMul, OpPop, OpSub, readUint16}
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.compiler.Bytecode
import com.anton.monkey.objectliteral.ObjectType.{INTEGER_OBJ, STRING_OBJ}
import com.anton.monkey.objectliteral.{BooleanObj, Closure, CompiledFunction, ErrorObj, NullObj, ObjectLiteral}
import com.anton.monkey.vm.VM.stackSize

import scala.collection.mutable.ListBuffer

case class VM(constants: List[ObjectLiteral],
         stack: ListBuffer[ObjectLiteral],
         sp: Int,
         globals: ListBuffer[ObjectLiteral],
         frames: ListBuffer[Frame],
         framesIndex: Int) {

  def lastPoppedStackElem(): ObjectLiteral = {
    stack(sp)
  }

  def run(): ObjectLiteral = {
    var ip : Int = 0
    var ins : Instructions = null
    var op : OpCode = null

    while (currentFrame().ip <
      currentFrame().instructions().instructionArray.length - 1) {

      currentFrame().ip += 1
      ip = currentFrame().ip
      ins = currentFrame().instructions()
      op = ins.instructionArray(ip)

      op match {
        case OpConstant =>
          val constIndex = readUint16(ins.instructionArray,ip+1)
          currentFrame().ip += 2
          val err = push(constants(constIndex))
          if(err != null) {
            return err
          }

        case OpPop =>
          pop()

        case OpAdd | OpSub | OpMul | OpDiv =>
          val err = executeBinaryOperation(op)
          if(err != null) {
            return err
          }

      }
    }
    null
  }


  def currentFrame(): Frame = {
    frames(framesIndex-1)
  }

  def pushFrame(f: Frame) {
    frames.append(f)
    framesIndex += 1
  }

  def popFrame(): Frame = {
    framesIndex -= 1
    frames.remove(framesIndex)
  }

  def push(obj: ObjectLiteral): ErrorObj = {
    if(sp >= stackSize) {
      return ErrorObj(s"Stack Overflow: $sp")
    }
    stack += obj
    sp += 1
    null
  }

  def pop(): ObjectLiteral = {
    if(sp <= 0) {
      return ErrorObj(s"Stack Underflow: $sp")
    }
    sp -= 1
    stack.remove(sp)
  }

  def executeBinaryOperation(op: OpCode): ErrorObj = {
    val right = pop()
    val left = pop()
    val leftType = left.objType()
    val rightType = right.objType()
    if(leftType == INTEGER_OBJ && rightType == INTEGER_OBJ) {
      executeBinaryIntegerOperation(op, left, right)
    } else if(leftType == STRING_OBJ && rightType == STRING_OBJ) {
      executeBinaryStringOperation(op, left, right)
    } else {
      return ErrorObj("Unsupported types for binary " +
        s"operation: $op, $left, $right")
    }
    null
  }

  def executeBinaryIntegerOperation(op: OpCode,
                                    left: ObjectLiteral,
                                    right: ObjectLiteral) {
    null
  }

  def executeBinaryStringOperation(op: OpCode,
                                    left: ObjectLiteral,
                                    right: ObjectLiteral) {
    null
  }

}

object VM {
  // StackSize const
  val stackSize = 2048

  // MaxFrames const
  val maxFrames = 1024

  // True var
  val True: BooleanObj = BooleanObj(true)

  // False var
  val False: BooleanObj = BooleanObj(false)

  // Null var
  val Null: NullObj = NullObj()

  // GlobalSize const
  val globalSize = 65536

  def newVM(bytecode: Bytecode): VM = {
    val mainFn = CompiledFunction(bytecode.instructions,0,0)
    val mainClosure = Closure(mainFn, List())
    val mainFrame = Frame.newFrame(mainClosure, 0)
    val frames = new ListBuffer[Frame]().addOne(mainFrame)
    new VM(
      constants = bytecode.constants,
      stack = new ListBuffer(),
      sp = 0,
      globals = new ListBuffer(),
      frames = frames,
      framesIndex = 1
    )
  }

  def newVMWithGlobalStore(bytecode: Bytecode,
                           s: List[ObjectLiteral]): VM = {
    val vm = newVM(bytecode)
    vm.globals.addAll(s)
    vm
  }
}

