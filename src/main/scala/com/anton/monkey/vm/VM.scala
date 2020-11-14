package com.anton.monkey.vm

import com.anton.monkey.code.Code.{OpAdd, OpBang, OpCode, OpConstant, OpDiv, OpEqual, OpFalse, OpGreaterThan, OpJump, OpJumpNotTruthy, OpMinus, OpMul, OpNotEqual, OpPop, OpSub, OpTrue, readUint16}
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.compiler.Bytecode
import com.anton.monkey.objectliteral.ObjectType.{INTEGER_OBJ, STRING_OBJ}
import com.anton.monkey.objectliteral.{ArrayObj, BooleanObj, Closure, CompiledFunction, ErrorObj, IntegerObj, NullObj, ObjectLiteral, StringObj}
import com.anton.monkey.vm.VM.{False, Null, True, stackSize}

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

        case OpTrue =>
          val err = push(True)
          if(err != null) {
            return err
          }

        case OpFalse =>
          val err = push(False)
          if(err != null) {
            return err
          }

        case OpEqual | OpNotEqual | OpGreaterThan =>
          val err = executeComparison(op)
          if(err != null) {
            return err
          }

        case OpBang =>
          val err = executeBangOperator(op)
          if(err != null) {
            return err
          }

        case OpMinus =>
          val err = executeMinusOperator(op)
          if(err != null) {
            return err
          }

        case OpJump =>
          val pos = Code.readUint16(ins.instructionArray,ip+1)
          currentFrame().ip = pos - 1

        case OpJumpNotTruthy =>
          val pos = Code.readUint16(ins.instructionArray,ip+1)
          currentFrame().ip += 2
          val condition = pop()
          if(!isTruthy(condition)) {
            currentFrame().ip = pos - 1
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
                                    right: ObjectLiteral): ErrorObj = {
    val leftValue = left.asInstanceOf[IntegerObj]
    val rightValue = right.asInstanceOf[IntegerObj]
    var result: Int = 0
    op match {
      case OpAdd => result = leftValue.value + rightValue.value
      case OpSub => result = leftValue.value - rightValue.value
      case OpMul => result = leftValue.value * rightValue.value
      case OpDiv => result = leftValue.value / rightValue.value
      case _ => return ErrorObj(s"unknown integer operator: $op")
    }
    push(IntegerObj(result))
  }

  def executeBinaryStringOperation(op: OpCode,
                                    left: ObjectLiteral,
                                    right: ObjectLiteral): ErrorObj = {
    val leftValue = left.asInstanceOf[StringObj]
    val rightValue = right.asInstanceOf[StringObj]
    var result: String = null
    op match {
      case OpAdd => result = leftValue.value + rightValue.value
      case _ => return ErrorObj(s"unknown string operator: $op")
    }
    push(StringObj(result))
  }

  def executeComparison(op: OpCode): ErrorObj = {
    val right = pop()
    val left = pop()
    if(left.objType() == INTEGER_OBJ && right.objType() == INTEGER_OBJ) {
      return executeIntegerComparison(op, left, right)
    }
    null
  }

  def executeIntegerComparison(op: OpCode,
                               left: ObjectLiteral,
                               right: ObjectLiteral): ErrorObj = {
    val leftValue = left.asInstanceOf[IntegerObj].value
    val rightValue = right.asInstanceOf[IntegerObj].value

    op match {
      case OpEqual =>
        return push(nativeBoolToBooleanObject(leftValue == rightValue))
      case OpNotEqual =>
        return push(nativeBoolToBooleanObject(leftValue != rightValue))
      case OpGreaterThan =>
        return push(nativeBoolToBooleanObject(leftValue > rightValue))
    }
    ErrorObj(s"unknown operator $op")
  }

  def executeBangOperator(op: OpCode): ErrorObj = {
    val operand = pop()
    operand match {
      case True => return push(False)
      case False => return push(True)
      case Null => return push(True)
    }
    push(False)
  }

  def executeMinusOperator(op: OpCode): ErrorObj = {
    val operand = pop()
    if(operand.objType() != INTEGER_OBJ) {
      return ErrorObj(s"unsupported type for negation: $operand")
    }
    val value = operand.asInstanceOf[IntegerObj].value
    push(IntegerObj(-value))
  }

  def nativeBoolToBooleanObject(input: Boolean): BooleanObj = {
    if (input) {
      return True
    }
    False
  }

  def buildArray(startIndex: Int, endIndex: Int): ObjectLiteral = {
    val elements = new ListBuffer[ObjectLiteral]()
    var i = startIndex
    while(i < endIndex) {
      elements(i-startIndex) = stack(i)
      i += 1
    }
    ArrayObj(elements.toList)
  }


  def isTruthy(obj: ObjectLiteral): Boolean = {
    obj match {
      case bo: BooleanObj => return bo.value
      case Null => return false
    }
    true
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

