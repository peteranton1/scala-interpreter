package com.anton.monkey.vm

import com.anton.monkey.code.Code.{OpAdd, OpArray, OpBang, OpCall, OpClosure, OpCode, OpConstant, OpCurrentClosure, OpDiv, OpEqual, OpFalse, OpGetBuiltin, OpGetFree, OpGetGlobal, OpGetLocal, OpGreaterThan, OpHash, OpIndex, OpJump, OpJumpNotTruthy, OpMinus, OpMul, OpNotEqual, OpNull, OpPop, OpReturn, OpReturnValue, OpSetGlobal, OpSetLocal, OpSub, OpTrue, readUint16}
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.compiler.Bytecode
import com.anton.monkey.objectliteral.ObjectType.{ARRAY_OBJ, HASH_OBJ, INTEGER_OBJ, STRING_OBJ}
import com.anton.monkey.objectliteral.{ArrayObj, BooleanObj, BuiltinObj, Closure, CompiledFunction, ErrorObj, HashKey, HashObj, HashPair, Hashable, IntegerObj, NullObj, ObjectLiteral, StringObj}
import com.anton.monkey.vm.VM.{False, Null, True, stackSize}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class VM(constants: List[ObjectLiteral],
              stack: ListBuffer[ObjectLiteral],
              var sp: Int,
              globals: ListBuffer[ObjectLiteral],
              frames: ListBuffer[Frame],
              var framesIndex: Int) {
  var lastPopped: ObjectLiteral = null

  def lastPoppedStackElem(): ObjectLiteral = {
    lastPopped
  }

  def run(): ObjectLiteral = {
    var ip: Int = 0
    var ins: Instructions = null
    var op: OpCode = 0.toByte

    while (currentFrame().ip <
      currentFrame().instructions().instructionArray.length - 1) {

      currentFrame().ip += 1
      ip = currentFrame().ip
      ins = currentFrame().instructions()
      op = ins.instructionArray(ip)

      op match {
        case OpConstant =>
          val constIndex = readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          val err1 = push(constants(constIndex))
          if (err1 != null) {
            return err1
          }

        case OpPop =>
          pop()

        case OpAdd | OpSub | OpMul | OpDiv =>
          val err1 = executeBinaryOperation(op)
          if (err1 != null) {
            return err1
          }

        case OpTrue =>
          val err1 = push(True)
          if (err1 != null) {
            return err1
          }

        case OpFalse =>
          val err1 = push(False)
          if (err1 != null) {
            return err1
          }

        case OpEqual | OpNotEqual | OpGreaterThan =>
          val err1 = executeComparison(op)
          if (err1 != null) {
            return err1
          }

        case OpBang =>
          val err1 = executeBangOperator(op)
          if (err1 != null) {
            return err1
          }

        case OpMinus =>
          val err1 = executeMinusOperator(op)
          if (err1 != null) {
            return err1
          }

        case OpJump =>
          val pos = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip = pos - 1

        case OpJumpNotTruthy =>
          val pos = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          val condition = pop()
          if (!isTruthy(condition)) {
            currentFrame().ip = pos - 1
          }

        case OpNull =>
          val err1 = push(Null)
          if (err1 != null) {
            return err1
          }

        case OpSetGlobal =>
          val globalIndex = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          globals(globalIndex) = pop()

        case OpGetGlobal =>
          val globalIndex = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          val err1 = push(globals(globalIndex))
          if (err1 != null) {
            return err1
          }

        case OpSetLocal =>
          val localIndex = Code.readUint8(ins.instructionArray, ip + 1)
          currentFrame().ip += 1
          val frame = currentFrame()
          stack(frame.basePointer + localIndex) = pop()

        case OpGetLocal =>
          val localIndex = Code.readUint8(ins.instructionArray, ip + 1)
          currentFrame().ip += 1
          val frame = currentFrame()
          val err1 = push(stack(frame.basePointer + localIndex))
          if (err1 != null) {
            return err1
          }

        case OpGetFree =>
          val freeIndex = Code.readUint8(ins.instructionArray, ip + 1)
          currentFrame().ip += 1
          val currentClosure = currentFrame().cl
          val err1 = push(currentClosure.free(freeIndex))
          if (err1 != null) {
            return err1
          }

        case OpArray =>
          val numElements = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          val array = buildArray(sp - numElements, sp)
          sp = sp - numElements

          val err1 = push(array)
          if (err1 != null) {
            return err1
          }

        case OpHash =>
          val numElements = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          val (hash, err1) = buildHash(sp - numElements, sp)
          if (err1 != null) {
            return err1
          }
          sp = sp - numElements
          val err2 = push(hash)
          if (err2 != null) {
            return err2
          }

        case OpIndex =>
          val index = pop()
          val left = pop()
          val err1 = executeIndexExpression(left, index)
          if (err1 != null) {
            return err1
          }

        case OpCall =>
          val numArgs = Code.readUint8(ins.instructionArray, ip + 1)
          currentFrame().ip += 1
          val err1 = executeCall(numArgs)
          if (err1 != null) {
            return err1
          }

        case OpReturnValue =>
          val returnValue = pop()
          val frame = popFrame()
          sp = frame.basePointer - 1
          val err1 = push(returnValue)
          if(err1 != null){
            return err1
          }

        case OpReturn =>
          val frame = popFrame()
          sp = frame.basePointer - 1
          val err1 = push(Null)
          if(err1 != null){
            return err1
          }

        case OpGetBuiltin =>
          val frame = popFrame()
          sp = frame.basePointer - 1
          val err1 = push(Null)
          if(err1 != null){
            return err1
          }

        case OpClosure =>
          val constIndex = Code.readUint16(ins.instructionArray, ip + 1)
          val numFree = Code.readUint8(ins.instructionArray, ip + 3)
          currentFrame().ip += 3
          val err1 = pushClosure(constIndex, numFree)
          if(err1 != null){
            return err1
          }

        case OpCurrentClosure =>
          val currentClosure = currentFrame().cl
          val err1 = push(currentClosure)
          if(err1 != null){
            return err1
          }

      }
    }
    null
  }

  def push(obj: ObjectLiteral): ErrorObj = {
    if (sp >= stackSize) {
      return ErrorObj(s"Stack Overflow: $sp")
    }
    stack += obj
    sp += 1
    null
  }

  def pop(): ObjectLiteral = {
    if (sp < 0) {
      return ErrorObj(s"Stack Underflow: $sp")
    }
    lastPopped = stack.remove(sp-1)
    sp -= 1
    lastPopped
  }

  def executeBinaryOperation(op: OpCode): ErrorObj = {
    val right = pop()
    val left = pop()
    val leftType = left.objType()
    val rightType = right.objType()
    if (leftType == INTEGER_OBJ && rightType == INTEGER_OBJ) {
      executeBinaryIntegerOperation(op, left, right)
    } else if (leftType == STRING_OBJ && rightType == STRING_OBJ) {
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
    if (left.objType() == INTEGER_OBJ && right.objType() == INTEGER_OBJ) {
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
    if (operand.objType() != INTEGER_OBJ) {
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
    while (i < endIndex) {
      elements.addOne(stack(i))
      i += 1
    }
    ArrayObj(elements.toList)
  }

  def buildHash(startIndex: Int, endIndex: Int): (ObjectLiteral, ErrorObj) = {
    val hashPairs = mutable.Map[HashKey, HashPair]()
    var i = startIndex
    while (i < endIndex) {
      val key = stack(i)
      val value = stack(i + 1)
      val pair = HashPair(key, value)
      if (!key.isInstanceOf[Hashable]) {
        return (null, ErrorObj(s"unusable as hash key: $key"))
      }
      val hashKey = key.asInstanceOf[Hashable]
      hashPairs(hashKey.hashKey()) = pair
      i += 1
    }
    (HashObj(hashPairs), null)
  }

  def executeIndexExpression(left: ObjectLiteral,
                             index: ObjectLiteral): ErrorObj = {
    if (left.objType() == ARRAY_OBJ && index.objType() == INTEGER_OBJ) {
      return executeArrayIndex(left.asInstanceOf[ArrayObj], index)
    }
    if (left.objType() == HASH_OBJ && index.objType() == INTEGER_OBJ) {
      return executeHashIndex(left.asInstanceOf[HashObj], index)
    }
    ErrorObj("index operator not supported: ${left.objType()}")
  }

  def executeArrayIndex(arrayObject: ArrayObj,
                        index: ObjectLiteral): ErrorObj = {
    val i = index.asInstanceOf[IntegerObj].value
    val max = arrayObject.elements.length - 1
    if (i < 0 || i > max) {
      return push(Null)
    }
    push(arrayObject.elements(i))
  }

  def executeHashIndex(hashObject: HashObj,
                       index: ObjectLiteral): ErrorObj = {
    if (!index.isInstanceOf[Hashable]) {
      return ErrorObj(s"unusable as hash key: $index")
    }
    val key = index.asInstanceOf[Hashable]
    val pair = hashObject.pairs(key.hashKey())
    if (pair == null) {
      push(Null)
    }
    push(pair.value)
  }

  def currentFrame(): Frame = {
    frames(framesIndex - 1)
  }

  def pushFrame(f: Frame) {
    frames.append(f)
    framesIndex += 1
  }

  def popFrame(): Frame = {
    framesIndex -= 1
    frames.remove(framesIndex)
  }

  def executeCall(numArgs: Int): ErrorObj = {
    val callee = stack(sp - 1 - numArgs)
    callee match {
      case cl: Closure =>
        return callClosure(cl, numArgs)
      case bo: BuiltinObj =>
        return callBuiltin(bo, numArgs)
    }
    ErrorObj("calling non-function and non-builtin")
  }

  def callClosure(cl: Closure, numArgs: Int): ErrorObj = {
    if (numArgs != cl.fn.numParameters) {
      return ErrorObj("wrong number of arguments: want=" +
        s"${cl.fn.numParameters}, got=$numArgs")
    }
    val frame = Frame.newFrame(cl, sp - numArgs)
    pushFrame(frame)

    sp = frame.basePointer + cl.fn.numLocals
    null
  }

  def callBuiltin(builtin: BuiltinObj, numArgs: Int): ErrorObj = {
    val args = getSlice(stack,sp,numArgs)
    val result = builtin.fn(args.toList)
    sp = sp - numArgs - 1
    if(result != null) {
      push(result)
    } else {
      push(Null)
    }
    null
  }

  def pushClosure(constIndex: Int, numFree: Int): ErrorObj = {
    val constant = constants(constIndex)
    if(!constant.isInstanceOf[CompiledFunction]) {
      return ErrorObj(s"not a function: ${constant.objType()}")
    }
    val function = constant.asInstanceOf[CompiledFunction]
    val free = new ListBuffer[ObjectLiteral]()
    var i = 0
    while(i<numFree){
      free.addOne(stack(sp-numFree+i))
      i += 1
    }
    val closure = Closure(function, free.toList)
    push(closure)
  }

  def getSlice(aListBuf: ListBuffer[ObjectLiteral],
               sp:Int, numArgs: Int): ListBuffer[ObjectLiteral] = {
    val buf = new ListBuffer[ObjectLiteral]()
    var i = sp - numArgs
    while(i <= sp) {
      buf.addOne(aListBuf(i))
      i += 1
    }
    buf
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
    val mainFn = CompiledFunction(bytecode.instructions, 0, 0)
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

