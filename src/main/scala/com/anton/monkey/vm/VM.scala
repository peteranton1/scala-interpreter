package com.anton.monkey.vm

import com.anton.monkey.code.Code._
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.compiler.Bytecode
import com.anton.monkey.objectliteral.ObjectType.{ARRAY_OBJ, HASH_OBJ, INTEGER_OBJ, STRING_OBJ}
import com.anton.monkey.objectliteral._
import com.anton.monkey.vm.VM.{False, Null, True, stackSize}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class VM(constants: List[ObjectLiteral],
              stack: Array[ObjectLiteral],
              var sp: Int,
              globals: ListBuffer[ObjectLiteral],
              frames: Array[Frame],
              var framesIndex: Int) {
  var lastPopped: ObjectLiteral = Null

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
          val err1 = executeBangOperator()
          if (err1 != null) {
            return err1
          }

        case OpMinus =>
          val err1 = executeMinusOperator()
          if (err1 != null) {
            return err1
          }

        case OpJump =>
          val pos = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip = pos

        case OpJumpNotTruthy =>
          val pos = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          val condition = pop()
          if (!isTruthy(condition)) {
            currentFrame().ip = pos
          }

        case OpNull =>
          val err1 = push(Null)
          if (err1 != null) {
            return err1
          }

        case OpSetGlobal =>
          //val globalIndex = Code.readUint16(ins.instructionArray, ip + 1)
          currentFrame().ip += 2
          globals.addOne(pop())

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
          //below not needed because of buildArray
          //adjustStackPointer( sp - numElements)

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
          adjustStackPointer( frame.basePointer - 1)
          val err1 = push(returnValue)
          if(err1 != null){
            return err1
          }

        case OpReturn =>
          val frame = popFrame()
          adjustStackPointer( frame.basePointer - 1 )
          val err1 = push(Null)
          if(err1 != null){
            return err1
          }

        case OpGetBuiltin =>
          val builtinIndex = Code.readUint8(ins.instructionArray,ip+1)
          currentFrame().ip += 1
          val definition = Builtin.builtins(builtinIndex)
          val err1 = push(definition.builtin)
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

        case _ =>
          return ErrorObj(s"Unknown opcode: $op" +
          s", ip: $ip, ins: ${showBytes(ins.instructionArray)}" +
            s"\n ins: $ins")
      }
    }
    null
  }

  def showBytes(bytes: Array[Byte]):String = {
    bytes.map(b => "" + b.toInt)
      .mkString(" ")
  }

  def push(obj: ObjectLiteral): ErrorObj = {
    if (sp >= stackSize) {
      return ErrorObj(s"Stack Overflow: $sp")
    }
    stack(sp) = obj
    sp += 1
    null
  }

  def pop(): ObjectLiteral = {
    if (sp < 1) {
      return ErrorObj(s"Stack Underflow: $sp")
    }
    if (stack.length < 1) {
      return ErrorObj("Stack Underflow: " +
        s"sp: $sp, stack: ${stack.length}")
    }
    lastPopped = stack(sp-1)
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
    op match {
      case OpEqual =>
        return push(nativeBoolToBooleanObject(right == left))
      case OpNotEqual =>
        return push(nativeBoolToBooleanObject(right != left))
    }
    ErrorObj("unknown operator " +
      s"$op (${left.objType()} ${right.objType()}")
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

  def executeBangOperator(): ErrorObj = {
    val operand = pop()
    operand match {
      case True => push(False)
      case False => push(True)
      case Null => push(True)
      case _ => push(False)
    }
  }

  def executeMinusOperator(): ErrorObj = {
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
      elements.addOne(pop())
      i += 1
    }
    ArrayObj(elements.reverse.toList)
  }

  def buildHash(startIndex: Int, endIndex: Int): (ObjectLiteral, ErrorObj) = {
    val hashPairs = mutable.Map[HashKey, HashPair]()
    var i = startIndex
    while (i < endIndex) {
      val value = pop()
      val key = pop()
      val pair = HashPair(key, value)
      if (!key.isInstanceOf[Hashable]) {
        return (null, ErrorObj(s"unusable as hash key: $key"))
      }
      val hashKey = key.asInstanceOf[Hashable]
      hashPairs(hashKey.hashKey()) = pair
      i += 2
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
    val pair = hashObject.pairs.get(key.hashKey())
    if (pair.isEmpty) {
      return push(Null)
    }
    push(pair.get.value)
  }

  def currentFrame(): Frame = {
    frames(framesIndex - 1)
  }

  def pushFrame(f: Frame) {
    frames(framesIndex) = f
    framesIndex += 1
  }

  def popFrame(): Frame = {
    framesIndex -= 1
    frames(framesIndex)
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

  def adjustStackPointer(spNew: Int) {
    // Because the stack pointer does not automatically
    // reduce the stack, we have to ensure the stack gets
    // reduced.
    val ZERO = 0
    while(spNew >= ZERO && sp > spNew) {
      pop()
    }
    while(sp >= ZERO && sp < spNew) {
      push(Null)
    }
  }

  def callClosure(cl: Closure, numArgs: Int): ErrorObj = {
    if (numArgs != cl.fn.numParameters) {
      return ErrorObj("wrong number of arguments: want=" +
        s"${cl.fn.numParameters}, got=$numArgs")
    }
    val frame = Frame.newFrame(cl, sp - numArgs)
    pushFrame(frame)

    adjustStackPointer(frame.basePointer + cl.fn.numLocals)
    null
  }

  def callBuiltin(builtin: BuiltinObj, numArgs: Int): ErrorObj = {
    val args = getSlice(stack,sp,numArgs)
    val result = builtin.fn(args.toList)
    adjustStackPointer( sp - numArgs - 1 )
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

  def getSlice(aStack: Array[ObjectLiteral],
               sp:Int, numArgs: Int): Array[ObjectLiteral] = {
    val localStackSize = sp - numArgs
    val aLocalStack = new Array[ObjectLiteral](localStackSize)
    var i = 0
    while(i < localStackSize) {
      aLocalStack(i) = aStack(sp - i)
      i += 1
    }
    aLocalStack
  }

  def isTruthy(obj: ObjectLiteral): Boolean = {
    obj match {
      case bo: BooleanObj => bo.value
      case Null => false
      case _ => true
    }
  }

}

object VM {
  // StackSize const
  val stackSize = 2048

  // MaxFrames const
  val maxFrames = 1048

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
    val stack = new Array[ObjectLiteral](stackSize)
    val frames = new Array[Frame](maxFrames)
    frames(0) = mainFrame
    new VM(
      constants = bytecode.constants,
      stack = stack,
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

