package com.anton.monkey.compiler

import com.anton.monkey.ast._
import com.anton.monkey.code.Code._
import com.anton.monkey.code.{Code, Instructions}
import com.anton.monkey.compiler.SymbolScope.{BuiltinScope, FreeScope, FunctionScope, GlobalScope, LocalScope}
import com.anton.monkey.compiler.SymbolTable.{newEnclosedSymbolTable, newSymbolTableWithBuiltins}
import com.anton.monkey.objectliteral._

import scala.collection.mutable.ListBuffer


case class Bytecode(instructions: Instructions,
                    constants: List[ObjectLiteral])

case class EmittedInstruction(var opcode: OpCode, position: Int)

case class CompilationScope(var instructions: Instructions) {
  var lastInstruction: EmittedInstruction = EmittedInstruction(OpNull,0)
  var previousInstruction: EmittedInstruction = EmittedInstruction(OpNull,1)
}


case class Compiler(constants: ListBuffer[ObjectLiteral],
                    symbolTable: SymbolTable,
                    scopes: ListBuffer[CompilationScope]) {

  var symbolTableVar: SymbolTable = symbolTable
  var scopeIndex: Int = 0
  val EmptyArrayInt: Array[Int] = Array()

  def compile(node: Node): String = { 
    node match {
      case p: Program =>
        for (s <- p.statements) {
          val err = compile(s)
          if (err != null) {
            return err
          }
        }

      case es: ExpressionStatement =>
        val err = compile(es.expression)
        if (err != null) {
          return err
        }
        emit(OpPop, EmptyArrayInt)

      case ie: InfixExpression =>
        // special treatment for '<' to swap order of operands
        // This is to show how to reuse an existing operator.
        var err: String = null
        if (ie.operator == "<") {
          err = compile(ie.right)
          if (err != null) {
            return err
          }
          err = compile(ie.left)
          if (err != null) {
            return err
          }
          emit(OpGreaterThan, EmptyArrayInt)
          return null
        }
        // otherwise order is left then right
        err = compile(ie.left)
        if (err != null) {
          return err
        }
        err = compile(ie.right)
        if (err != null) {
          return err
        }

        ie.operator match {
          case "+" => emit(OpAdd, EmptyArrayInt)
          case "-" => emit(OpSub, EmptyArrayInt)
          case "*" => emit(OpMul, EmptyArrayInt)
          case "/" => emit(OpDiv, EmptyArrayInt)
          case ">" => emit(OpGreaterThan, EmptyArrayInt)
          case "==" => emit(OpEqual, EmptyArrayInt)
          case "!=" => emit(OpNotEqual, EmptyArrayInt)
          case _ => return "unknown operator %s".format(ie.operator)
        }

      case il: IntegerLiteral =>
        val integer = IntegerObj(value = il.value)
        emit(OpConstant, Array(addConstant(integer)))

      case bl: BooleanValue =>
        if (bl.value) {
          emit(OpTrue, EmptyArrayInt)
        } else {
          emit(OpFalse, EmptyArrayInt)
        }

      case pe: PrefixExpression =>
        val err = compile(pe.right)
        if (err != null) {
          return err
        }
        pe.operator match {
          case "!" => emit(OpBang, EmptyArrayInt)
          case "-" => emit(OpMinus, EmptyArrayInt)
          case _ => return "unknown operator %s".format(pe.operator)
        }

      case ie: IfExpression =>
        var err = compile(ie.condition)
        if (err != null) {
          return err
        }

        // Emit an OpJumpNotTruthy with bogus value
        val jumpNotTruthyPos = emit(OpJumpNotTruthy, Array(9999))

        err = compile(ie.consequence)
        if (err != null) {
          return err
        }

        if (lastInstructionIs(OpPop)) {
          removeLastPop()
        }

        // Emit an OpJump with a bogus value
        val jumpPos = emit(OpJump, Array(9999))

        val afterConsequencePos = currentInstructions().instructionArray.length - 1
        changeOperand(jumpNotTruthyPos, afterConsequencePos)

        if (ie.alternative == null) {
          emit(OpNull, EmptyArrayInt)
        } else {
          err = compile(ie.alternative)
          if (err != null) {
            return err
          }
          if (lastInstructionIs(OpPop)) {
            removeLastPop()
          }
        }

        val afterAlternativePos = currentInstructions().instructionArray.length - 1
        changeOperand(jumpPos, afterAlternativePos)

      case bs: BlockStatement =>
        for (s <- bs.statements) {
          val err = compile(s)
          if (err != null) {
            return err
          }
        }

      case ls: LetStatement =>
        val symbol = symbolTable.define(ls.name.value)
        val err = compile(ls.value)
        if (err != null) {
          return err
        }
        if (symbol.scope == GlobalScope) {
          emit(OpSetGlobal, Array(symbol.index))
        } else {
          emit(OpSetLocal, Array(symbol.index))
        }

      case id: Identifier =>
        val (symbol, ok) = symbolTable.resolve(id.value)
        if (!ok) {
          return "undefined variable %s".format(id.value)
        }
        loadSymbol(symbol)

      case sl: StringLiteral =>
        val str = StringObj(value = sl.value)
        emit(OpConstant, Array(addConstant(str)))

      case al: ArrayLiteral =>
        for (el <- al.elements) {
          val err = compile(el)
          if (err != null) {
            return err
          }
        }

        emit(OpArray, Array(al.elements.length))

      case hl: HashLiteral =>
        val keys: ListBuffer[Expression] = new ListBuffer()
        for (pair <- hl.pairs) {
          keys.append(pair._1)
        }
        //      sort.Slice(keys, func(i, j int) bool {
        //        return keys[i].String() < keys[j].String()
        //      })
        for (k <- keys.toList) {
          var err = compile(k)
          if (err != null) {
            return err
          }
          val v = hl.pairs.get(k) match {
            case Some(value) => value
            case None => return null
          }
          err = compile(v)
          if (err != null) {
            return err
          }
        }
        emit(OpHash, Array(keys.length * 2))

      case ie: IndexExpression =>
        var err = compile(ie.left)
        if (err != null) {
          return err
        }
        err = compile(ie.index)
        if (err != null) {
          return err
        }
        emit(OpIndex, EmptyArrayInt)

      case fl: FunctionLiteral =>

        enterScope()
        if (fl.name != "") {
          symbolTable.defineFunctionName(fl.name)
        }
        for (parameter <- fl.parameters) {
          symbolTable.define(parameter.value)
        }

        val err = compile(fl.body)
        if (err != null) {
          return err
        }

        if (lastInstructionIs(OpPop)) {
          replaceLastPopWithReturn()
        }
        if (!lastInstructionIs(OpReturnValue)) {
          emit(OpReturn,EmptyArrayInt)
        }

        val freeSymbols = symbolTable.freeSymbols

        val numLocals = symbolTable.numDefinitions
        val instructions = leaveScope()

        for (free <- freeSymbols) {
          loadSymbol(free)
        }

        val compiledFn = CompiledFunction(
          instructions = instructions,
          numLocals = numLocals,
          numParameters = fl.parameters.length
        )
        val fnIndex = addConstant(compiledFn)
        emit(OpClosure, Array(fnIndex, freeSymbols.length))

      case rs: ReturnStatement =>
        val err = compile(rs.returnValue)
        if (err != null) {
          return err
        }
        emit(OpReturnValue,EmptyArrayInt)

      case cs: CallExpression =>

        val err = compile(cs.function)
        if (err != null) {
          return err
        }

        for (a <- cs.arguments) {
          val err = compile(a)
          if (err != null) {
            return err
          }
        }
        emit(OpCall, Array(cs.arguments.length))
    }

    null
  }


  def bytecode(): Bytecode = {
    Bytecode(
      instructions = currentInstructions(),
      constants = constants.toList
    )
  }

  def addConstant(obj: ObjectLiteral): Int = {
    constants += obj
    constants.length - 1
  }

  def emit(op: Code.OpCode, operands: Array[Int]): Int = {
    val ins = Code.make(op, operands)
    val pos = addInstruction(ins)
    setLastInstruction(op, pos)
    pos
  }

  def addInstruction(ins: Array[Byte]): Int = {
    val posNewInstruction = currentInstructions()
      .instructionArray.length
    val existingInstructions = scopes(scopeIndex)
      .instructions.instructionArray
    val updatedInstructions = Array.concat(existingInstructions,ins)
    scopes(scopeIndex).instructions =
      Instructions(updatedInstructions)
    posNewInstruction
  }

  def setLastInstruction(op: OpCode, pos: Int) {
    val previous = scopes(scopeIndex).lastInstruction
    val last = EmittedInstruction(opcode = op, position = pos)
    scopes(scopeIndex).previousInstruction = previous
    scopes(scopeIndex).lastInstruction = last
  }

  def lastInstructionIs(op: OpCode): Boolean = {
    if (currentInstructions().instructionArray.length == 0) {
      return false
    }
    scopes(scopeIndex).lastInstruction.opcode == op
  }

  def removeLastPop() {
    val last = scopes(scopeIndex).lastInstruction
    val previous = scopes(scopeIndex).previousInstruction
    val oldIns = currentInstructions().instructionArray
    val newIns = oldIns.slice(0,last.position)
    scopes(scopeIndex).instructions = Instructions(newIns)
    scopes(scopeIndex).lastInstruction = previous
  }

  def replaceInstruction(pos: Int, newInstruction: Array[Byte]) {
    val instructions = currentInstructions()
    var i = 0
    for (b <- newInstruction) {
      instructions.instructionArray(pos + i) = b
      i += 1
    }
  }

  def changeOperand(opPos: Int, operand: Int) {
    val instructions = currentInstructions().instructionArray
    val op = instructions(opPos)
    val newInstruction = Code.make(op, Array(operand))
    replaceInstruction(opPos, newInstruction)
  }

  def currentInstructions(): Instructions = {
    scopes(scopeIndex).instructions
  }

  def enterScope() {
    val scope = CompilationScope(Instructions(Array()))
    scopes.append(scope)
    scopeIndex += 1
    symbolTableVar = newEnclosedSymbolTable(symbolTable)
  }

  def leaveScope(): Instructions = {
    val instructions = currentInstructions()
    scopes.remove(scopes.length - 1)
    scopeIndex -= 1
    symbolTableVar = symbolTableVar.outer
    instructions
  }

  def replaceLastPopWithReturn() {
    val lastPos = scopes(scopeIndex).lastInstruction.position
    replaceInstruction(lastPos, Code.make(OpReturnValue, EmptyArrayInt))
    scopes(scopeIndex).lastInstruction.opcode = OpReturnValue
  }

  def loadSymbol(s: Symbol) {
    s.scope match {
      case GlobalScope =>
        emit(OpGetGlobal, Array(s.index))
      case LocalScope =>
        emit(OpGetLocal, Array(s.index))
      case BuiltinScope =>
        emit(OpGetBuiltin, Array(s.index))
      case FreeScope =>
        emit(OpGetFree, Array(s.index))
      case FunctionScope =>
        emit(OpCurrentClosure, Array())
    }
  }

}

object Compiler {

  // New func
  def newCompiler(): Compiler = {
    val symbolTable = newSymbolTableWithBuiltins()
    newWithState(
      constants = new ListBuffer[ObjectLiteral](),
      symbolTable = symbolTable
    )
  }

  // NewWithState func
  def newWithState(constants: ListBuffer[ObjectLiteral],
                   symbolTable: SymbolTable): Compiler = {
    val mainScope = CompilationScope(
      instructions = Instructions(Array())
    )
    Compiler(
      constants = constants,
      symbolTable = symbolTable,
      scopes = new ListBuffer[CompilationScope].append(mainScope)
    )
  }

}
