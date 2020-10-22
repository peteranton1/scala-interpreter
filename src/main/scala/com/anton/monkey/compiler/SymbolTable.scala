package com.anton.monkey.compiler

import com.anton.monkey.compiler.SymbolScope.{BuiltinScope, FreeScope, GlobalScope, LocalScope}
import com.anton.monkey.objectliteral.Builtin

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object SymbolScope extends Enumeration {

  protected case class Val(name: String) extends super.Val

  val GlobalScope: Val = Val("GLOBAL")
  val LocalScope: Val = Val("LOCAL")
  val BuiltinScope: Val = Val("BUILTIN")
  val FreeScope: Val = Val("FREE")
  val FunctionScope: Val = Val("FUNCTION")
}

case class Symbol(name: String, scope: SymbolScope.Value, index: Int)

case class SymbolTable(outer: SymbolTable,
                       store: mutable.Map[String, Symbol],
                       freeSymbols: ListBuffer[Symbol]) {
  var numDefinitions: Int = 0

  def define(name: String): Symbol = {
    val symbol = Symbol(
      name = name,
      index = numDefinitions,
      scope = if (this.outer == null)
        GlobalScope
      else
        LocalScope
    )
    this.store(name) = symbol
    numDefinitions += 1
    symbol
  }

  def resolve(name: String): (Symbol, Boolean) = {
    val (symbol, ok) = Try(this.store(name)) match {
      case Success(value) => (value, true)
      case Failure(_) => (null, false)
    }
    if (!ok && outer != null) {
      val (symbol, ok) = this.outer.resolve(name)
      if (!ok) {
        return (symbol, ok)
      }
      if (symbol.scope == GlobalScope ||
        symbol.scope == BuiltinScope) {
        return (symbol, ok)
      }
      val free = defineFree(symbol)
      return (free, true)
    }
    (symbol, ok)
  }

  def defineBuiltin(index: Int, name: String): Symbol = {
    val symbol = Symbol(
      name = name,
      index = index,
      scope = BuiltinScope
    )
    this.store(name) = symbol
    symbol
  }

  def defineFree(original: Symbol): Symbol = {
    this.freeSymbols.append(original)
    val symbol = Symbol(
      name = original.name,
      index = freeSymbols.length - 1,
      scope = FreeScope
    )
    this.store(original.name) = symbol
    symbol
  }

  def defineFunctionName(name: String): Symbol = {
    val symbol = Symbol(name = name, index = 0,
      scope = SymbolScope.FunctionScope)
    this.store(name) = symbol
    symbol
  }

}

object SymbolTable {
  def newSymbolTable(): SymbolTable = {
    newEnclosedSymbolTable(outer = null)
  }

  def newSymbolTableWithBuiltins(): SymbolTable = {
    val symbolTable = newEnclosedSymbolTable(outer = null)
    var i = 0
    for (builtin <- Builtin.builtins) {
      symbolTable.defineBuiltin(i, builtin.name)
      i += 1
    }
    symbolTable
  }

  def newEnclosedSymbolTable(outer: SymbolTable): SymbolTable = {
    val store = mutable.Map[String, Symbol]()
    val free = new ListBuffer[Symbol]()
    SymbolTable(outer = outer, store = store, freeSymbols = free)
  }
}