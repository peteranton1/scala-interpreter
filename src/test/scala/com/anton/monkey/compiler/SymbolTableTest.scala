package com.anton.monkey.compiler

import com.anton.monkey.compiler.SymbolScope.{BuiltinScope, FreeScope, GlobalScope, LocalScope}
import com.anton.monkey.compiler.SymbolTable.{newEnclosedSymbolTable, newSymbolTable}
import org.scalatest.FunSuite

class SymbolTableTest extends FunSuite {

  test("should Define") {
    val expected = Map(
      "a" -> Symbol(name = "a", scope = GlobalScope, index = 0)
      , "b" -> Symbol(name = "b", scope = GlobalScope, index = 1)
      , "c" -> Symbol(name = "c", scope = LocalScope, index = 0)
      , "d" -> Symbol(name = "d", scope = LocalScope, index = 1)
      , "e" -> Symbol(name = "e", scope = LocalScope, index = 0)
      , "f" -> Symbol(name = "f", scope = LocalScope, index = 1)
    )

    val global = newSymbolTable()
    val a = global.define("a")
    assert(a == expected("a"))
    val b = global.define("b")
    assert(b == expected("b"))

    val firstLocal = newEnclosedSymbolTable(global)
    val c = firstLocal.define("c")
    assert(c == expected("c"))
    val d = firstLocal.define("d")
    assert(d == expected("d"))

    val secondLocal = newEnclosedSymbolTable(firstLocal)
    val e = secondLocal.define("e")
    assert(e == expected("e"))
    val f = secondLocal.define("f")
    assert(f == expected("f"))

  }

  test("should global resolve") {
    val global = newSymbolTable()

    global.define("a")
    global.define("b")

    val expected = List(
      Symbol(name = "a", scope = GlobalScope, index = 0),
      Symbol(name = "b", scope = GlobalScope, index = 1)
    )

    for (sym <- expected) {
      val (result, ok) = global.resolve(sym.name)
      assert(ok)
      assert(result == sym)
    }
  }

  test("should local resolve") {
    val global = newSymbolTable()

    global.define("a")
    global.define("b")

    val local = newEnclosedSymbolTable(global)

    local.define("c")
    local.define("d")

    val expected = List(
      Symbol(name = "a", scope = GlobalScope, index = 0),
      Symbol(name = "b", scope = GlobalScope, index = 1),
      Symbol(name = "c", scope = LocalScope, index = 0),
      Symbol(name = "d", scope = LocalScope, index = 1)
    )

    for (sym <- expected) {
      val (result, ok) = local.resolve(sym.name)
      assert(ok)
      assert(result == sym)
    }
  }

  test("should define resolve builtin") {

    val global = newSymbolTable()
    val firstLocal = newEnclosedSymbolTable(global)
    val secondLocal = newEnclosedSymbolTable(firstLocal)

    val expected = List(
      Symbol(name = "a", scope = BuiltinScope, index = 0)
      , Symbol(name = "c", scope = BuiltinScope, index = 1)
      , Symbol(name = "e", scope = BuiltinScope, index = 2)
      , Symbol(name = "f", scope = BuiltinScope, index = 3)
    )

    var i = 0
    for (sym <- expected) {
      global.defineBuiltin(i, sym.name)
      i += 1
    }

    for (table <- List(global, firstLocal, secondLocal)) {
      for (sym <- expected) {
        val (result, ok) = table.resolve(sym.name)
        assert(ok)
        assert(result == sym)
      }
    }
  }

  test("should resolve free") {

    val global = newSymbolTable()

    global.define("a")
    global.define("b")

    val firstLocal = newEnclosedSymbolTable(global)

    firstLocal.define("c")
    firstLocal.define("d")

    val secondLocal = newEnclosedSymbolTable(firstLocal)

    secondLocal.define("e")
    secondLocal.define("f")

    case class TestInput(table: SymbolTable, symbols: List[Symbol], free: List[Symbol])

    val tests = List(
      TestInput(
        firstLocal,
        List(
          Symbol(name = "a", scope = GlobalScope, index = 0),
          Symbol(name = "b", scope = GlobalScope, index = 1),
          Symbol(name = "c", scope = LocalScope, index = 0),
          Symbol(name = "d", scope = LocalScope, index = 1)
        ),
        List()
      ),
      TestInput(
        secondLocal,
        List(
          Symbol(name = "a", scope = GlobalScope, index = 0),
          Symbol(name = "b", scope = GlobalScope, index = 1),
          Symbol(name = "c", scope = FreeScope, index = 0),
          Symbol(name = "d", scope = FreeScope, index = 1),
          Symbol(name = "e", scope = LocalScope, index = 0),
          Symbol(name = "f", scope = LocalScope, index = 1)
        ),
        List(
          Symbol(name = "c", scope = LocalScope, index = 0),
          Symbol(name = "d", scope = LocalScope, index = 1)
        ),
      )
    )

    for (tt <- tests) {
      for (sym <- tt.symbols) {
        val (result, ok) = tt.table.resolve(sym.name)
        assert(ok)
        assert(result == sym)
      }
      assert(tt.table.freeSymbols.toList == tt.free)
      var i = 0
      for (sym <- tt.free) {
        val result = tt.table.freeSymbols(i)
        assert(result == sym)
        i += 1
      }
    }
  }

  test("should resolve unresolvable free") {

    val global = newSymbolTable()
    global.define("a")

    val firstLocal = newEnclosedSymbolTable(global)
    firstLocal.define("c")

    val secondLocal = newEnclosedSymbolTable(firstLocal)
    secondLocal.define("e")
    secondLocal.define("f")

    val expected = List(
      Symbol(name = "a", scope = GlobalScope, index = 0),
      Symbol(name = "c", scope = FreeScope, index = 0),
      Symbol(name = "e", scope = LocalScope, index = 0),
      Symbol(name = "f", scope = LocalScope, index = 1)
    )

    for (sym <- expected) {
      val (result, ok) = secondLocal.resolve(sym.name)
      assert(ok)
      assert(result == sym)
    }

    val expectedUnresolvable = List(
      "b",
      "d"
    )

    for (name <- expectedUnresolvable) {
      val (result, ok) = secondLocal.resolve(name)
      assert(!ok)
    }
  }

}
