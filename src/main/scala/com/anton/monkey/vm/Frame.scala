package com.anton.monkey.vm

import com.anton.monkey.code.Instructions
import com.anton.monkey.objectliteral.Closure

case class Frame(cl: Closure, var ip: Int, basePointer: Int) {
  def instructions(): Instructions = {
    cl.fn.instructions
  }
}

object Frame {
  def newFrame(cl: Closure, basePointer: Int): Frame = {
    Frame(cl, -1, basePointer)
  }
}
