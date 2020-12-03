package com.anton.monkey.benchmark

import scala.concurrent.duration._

case class StopWatch() {
  private val startedAtNanos = System.nanoTime()

  def elapsed(): Duration = {
    (System.nanoTime() - startedAtNanos).nanos
  }
}
