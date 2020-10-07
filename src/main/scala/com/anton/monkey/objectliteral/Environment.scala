package com.anton.monkey.objectliteral

import scala.collection.mutable

case class Environment(store: mutable.Map[String, ObjectLiteral],
                       outer: Environment) {

  def get(name: String): (ObjectLiteral, Boolean) = {
    store.get(name) match {
      case Some(value) => return (value, true)
    }
    if (outer != null) {
      return outer.get(name)
    }
    (null, false)
  }

  def set(name: String, value: ObjectLiteral): ObjectLiteral = {
    store(name) = value
    value
  }
}

object Environment {
  def NewEnvironment(): Environment = {
    val store: mutable.Map[String, ObjectLiteral] = mutable.Map()
    Environment(store, null)
  }

  def NewEnclosedEnvironment(outer: Environment): Environment = {
    val store: mutable.Map[String, ObjectLiteral] = mutable.Map()
    Environment(store, outer)
  }
}