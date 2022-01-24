package com.knoldus

object Positive extends Sign {
  override def isPositive: Boolean = true

  override def negate: Sign = Negative
}