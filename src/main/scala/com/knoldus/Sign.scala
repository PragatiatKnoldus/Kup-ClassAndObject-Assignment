package com.knoldus

trait Sign {
  def isPositive: Boolean
  def negate: Sign
}