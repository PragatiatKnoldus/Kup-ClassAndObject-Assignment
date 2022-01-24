package com.knoldus

import scala.sys.error

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = error("negative number")

  def successor: Nat = new Succ(Zero)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) Zero else error("negative number")
}