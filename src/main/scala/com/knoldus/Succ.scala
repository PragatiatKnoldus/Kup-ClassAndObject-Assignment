package com.knoldus

class Succ(num: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = num

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = num + that.successor

  def -(that: Nat): Nat = if (that.isZero) this else num - that.predecessor
}