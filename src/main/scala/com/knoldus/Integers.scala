package com.knoldus

case class Integers(value: Nat, sign: Sign = Positive) extends Nat with Sign {
  def isZero: Boolean = value.isZero

  def predecessor: Nat =
    if (isZero) Integers(value.successor, Negative)
    else if (sign.isPositive) Integers(value.predecessor, sign)
    else Integers(value.successor, Negative)

  def successor: Nat =
    if (isZero) Integers(value.successor, Positive)
    else if (sign.isPositive) Integers(value.successor, sign)
    else Integers(value.predecessor, Negative)

  def +(that: Nat): Nat =
    if (isZero) that
    else if (sign.isPositive) this.predecessor + that.successor
    else this.successor + that.predecessor

  def -(that: Nat): Nat =
    if (that.isZero) this
    else that match {
      case Integers(v, s) => this + Integers(v, s.negate)
    }

  def isPositive: Boolean = sign.isPositive

  def negate: Integers = Integers(value, sign.negate)
}