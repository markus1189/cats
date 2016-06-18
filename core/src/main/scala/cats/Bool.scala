package cats

import simulacrum.typeclass

@typeclass trait Bool[A] { self =>
  def fromBoolean: Boolean => A

  @op("!", alias=true)
  def not: A => A

  @op("&&", alias=true)
  def and(lhs: A,rhs: A): A

  @op("||", alias=true)
  def or(lhs: A,rhs: A): A

  // Derived operations, overwrite for efficiency

  def zero: A = fromBoolean(false)

  def one: A = fromBoolean(true)

  def xor(lhs: A, rhs: A): A =
    or(and(lhs, not(rhs)),and(not(lhs),rhs))

  @op("==>", alias=true)
  def implies(lhs: A,rhs: A): A =
    and(not(lhs),rhs)

  @op("<=>", alias=true)
  def equivalent(lhs: A,rhs: A): A = not(xor(lhs,rhs))

  def nand(lhs: A,rhs: A): A = not(and(lhs,rhs))

  def nor(lhs: A,rhs: A): A = not(or(lhs,rhs))
}
