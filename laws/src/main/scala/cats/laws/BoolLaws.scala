package cats
package laws

import cats.syntax.bool._

/**
 * Laws that must be obeyed by any `cats.Bool`.
 */
trait BoolLaws[A] {
  implicit def B: Bool[A]

  def boolAndAssociative(x: A, y: A, z: A): IsEq[A] = ((x && y) && z) <-> (x && (y && z))

  def boolOrAssociative(x: A, y: A, z: A): IsEq[A] = ((x || y) || z) <-> (x || (y || z))

  def boolAndCommutative(x: A, y: A): IsEq[A] = (x && y) <-> (y && x)

  def boolOrCommutative(x: A, y: A): IsEq[A] = (x || y) <-> (y || x)

  def boolDistributive1(x: A, y: A, z: A): IsEq[A] =
    (x && (y || z)) <-> ((x && y) || (x && z))

  def boolDistributive2(x: A, y: A, z: A): IsEq[A] =
    (x || (y && z)) <-> ((x || y) && (x || z))

  def boolOrIdentity(x: A): IsEq[A] = (x || B.zero) <-> x

  def boolAndIdentity(x: A): IsEq[A] = (x && B.one) <-> x

  def boolAndZero(x: A): IsEq[A] = (x && B.zero) <-> B.zero

  def boolOrOne(x: A): IsEq[A] = (x || B.one) <-> B.one

  def boolAndIdempotent(x: A): IsEq[A] = (x && x) <-> x

  def boolOrIdempotent(x: A): IsEq[A] = (x || x) <-> x

  def boolAndAbsorb(x: A, y: A): IsEq[A] = (x && (x || y)) <-> x

  def boolOrAbsorb(x: A, y: A): IsEq[A] = (x || (x && y)) <-> x

  def boolComplementAnd(x: A): IsEq[A] = (x && B.not(x)) <-> B.zero

  def boolComplementOr(x: A): IsEq[A] = (x || B.not(x)) <-> B.one

  def boolDoubleNeg(x: A): IsEq[A] = B.not(B.not(x)) <-> x

  def boolDeMorgan1(x: A, y: A): IsEq[A] = (B.not(x) && B.not(y)) <-> B.not(x || y)

  def boolDeMorgan2(x: A, y: A): IsEq[A] = (B.not(x) || B.not(y)) <-> B.not(x && y)
}
