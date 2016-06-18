package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait BoolTests[A] extends Laws {
  def laws: BoolLaws[A]

  def bool(implicit arbitrary: Arbitrary[A], eq: Eq[A]): RuleSet = {
    new DefaultRuleSet(
      name = "bool",
      parent = None,
      "bool 'and' associativity" -> forAll(laws.boolAndAssociative _),
      "bool 'or' associativity" -> forAll(laws.boolOrAssociative _),
      "bool 'and' commutativity" -> forAll(laws.boolAndCommutative _),
      "bool 'or' commutativity" -> forAll(laws.boolOrCommutative _),
      "bool 'and' 'or' distributivity" -> forAll(laws.boolDistributive1 _),
      "bool 'or' 'and' distributivity" -> forAll(laws.boolDistributive2 _),
      "bool 'or' identity" -> forAll(laws.boolOrIdentity _),
      "bool 'and' identity" -> forAll(laws.boolAndIdentity _),
      "bool 'and' annihilator" -> forAll(laws.boolAndZero _),
      "bool 'or' annihilator" -> forAll(laws.boolOrOne _),
      "bool 'and' idempotence" -> forAll(laws.boolAndIdempotent _),
      "bool 'or' idempotence" -> forAll(laws.boolOrIdempotent _),
      "bool 'and' absorption" -> forAll(laws.boolAndAbsorb _),
      "bool 'or' absorption" -> forAll(laws.boolOrAbsorb _),
      "bool 'and' complementation" -> forAll(laws.boolComplementAnd _),
      "bool 'or complementation" -> forAll(laws.boolComplementOr _),
      "bool double negation" -> forAll(laws.boolDoubleNeg _),
      "bool De Morgan 1" -> forAll(laws.boolDeMorgan1 _),
      "bool De Morgan 2" -> forAll(laws.boolDeMorgan2 _)
    )
  }
}
