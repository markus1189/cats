package cats

package object syntax {
  object all extends AllSyntax
  object applicative extends ApplicativeSyntax
  object applicativeError extends ApplicativeErrorSyntax
  object apply extends ApplySyntax
  object bifunctor extends BifunctorSyntax
  object bifoldable extends BifoldableSyntax
  object bitraverse extends BitraverseSyntax
  object cartesian extends CartesianSyntax
  object coflatMap extends CoflatMapSyntax
  object coproduct extends CoproductSyntax
  object comonad extends ComonadSyntax
  object compose extends ComposeSyntax
  object contravariant extends ContravariantSyntax
  object either extends EitherSyntax
  object eq extends EqSyntax
  object flatMap extends FlatMapSyntax
  object foldable extends FoldableSyntax
  object functor extends FunctorSyntax
  object functorFilter extends FunctorFilterSyntax
  object group extends GroupSyntax
  object invariant extends InvariantSyntax
  object list extends ListSyntax
  object monadCombine extends MonadCombineSyntax
  object monadFilter extends MonadFilterSyntax
  object monoid extends MonoidSyntax
  object option extends OptionSyntax
  object order extends OrderSyntax
  object partialOrder extends PartialOrderSyntax
  object profunctor extends ProfunctorSyntax
  object reducible extends ReducibleSyntax
  object semigroup extends SemigroupSyntax
  object semigroupk extends SemigroupKSyntax
  object show extends Show.ToShowOps
  object split extends SplitSyntax
  object strong extends StrongSyntax
  object transLift extends TransLiftSyntax
  object traverse extends TraverseSyntax
  object traverseFilter extends TraverseFilterSyntax
  object tuple extends TupleSyntax
  object validated extends ValidatedSyntax
  object writer extends WriterSyntax
}
