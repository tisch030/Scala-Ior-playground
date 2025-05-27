package org.qadusch.example
package TrySecondFinal
import model._

import cats.data.NonEmptyList

trait OnOutputErrorCondition {
  def shouldDrop(errors: NonEmptyList[OutputError]): Boolean

  def negate: OnOutputErrorCondition = (errors: NonEmptyList[OutputError]) => !OnOutputErrorCondition.this.shouldDrop(errors)

  def toDropCondition[A]: CombinedConditions[A] = CombinedConditions[A](this)
}

class PredicateOutputErrorCondition(predicate: NonEmptyList[OutputError] => Boolean) extends OnOutputErrorCondition {
  override def shouldDrop(errors: NonEmptyList[OutputError]): Boolean = predicate(errors)
}

sealed abstract class ConditionOnField[A](
    extractField: OutputError => A,
    reference: A,
    compareFn: (A, A) => Boolean,
    matchFn: List[Boolean] => Boolean,
    excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
) extends PredicateOutputErrorCondition(errors => {
      val filtered = errors.toList.filterNot(e => excludeErrorWithLevel.contains(e.level))
      val results = filtered.map { e =>
        val fieldValue = extractField(e)
        compareFn(fieldValue, reference)
      }
      matchFn(results)
    })

case class FirstMatchLevelLowerThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
    extends ConditionOnField(
      _.level,
      threshold,
      (a: OutputErrorLevel, b: OutputErrorLevel) => a.order < b.order,
      _.exists(identity),
      excludeErrorWithLevel
    )

case class FirstMatchLevelHigherThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
    extends ConditionOnField(
      _.level,
      threshold,
      (a: OutputErrorLevel, b: OutputErrorLevel) => a.order > b.order,
      _.exists(identity),
      excludeErrorWithLevel
    )

case class AllMatchLevelLowerThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
    extends ConditionOnField(
      _.level,
      threshold,
      (a: OutputErrorLevel, b: OutputErrorLevel) => a.order < b.order,
      _.forall(identity),
      excludeErrorWithLevel
    )

case class AllMatchLevelHigherThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
    extends ConditionOnField(
      _.level,
      threshold,
      (a: OutputErrorLevel, b: OutputErrorLevel) => a.order > b.order,
      _.forall(identity),
      excludeErrorWithLevel
    )

case class FirstMatchNameEquals(expectedName: String, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
    extends ConditionOnField(
      _.message,
      expectedName,
      (a: String, b: String) => a == b,
      _.exists(identity),
      excludeErrorWithLevel
    )

case class AllMatchNameEquals(expectedName: String, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
    extends ConditionOnField(
      _.message,
      expectedName,
      (a: String, b: String) => a == b,
      _.forall(identity),
      excludeErrorWithLevel
    )
