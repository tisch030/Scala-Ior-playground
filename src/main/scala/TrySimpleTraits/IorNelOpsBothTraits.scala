package org.qadusch.example

import cats.data.{Ior, IorNel, NonEmptyList}

object IorNelOpsCpy {
  implicit class IorOutputErrorOpsCpy[A](val ior: IorNel[OutputError, A]) extends AnyVal {

    def dropRightOnLeftCondition(condition: TrySecond.OnOutputErrorCondition): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], _) if condition.shouldDrop(left) => Ior.Left(left)
      case _                                                                          => ior
    }

    def dropLeftOnRightCondition(condition: TrySecond.OnValueCondition[A]): IorNel[OutputError, A] = ior match {
      case Ior.Both(_, right: A) if condition.shouldDrop(right) => Ior.Right(right)
      case _                                                    => ior
    }
  }

}

trait OnOutputErrorCondition {
  def shouldDrop(errors: NonEmptyList[OutputError]): Boolean
}

case class PredicateOutputErrorCondition(predicate: NonEmptyList[OutputError] => Boolean) extends TrySecond.OnOutputErrorCondition {
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

object ErrorLevelConditions {
  case class FirstMatchLevelLowerThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
      extends TrySecond.ConditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order < b.order,
        _.exists(identity),
        excludeErrorWithLevel
      )

  case class FirstMatchLevelHigherThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
      extends TrySecond.ConditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order > b.order,
        _.exists(identity),
        excludeErrorWithLevel
      )

  case class AllMatchLevelLowerThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
      extends TrySecond.ConditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order < b.order,
        _.forall(identity),
        excludeErrorWithLevel
      )

  case class AllMatchLevelHigherThan(threshold: OutputErrorLevel, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
      extends TrySecond.ConditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order > b.order,
        _.forall(identity),
        excludeErrorWithLevel
      )

}

object ErrorMessageConditions {
  case class FirstMatchNameEquals(expectedName: String, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
      extends TrySecond.ConditionOnField(
        _.message,
        expectedName,
        (a: String, b: String) => a == b,
        _.exists(identity),
        excludeErrorWithLevel
      )

  case class AllMatchNameEquals(expectedName: String, excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty)
      extends TrySecond.ConditionOnField(
        _.message,
        expectedName,
        (a: String, b: String) => a == b,
        _.forall(identity),
        excludeErrorWithLevel
      )
}

trait OnValueCondition[A] {
  def shouldDrop(value: A): Boolean
}

case class PredicateValueCondition[A](predicate: A => Boolean) extends TrySecond.OnValueCondition[A] {
  def shouldDrop(value: A): Boolean = predicate(value)
}

case class IfEmptyString() extends TrySecond.PredicateValueCondition[String](_.isEmpty)

case class ExistsInfraObject(name: String) extends TrySecond.PredicateValueCondition[List[InfraObject]](_.exists(_.name.equals(name)))
