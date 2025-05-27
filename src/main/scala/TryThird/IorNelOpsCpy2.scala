package org.qadusch.example
package TryThird

import model._

import cats.data.{Ior, IorNel, NonEmptyList}

sealed trait Side
object Side {
  case object Left extends Side
  case object Right extends Side
  case object Both extends Side
}

object IorNelOpsCpy2 {
  implicit class IorOutputErrorOpsCpy2[A](val ior: IorNel[OutputError, A]) extends AnyVal {

    def dropSideOnErrorCondition(
        sideToDrop: Side,
        condition: OnOutputErrorCondition
    ): Option[IorNel[OutputError, A]] = ior match {
      case Ior.Right(right) => Some(ior)
      case Ior.Left(left: NonEmptyList[OutputError]) =>
        sideToDrop match {
          case Side.Left if condition.shouldDrop(left) => None
          case _                                       => Some(ior)
        }
      case Ior.Both(left: NonEmptyList[OutputError], right: A) =>
        if (condition.shouldDrop(left)) {
          sideToDrop match {
            case Side.Left  => Some(Ior.Right(right))
            case Side.Right => Some(Ior.Left(left))
            case Side.Both  => None
          }
        } else {
          Some(ior)
        }
    }

    def dropLeftOnLeftConditionOptional(
        condition: OnOutputErrorCondition
    ): Option[IorNel[OutputError, A]] = ior match {
      case Ior.Left(left: NonEmptyList[OutputError]) if condition.shouldDrop(left) => None
      case Ior.Right(right)                                                        => Some(ior)
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if condition.shouldDrop(left) =>
        Some(Ior.Right(right))
    }

    def dropLeftOnLeftCondition(
        condition: OnOutputErrorCondition
    ): IorNel[OutputError, A] = ior match {
      case Ior.Left(left: NonEmptyList[OutputError]) if condition.shouldDrop(left) =>
        Ior.Left(NonEmptyList.one(OutputError("Errors explicitly dropped due to condition {conditionInfo}", Info)))
      case Ior.Right(right) => ior
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if condition.shouldDrop(left) =>
        Ior.Right(right)
    }

    def dropRightOnRightCondition(
        condition: OnValueCondition[A]
    ): IorNel[OutputError, A] = ior match {
      case Ior.Left(left) => ior
      case Ior.Right(right: A) if condition.shouldDrop(right) =>
        Ior.Left(NonEmptyList.one(OutputError("Values explicitly dropped due to condition {conditionInfo}", Info)))
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if condition.shouldDrop(right) => Ior.Left(left)
    }

    def dropRightOnLeftCondition(condition: OnOutputErrorCondition): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], _) if condition.shouldDrop(left) => Ior.Left(left)
      case _                                                                          => ior
    }

    def dropLeftOnRightCondition(condition: OnValueCondition[A]): IorNel[OutputError, A] = ior match {
      case Ior.Both(_, right: A) if condition.shouldDrop(right) => Ior.Right(right)
      case _                                                    => ior
    }
  }

}

trait OnOutputErrorCondition {
  def shouldDrop(errors: NonEmptyList[OutputError]): Boolean
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

object ErrorLevelConditions2 {
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

}

object ErrorMessageConditions2 {
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
}

trait OnValueCondition[A] {
  def shouldDrop(value: A): Boolean
}

class PredicateValueCondition[A](predicate: A => Boolean) extends OnValueCondition[A] {
  def shouldDrop(value: A): Boolean = predicate(value)
}

case class IfEmptyString() extends PredicateValueCondition[String](_.isEmpty)

case class ExistsInfraObject(name: String) extends PredicateValueCondition[List[InfraObject]](_.exists(_.name.equals(name)))
