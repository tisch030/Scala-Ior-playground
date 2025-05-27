package org.qadusch.example
package TrySimpleFunctions
import model.{InfraObject, OutputError, OutputErrorLevel}

import cats.data.{Ior, IorNel, NonEmptyList}

object IorNelOpsSimpleFunction {
  implicit class IorOutputErrorSimpleFunctionOps[A](val ior: IorNel[OutputError, A]) extends AnyVal {

    def dropRightOnLeftCondition(
        condition: NonEmptyList[OutputError] => Boolean
    ): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], _) if condition(left) =>
        Ior.Left(left)
      case _ => ior
    }

    def dropLeftOnRightCondition(
        condition: A => Boolean
    ): IorNel[OutputError, A] = ior match {
      case Ior.Both(_, right: A) if condition(right) => Ior.Right(right)
      case _                                         => ior
    }
  }

}
object DropConditions {

  object ConditionOnOutputError {

    private def conditionOnField[A](
        extractField: OutputError => A,
        reference: A,
        compareFn: (A, A) => Boolean,
        matchFn: List[Boolean] => Boolean,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean = errors => {
      val filtered =
        errors.toList.filterNot(e => excludeErrorWithLevel.contains(e.level))
      val results = filtered.map { e =>
        val fieldValue = extractField(e)
        compareFn(fieldValue, reference)
      }
      matchFn(results)
    }

    def firstMatchLevelLowerThan(
        threshold: OutputErrorLevel,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean =
      conditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order < b.order,
        _.exists(identity),
        excludeErrorWithLevel
      )

    def firstMatchLevelHigherThan(
        threshold: OutputErrorLevel,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean =
      conditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order > b.order,
        _.exists(identity),
        excludeErrorWithLevel
      )

    // Examples for level-based matching

    def allMatchLevelLowerThan(
        threshold: OutputErrorLevel,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean =
      conditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order < b.order,
        _.forall(identity),
        excludeErrorWithLevel
      )

    def allMatchLevelHigherThan(
        threshold: OutputErrorLevel,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean =
      conditionOnField(
        _.level,
        threshold,
        (a: OutputErrorLevel, b: OutputErrorLevel) => a.order > b.order,
        _.forall(identity),
        excludeErrorWithLevel
      )

    // Examples for message-based matching

    def firstMatchNameEquals(
        expectedName: String,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean =
      conditionOnField(
        _.message,
        expectedName,
        (a: String, b: String) => a == b,
        _.exists(identity),
        excludeErrorWithLevel
      )

    def allMatchNameEquals(
        expectedName: String,
        excludeErrorWithLevel: Set[OutputErrorLevel] = Set.empty
    ): NonEmptyList[OutputError] => Boolean =
      conditionOnField(
        _.message,
        expectedName,
        (a: String, b: String) => a == b,
        _.forall(identity),
        excludeErrorWithLevel
      )

  }

  object ConditionOnValue {

    def emptyString: String => Boolean = _.isEmpty

    def existsInfraObjects(name: String): List[InfraObject] => Boolean =
      _.exists(_.name.equals(name))

  }

}
