package org.qadusch.example
package TrySecondFinal

import model.OutputError

import cats.data.{Ior, IorNel, NonEmptyList}

object IorNelBothOpsFinal {
  implicit class IorNelOutputErrorBothOpsFinal[A](val ior: IorNel[OutputError, A]) extends AnyVal {

    def dropLeftOnCondition(condition: OnOutputErrorCondition): IorNel[OutputError, A] = dropLeftOnCondition(condition.toDropCondition[A])

    def dropLeftOnCondition(condition: OnValueCondition[A]): IorNel[OutputError, A] = dropLeftOnCondition(condition.toDropCondition)

    def dropLeftOnCondition(condition: CombinedConditions[A]): IorNel[OutputError, A] = dropLeftOnConditionList(List(condition))

    def dropLeftOnConditionList(conditions: List[CombinedConditions[A]]): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if conditions.forall(_.shouldDrop(left, right)) =>
        Ior.Right(right)
      case _ => ior
    }

    def dropRightOnCondition(condition: OnOutputErrorCondition): IorNel[OutputError, A] = dropRightOnCondition(condition.toDropCondition[A])

    def dropRightOnCondition(condition: OnValueCondition[A]): IorNel[OutputError, A] = dropRightOnCondition(condition.toDropCondition)

    def dropRightOnCondition(condition: CombinedConditions[A]): IorNel[OutputError, A] = dropRightOnConditionList(List(condition))

    def dropRightOnConditionList(conditions: List[CombinedConditions[A]]): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if conditions.forall(_.shouldDrop(left, right)) =>
        Ior.Left(left)
      case _ => ior
    }
  }

}
