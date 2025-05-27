package org.qadusch.example
package TrySecondFinal
import model.OutputError

import cats.data.NonEmptyList

sealed abstract class CombinationMode(val name: String)
case object All extends CombinationMode("all")
case object AnyCombination extends CombinationMode("any")
case object None extends CombinationMode("none")

case class OnErrorWithValueConditions[A](
    errorConditions: List[OnOutputErrorCondition] = List.empty,
    valueConditions: List[OnValueCondition[A]] = List.empty,
    combinationMode: CombinationMode = All
) {
  def shouldDrop(errors: NonEmptyList[OutputError], value: A): Boolean = combinationMode match {
    case All =>
      errorConditions.forall(_.shouldDrop(errors)) && valueConditions.forall(_.shouldDrop(value))
    case AnyCombination =>
      errorConditions.exists(_.shouldDrop(errors)) || valueConditions.exists(_.shouldDrop(value))
    case None =>
      !errorConditions.exists(_.shouldDrop(errors)) && !valueConditions.exists(_.shouldDrop(value))
    case _ => false
  }

  def shouldDropOnErrors(errors: NonEmptyList[OutputError]): Boolean = combinationMode match {
    case All            => errorConditions.forall(_.shouldDrop(errors))
    case AnyCombination => errorConditions.exists(_.shouldDrop(errors))
    case None           => !errorConditions.exists(_.shouldDrop(errors))
    case _              => false
  }
}

case class OnErrorWithValueConditionsBuilder[A](
    errorConditions: List[OnOutputErrorCondition] = List.empty,
    valueConditions: List[OnValueCondition[A]] = List.empty,
    mode: CombinationMode = All
) {

  def withErrorCondition(condition: OnOutputErrorCondition): OnErrorWithValueConditionsBuilder[A] =
    copy(errorConditions = errorConditions :+ condition)

  def withErrorConditions(conditions: List[OnOutputErrorCondition]): OnErrorWithValueConditionsBuilder[A] =
    copy(errorConditions = errorConditions ++ conditions)

  def withValueCondition(condition: OnValueCondition[A]): OnErrorWithValueConditionsBuilder[A] =
    copy(valueConditions = valueConditions :+ condition)

  def withValueConditions(conditions: List[OnValueCondition[A]]): OnErrorWithValueConditionsBuilder[A] =
    copy(valueConditions = valueConditions ++ conditions)

  def withCombinationMode(mode: CombinationMode): OnErrorWithValueConditionsBuilder[A] =
    copy(mode = mode)

  def build: OnErrorWithValueConditions[A] =
    OnErrorWithValueConditions(errorConditions, valueConditions, mode)
}

object OnErrorWithValueConditions {

  def apply[A](
      errorCondition: OnOutputErrorCondition
  ): OnErrorWithValueConditions[A] =
    OnErrorWithValueConditions(List(errorCondition), Nil, All)

  def apply[A](
      valueCondition: OnValueCondition[A]
  ): OnErrorWithValueConditions[A] =
    OnErrorWithValueConditions(Nil, List(valueCondition), All)

  def apply[A](
      errorCondition: OnOutputErrorCondition,
      valueCondition: OnValueCondition[A],
      mode: CombinationMode
  ): OnErrorWithValueConditions[A] =
    OnErrorWithValueConditions(List(errorCondition), List(valueCondition), mode)

  def apply[A](
      errorConditions: List[OnOutputErrorCondition] = List.empty,
      valueConditions: List[OnValueCondition[A]] = List.empty,
      mode: CombinationMode = All
  ): OnErrorWithValueConditions[A] =
    new OnErrorWithValueConditions(errorConditions, valueConditions, mode)

  def dsl[A]: OnErrorWithValueConditionsBuilder[A] = OnErrorWithValueConditionsBuilder[A]()
}
