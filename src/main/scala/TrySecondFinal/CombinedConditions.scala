package org.qadusch.example
package TrySecondFinal
import model.OutputError

import cats.data.NonEmptyList

sealed abstract class CombinationMode(val name: String)
case object All extends CombinationMode("all")
case object AnyCombination extends CombinationMode("any")
case object None extends CombinationMode("none")

case class CombinedConditions[A](
    errorConditions: List[OnOutputErrorCondition] = List.empty,
    valueConditions: List[OnValueCondition[A]] = List.empty,
    nestedConditions: List[CombinedConditions[A]] = List.empty,
    combinationMode: CombinationMode = All
) {
  def shouldDrop(errors: NonEmptyList[OutputError], value: A): Boolean = combinationMode match {
    case All =>
      errorConditions.forall(_.shouldDrop(errors)) && valueConditions.forall(_.shouldDrop(value)) &&
      nestedConditions.forall(_.shouldDrop(errors, value))
    case AnyCombination =>
      errorConditions.exists(_.shouldDrop(errors)) || valueConditions.exists(_.shouldDrop(value)) ||
      nestedConditions.exists(_.shouldDrop(errors, value))
    case None =>
      !errorConditions.exists(_.shouldDrop(errors)) && !valueConditions.exists(_.shouldDrop(value)) &&
      !nestedConditions.exists(_.shouldDrop(errors, value))
    case _ => false
  }

  def shouldDropOnErrors(errors: NonEmptyList[OutputError]): Boolean = combinationMode match {
    case All =>
      errorConditions.forall(_.shouldDrop(errors)) &&
      nestedConditions.forall(_.shouldDropOnErrors(errors))
    case AnyCombination =>
      errorConditions.exists(_.shouldDrop(errors)) ||
      nestedConditions.exists(_.shouldDropOnErrors(errors))
    case None =>
      !errorConditions.exists(_.shouldDrop(errors)) &&
      !nestedConditions.exists(_.shouldDropOnErrors(errors))
    case _ => false
  }

  def shouldDropOnValue(value: A): Boolean = combinationMode match {
    case All =>
      valueConditions.forall(_.shouldDrop(value)) &&
      nestedConditions.forall(_.shouldDropOnValue(value))
    case AnyCombination =>
      valueConditions.exists(_.shouldDrop(value)) ||
      nestedConditions.exists(_.shouldDropOnValue(value))
    case None =>
      !valueConditions.exists(_.shouldDrop(value)) &&
      !nestedConditions.exists(_.shouldDropOnValue(value))
    case _ => false
  }

}

case class CombinedConditionsBuilder[A](
    errorConditions: List[OnOutputErrorCondition] = List.empty,
    valueConditions: List[OnValueCondition[A]] = List.empty,
    nestedConditions: List[CombinedConditions[A]] = List.empty,
    mode: CombinationMode = All
) {

  def withErrorCondition(condition: OnOutputErrorCondition): CombinedConditionsBuilder[A] =
    copy(errorConditions = errorConditions :+ condition)

  def withErrorConditions(conditions: List[OnOutputErrorCondition]): CombinedConditionsBuilder[A] =
    copy(errorConditions = errorConditions ++ conditions)

  def withValueCondition(condition: OnValueCondition[A]): CombinedConditionsBuilder[A] =
    copy(valueConditions = valueConditions :+ condition)

  def withValueConditions(conditions: List[OnValueCondition[A]]): CombinedConditionsBuilder[A] =
    copy(valueConditions = valueConditions ++ conditions)

  def withNestedCondition(condition: CombinedConditions[A]): CombinedConditionsBuilder[A] =
    copy(nestedConditions = nestedConditions :+ condition)

  def withCombinationMode(mode: CombinationMode): CombinedConditionsBuilder[A] =
    copy(mode = mode)

  def build: CombinedConditions[A] =
    CombinedConditions(errorConditions, valueConditions, nestedConditions, mode)
}

object CombinedConditions {

  def apply[A](
      errorCondition: OnOutputErrorCondition
  ): CombinedConditions[A] =
    CombinedConditions(List(errorCondition), Nil, Nil, All)

  def apply[A](
      valueCondition: OnValueCondition[A]
  ): CombinedConditions[A] =
    CombinedConditions(Nil, List(valueCondition), Nil, All)

  def apply[A](
      errorCondition: OnOutputErrorCondition,
      valueCondition: OnValueCondition[A],
      mode: CombinationMode
  ): CombinedConditions[A] =
    CombinedConditions(List(errorCondition), List(valueCondition), Nil, mode)

  def apply[A](
      errors: List[OnOutputErrorCondition],
      values: List[OnValueCondition[A]],
      mode: CombinationMode
  ): CombinedConditions[A] =
    CombinedConditions(errors, values, Nil, mode)

  def apply[A](
      errors: List[OnOutputErrorCondition] = Nil,
      values: List[OnValueCondition[A]] = Nil,
      nested: List[CombinedConditions[A]] = Nil,
      mode: CombinationMode = All
  ): CombinedConditions[A] =
    new CombinedConditions(errors, values, nested, mode)

  def dsl[A]: CombinedConditionsBuilder[A] = CombinedConditionsBuilder[A]()
}
