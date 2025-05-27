package org.qadusch.example
package TrySecondFinal
import model.InfraObject

trait OnValueCondition[A] {
  def shouldDrop(value: A): Boolean

  def negate: OnValueCondition[A] = (value: A) => !OnValueCondition.this.shouldDrop(value)

  def toDropCondition: CombinedConditions[A] = CombinedConditions[A](this)
}

sealed class PredicateValueCondition[A](predicate: A => Boolean) extends OnValueCondition[A] {
  def shouldDrop(value: A): Boolean = predicate(value)
}

case class IfEmptyString() extends PredicateValueCondition[String](_.isEmpty)

case class ExistsInfraObject(name: String) extends PredicateValueCondition[List[InfraObject]](_.exists(_.name.equals(name)))
