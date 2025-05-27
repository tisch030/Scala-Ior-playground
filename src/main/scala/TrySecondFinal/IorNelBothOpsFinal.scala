package org.qadusch.example
package TrySecondFinal

import cats.data.{Ior, IorNel, NonEmptyList}

object IorNelOpsCpy2 {
  implicit class IorOutputErrorOpsCpy2[A](val ior: IorNel[OutputError, A]) extends AnyVal {

    def dropLeftOnConditions(conditions: CombinedCondition[A]): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if conditions.shouldDrop(left, right) =>
        Ior.Right(right)
      case _ => ior
    }

    def dropRightOnConditions(conditions: CombinedCondition[A]): IorNel[OutputError, A] = ior match {
      case Ior.Both(left: NonEmptyList[OutputError], right: A) if conditions.shouldDrop(left, right) =>
        Ior.Left(left)
      case _ => ior
    }
  }

}
