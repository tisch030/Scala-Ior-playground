package org.qadusch.example
package TryThird

import TryThird.ErrorLevelConditions2._
import TryThird.ErrorMessageConditions2._
import TryThird.IorNelOpsCpy2._
import model._

import cats.data.{Ior, IorNel, NonEmptyList}

// === EXAMPLES ===

object Hello3 extends App {
  val errors = NonEmptyList.of(
    OutputError("Missing field", Info),
    OutputError("Missing Noob", Info),
    OutputError("Deprecated field", Warning),
    OutputError("Critical error", Error),
    OutputError("Critical fatalito", Error)
  )

  val result1: IorNel[OutputError, String] = Ior.Both(errors, "valid")
  val result2: IorNel[OutputError, String] = Ior.Both(errors, "")
  val result3: IorNel[OutputError, List[InfraObject]] =
    Ior.Both(errors, List(InfraObject("Rail", isAttachment = true), InfraObject("Road", isAttachment = false)))

  val noRightsOnlyErrors = result1.dropRightOnLeftCondition(FirstMatchLevelLowerThan(Warning)) // no rights
  val both = result1.dropRightOnLeftCondition(FirstMatchLevelLowerThan(Error))

  val noLeftEmptyRight = result2.dropLeftOnRightCondition(TryThird.IfEmptyString())

  val noRightAllErrorsName = result1.dropRightOnLeftCondition(FirstMatchNameEquals("Missing Noob"))
  val both2 = result1.dropRightOnLeftCondition(AllMatchNameEquals("Missing Noob"))

  val errorsCondition = new PredicateOutputErrorCondition(_.exists(_.level == Error))
  val valueCondition = new PredicateValueCondition[String](_.isEmpty)

  val onlyInfras = result3.dropLeftOnRightCondition(TryThird.ExistsInfraObject("Rail"))

  val test: IorNel[OutputError, List[InfraObject]] = result3 match {
    case Ior.Both(left: NonEmptyList[OutputError], right) if FirstMatchLevelLowerThan(Warning).shouldDrop(left) =>
      result3.dropSideOnErrorCondition(Side.Left, FirstMatchLevelLowerThan(Warning)).get
    case Ior.Both(left, right) =>
      if (FirstMatchLevelLowerThan(Warning).shouldDrop(left) && TryThird.ExistsInfraObject("Rail").shouldDrop(right)) {
        Ior.Right(right)
      } else {
        result3
      }
    case Ior.Both(_, _) => result3.dropLeftOnLeftCondition(FirstMatchLevelLowerThan(Warning))
    case Ior.Both(_, _) => result3.dropLeftOnLeftConditionOptional(FirstMatchLevelLowerThan(Warning)).get
    case Ior.Left(left: NonEmptyList[OutputError]) if FirstMatchLevelLowerThan(Warning).shouldDrop(left) =>
      Ior.Left(NonEmptyList.one(OutputError("Errors explicitly dropped due to condition {conditionInfo}", Info)))
    case Ior.Left(left: NonEmptyList[OutputError]) if FirstMatchLevelLowerThan(Warning).shouldDrop(left) => result3
    case Ior.Right(right)                                                                                => result3
    case _                                                                                               => result3
  }

}
