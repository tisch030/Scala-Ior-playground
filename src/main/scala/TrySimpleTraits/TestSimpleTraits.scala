package org.qadusch.example

import ErrorLevelConditions._
import ErrorMessageConditions._
import IorNelOpsCpy.IorOutputErrorOpsCpy
import TrySecond.{ExistsInfraObject, IfEmptyString, PredicateOutputErrorCondition, PredicateValueCondition}

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

  // Drop left if right value is empty string

  val noRightsOnlyErrors = result1.dropRightOnLeftCondition(FirstMatchLevelLowerThan(Warning)) // no rights
  val both = result1.dropRightOnLeftCondition(FirstMatchLevelLowerThan(Error))

  val noLeftEmptyRight = result2.dropLeftOnRightCondition(IfEmptyString())

  val noRightAllErrorsName = result1.dropRightOnLeftCondition(FirstMatchNameEquals("Missing Noob"))
  val both2 = result1.dropRightOnLeftCondition(AllMatchNameEquals("Missing Noob"))

  val errorsCondition = PredicateOutputErrorCondition(_.exists(_.level == Error))
  val valueCondition = PredicateValueCondition[String](_.isEmpty)

  val onlyInfras = result3.dropLeftOnRightCondition(ExistsInfraObject("Rail"))

}
