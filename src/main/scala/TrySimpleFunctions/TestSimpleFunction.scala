package org.qadusch.example
package TrySimpleFunctions

import TrySimpleFunctions.DropConditions._
import TrySimpleFunctions.IorNelOpsSimpleFunction._
import model._

import cats.data.{Ior, IorNel, NonEmptyList}

// === EXAMPLES ===

object TestSimpleFunction extends App {
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

  val noRightsOnlyErrors = result1.dropRightOnLeftCondition(DropConditions.ConditionOnOutputError.firstMatchLevelLowerThan(Warning)) // no rights
  val both = result1.dropRightOnLeftCondition(DropConditions.ConditionOnOutputError.firstMatchLevelLowerThan(Error))

  val noLeftEmptyRight = result2.dropLeftOnRightCondition(ConditionOnValue.emptyString)

  val noRightAllErrorsName = result1.dropRightOnLeftCondition(ConditionOnOutputError.firstMatchNameEquals("Missing Noob"))
  val both2 = result1.dropRightOnLeftCondition(ConditionOnOutputError.allMatchNameEquals("Missing Noob"))

  val infrasCondition: String => Boolean = _.isEmpty

  val onlyInfras = result3.dropLeftOnRightCondition(ConditionOnValue.existsInfraObjects("Rail"))

}
