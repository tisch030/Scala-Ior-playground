package org.qadusch.example
package TrySecondFinal

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

  val test: CombinedCondition[List[InfraObject]] = CombinedCondition
    .dsl[List[InfraObject]]
    .withErrorCondition(OnOutputErrorCondition.errorLevel(Warning))
    .withValueCondition(OnValueCondition.nonEmpty)
    .build

}
