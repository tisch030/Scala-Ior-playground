package org.qadusch.example
package TrySecondFinal

import TrySecondFinal.IorNelBothOpsFinal._
import model.{OutputError, _}

import cats.data.{Ior, IorNel, NonEmptyList}

object TestIorFinal extends App {
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

  val firstErrorLevelLowerThanWarning = CombinedConditions
    .dsl[String]
    .withErrorCondition(FirstMatchLevelLowerThan(Warning))
    .withErrorConditions(List(FirstMatchLevelLowerThan(Warning)))
    .withCombinationMode(AnyCombination)
    .build

  val noRightsOnlyErrors = result1.dropRightOnCondition(firstErrorLevelLowerThanWarning) // no rights
  val both = result1.dropRightOnCondition(CombinedConditions[String](List(FirstMatchLevelLowerThan(Error))))

  val noLeftEmptyRight = result2.dropLeftOnCondition(IfEmptyString())

  val noRightAllErrorsName = result1.dropRightOnCondition(FirstMatchNameEquals("Missing Noob").toDropCondition[String])
  val both2 = result1.dropRightOnCondition(AllMatchNameEquals("Missing Noob"))

  val emptyStringInline: String => Boolean = _.isEmpty
  val emptyStringsFoundResult = result1.dropLeftOnCondition(new PredicateValueCondition(emptyStringInline))

  val onlyInfras = result3.dropLeftOnCondition(ExistsInfraObject("Rail").toDropCondition)

  val dummyConditions = CombinedConditions
    .dsl[List[InfraObject]]
    .build

  result3.dropLeftOnCondition(dummyConditions)
  result3.dropLeftOnCondition(FirstMatchLevelLowerThan(Warning))
  result3.dropLeftOnCondition(FirstMatchLevelLowerThan(Warning).toDropCondition[List[InfraObject]])
  result3.dropLeftOnCondition(ExistsInfraObject("Rail"))
  result3.dropLeftOnCondition(ExistsInfraObject("Rail").toDropCondition)

  result3.dropRightOnCondition(dummyConditions)
  result3.dropRightOnCondition(FirstMatchLevelLowerThan(Warning))
  result3.dropRightOnCondition(FirstMatchLevelLowerThan(Warning).toDropCondition[List[InfraObject]])
  result3.dropRightOnCondition(ExistsInfraObject("Rail"))
  result3.dropRightOnCondition(ExistsInfraObject("Rail").toDropCondition)
  result3.dropRightOnCondition(CombinedConditions[List[InfraObject]](List(FirstMatchLevelLowerThan(Error))))
  result3.dropRightOnCondition(CombinedConditions[List[InfraObject]](FirstMatchLevelLowerThan(Error)))

  val test: IorNel[OutputError, List[InfraObject]] = result3 match {
    case Ior.Both(_, _) => result3.dropLeftOnCondition(FirstMatchLevelLowerThan(Warning))
    case Ior.Left(left: NonEmptyList[OutputError]) if dummyConditions.shouldDropOnErrors(left) => result3
    case Ior.Left(left: NonEmptyList[OutputError]) if FirstMatchLevelLowerThan(Warning).shouldDrop(left) =>
      Ior.Left(NonEmptyList.one(OutputError("Errors explicitly dropped due to condition {conditionInfo}", Info)))
    case Ior.Left(left: NonEmptyList[OutputError]) if FirstMatchLevelLowerThan(Warning).shouldDrop(left) => result3 // Could return Option instead
    case Ior.Right(right)                                                                                => result3
    case _                                                                                               => result3
  }

  val conditionErrorA = AllMatchNameEquals("a")
  val conditionErrorB = FirstMatchLevelLowerThan(Warning)
  val conditionErrorC = FirstMatchNameEquals("c")

  // Combined condition: (conditionErrorA && conditionErrorB) AND conditionErrorC
  val combinedCondition = CombinedConditions
    .dsl[String]
    .withNestedCondition(
      CombinedConditions
        .dsl[String]
        .withErrorConditions(List(conditionErrorA, conditionErrorB))
        .withCombinationMode(All)
        .build
    )
    .withNestedCondition(
      CombinedConditions
        .dsl[String]
        .withErrorCondition(conditionErrorC)
        .build
    )
    .withCombinationMode(AnyCombination) // ALL not ANY
    .build

  // CASE 1: (conditionErrorA && conditionErrorB) is false, conditionErrorC is false
  val errorsCase1 = NonEmptyList.of(
    OutputError("a", Info),
    OutputError("a", Info), // Matches A and B
    OutputError("a", Info)
  )
  val bothCase1 = Ior.Both(errorsCase1, "value1")
  val resultCase1 = bothCase1.dropRightOnCondition(combinedCondition)

  println("=== CASE 1: Left condition false, right condition false ===")
  println("Original Ior: " + bothCase1)
  println("After dropRightOnCondition: " + resultCase1)

  // CASE 2: (conditionErrorA && conditionErrorB) is true, conditionErrorC is false
  val errorsCase2 = NonEmptyList.of(
    OutputError("a", Warning),
    OutputError("a", Error)
  )
  val bothCase2 = Ior.Both(errorsCase2, "value2")
  val resultCase2 = bothCase2.dropRightOnCondition(combinedCondition)

  println("=== CASE 2: Left condition true, right condition false ===")
  println("Original Ior: " + bothCase2)
  println("After dropRightOnCondition: " + resultCase2)

  // CASE 2: (conditionErrorA && conditionErrorB) is true, conditionErrorC is false
  val errorsCase3 = NonEmptyList.of(
    OutputError("c", Warning),
    OutputError("c", Error),
    OutputError("c", Error)
  )
  val bothCase3 = Ior.Both(errorsCase3, "value3")
  val resultCase3 = bothCase3.dropRightOnCondition(combinedCondition)

  println("=== CASE 3: Left condition false, right condition true")
  println("Original Ior: " + bothCase3)
  println("After dropRightOnCondition: " + resultCase3)

}
