package org.qadusch.example
package model

sealed abstract class OutputErrorLevel(val name: String, val order: Int)

case object Info extends OutputErrorLevel("info", 3)

case object Warning extends OutputErrorLevel("warning", 2)

case object Error extends OutputErrorLevel("error", 1)
