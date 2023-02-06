package com.xebia.functional
package scalacheck.trace

import org.scalacheck.Prop

package object formula {

  private[formula] type FormulaStepResult = Prop.Result
  private[formula] type Result[A] = Either[Throwable, A]

  private[formula] val everythingOk: FormulaStepResult = Prop.Result(Prop.True)
  private[formula] def problem(message: String): FormulaStepResult = Prop.Result(Prop.False).label(message)
  private[formula] def error(message: String, t: Throwable): FormulaStepResult = Prop.Result(Prop.Exception(t)).label(message)

}
