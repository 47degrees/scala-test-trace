package com.xebia.functional
package scalacheck.trace

package object formula {
  
  private[formula] type Problem = String
  private[formula] type FormulaStepResult = Option[List[Problem]]
  private[formula] type Result[A] = Either[Throwable, A]
  

  private[formula] val everythingOk: FormulaStepResult = None
  private[formula] def problem (message: String): Option[List[Problem]] = Some(List(message))

}
