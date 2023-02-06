package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

import scala.util.{Failure, Success, Try}

final case class Step[State, Response](state: State, response: Response)

final case class Info[Action, State, Response](
  action: Action,
  previousState: State,
  nextState: State,
  response: Response
)

extension (fsr: FormulaStepResult)
  def check[A, T](actions: List[A], state: T): Unit = fsr.fold((): Unit) { problems =>
    val message =
      s"""Formula falsified for $state:
         |${problems.mkString("\n")}
         |trace: $actions""".stripMargin
    throw new AssertionError(message)
  }

extension[Action, State, Response] (f: Formula[Info[Action, State, Response]])
  def check(actions: List[Action], initial: State, step: (Action, State) => Step[State, Response]): Prop.Result = {
    ???
  }

