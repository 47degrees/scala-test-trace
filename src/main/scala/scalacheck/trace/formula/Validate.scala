package com.xebia.functional
package scalacheck.trace.formula
import scala.util.Try

final case class Step[State, Response](state: State, response: Either[Throwable, Response])
object Step {
  def apply[State, Response](state: State, response: => Response): Step[State, Response] =
    Step(state, Try(response).toEither)
}

final case class Info[Action, State, Response](
    action: Action,
    previousState: State,
    nextState: State,
    response: Response
)
