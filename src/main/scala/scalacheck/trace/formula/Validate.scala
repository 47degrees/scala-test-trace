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

final case class ResultStep[Action, State, Response](
    result: Prop.Result,
    state: State,
    formula: Formula[Info[Action, State, Response]],
    actions: List[Action],
    ended: Boolean
)
object ResultStep {
  def init[Action, State, Response](s: State, f: Formula[Info[Action, State, Response]]): ResultStep[Action, State, Response] =
    ResultStep(Prop.Result(Prop.Undecided), s, f, Nil, false)
}

extension [Action, State, Response](f: Formula[Info[Action, State, Response]])
  def check(actions: List[Action], initial: State, step: (Action, State) => Step[State, Response]): Prop.Result = {

    val initialStep: ResultStep[Action, State, Response] =
      ResultStep.init(initial, f)

    actions
      .foldLeft(initialStep) {
        case (rs, action) if !rs.ended =>
          Try(step(action, rs.state)) match
            case Success(value) =>
              val info = Info(action, rs.state, value.state, value.response)
              val progress = rs.formula.progress(Right(info))
              val currentState: State = info.nextState
              val currentFormula: Formula[Info[Action, State, Response]] = progress.next
              rs.copy(Prop.Result(Prop.True), currentState, currentFormula, rs.actions)
            case Failure(exception) =>
              val progress = rs.formula.progress(Left(exception))
              // TODO - rs.actions
              val result = Prop.Result(Prop.False).++(progress.next.done)
              rs.copy(rs.result ++ result, ended = true)
        case (rs, _) => rs
      }.result
  }
