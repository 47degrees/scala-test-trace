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

extension [Action, State, Response](f: Formula[Info[Action, State, Response]])
  def check(actions: List[Action], initial: State, step: (Action, State) => Step[State, Response]): Prop = {

    val (_, pendingFormulas, result) = actions.foldLeft((initial, f, Prop.Result(Prop.Undecided))) { case ((state, form, result), action) =>
      if (result.success || result.status == Prop.Undecided) {
        Try(step(action, state)) match
          case Failure(t) =>
            val formulaStep = form.progress(Left(t))
            (state, formulaStep.next, formulaStep.result)
          case Success(value) =>
            val info = Info(action, state, value.state, value.response)
            val formulaStep = form.progress(Right(info))
            (value.state, formulaStep.next, formulaStep.result)
      } else (state, form, result)
    }
    Prop(result && pendingFormulas.done)
  }
