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
