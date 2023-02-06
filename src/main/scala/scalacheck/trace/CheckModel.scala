package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.*
import scalacheck.trace.formula.*

import org.scalacheck.{Arbitrary, Gen}

object CheckModel extends App {

  enum Action:
    case Increment, Read

  val model: ArbModel[Unit, Action] = new StatelessArbModel[Action] {
    override def nexts(): Arbitrary[Option[Action]] = Arbitrary(Gen.some(Gen.oneOf(Action.Increment, Action.Read)))
  }

  def right(action: Action, state: Int): Step[Int, Int] = action match
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read => Step(state = state, response = state + 1)

  def wrong(action: Action, state: Int): Step[Int, Int] = action match
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read => Step(state = state, response = if (state == 0) 0 else state + 1)

  def formula: Formula[Info[Action, Int, Int]] =
    always(() =>
      holds[Info[Action, Int, Int]](
        "non-negative",
        item => {
          item.action match
            case Action.Read => item.response >= 0
            case _ => true
        }
      )
    )

  TraceTest.checkAgainst(model, -2, wrong, () => formula).check()
}
