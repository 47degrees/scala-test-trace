package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.*
import scalacheck.trace.formula.*

import org.scalacheck.{Arbitrary, Gen, Prop}

import scala.util.{Failure, Success, Try}

object TestScalacheck extends App {

  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Prop.{forAll, forAllNoShrink}

  enum Action:
    case Increment, Read

  val model: ArbModel[Unit, Action] = new StatelessArbModel[Action] {
    override def nexts(): Arbitrary[Option[Action]] = Arbitrary(Gen.some(Gen.oneOf(Action.Increment, Action.Read)))
  }

  val gen = Gen.oneOf(Action.Increment, Action.Read)

  def right(action: Action, state: Int): Step[Int, Int] = action match
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read => Step(state = state, response = state + 1)

  def wrong(action: Action, state: Int): Step[Int, Int] = action match
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read => Step(state = state, response = if (state == 2) -1 else state + 1)

  def error(action: Action, state: Int): Step[Int, Int] = action match
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read =>
      if (state == 2) throw new RuntimeException("ERROR!")
      Step(state = state, response = state + 1)

  def formula: Formula[Info[Action, Int, Int]] =
    always(() =>
      holds(
        "non-negative",
        item => {
          item.action match
            case Action.Read if item.response >= 0 => Prop.Result(Prop.True)
            case Action.Read => Prop.Result(Prop.False)
            case _ => Prop.Result(Prop.True)
        }
      )
    )

  val initialState: Int = 0
  val stepAction: (Action, Int) => Step[Int, Int] = right
  val initialFormula: Formula[Info[Action, Int, Int]] = formula
  forAllNoShrink(model.gen) { actions =>
    initialFormula.check(actions, initialState, stepAction)
  }.check(_.withMinSize(100))

}
