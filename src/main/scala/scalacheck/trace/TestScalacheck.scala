package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.CheckModel.Action
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
            case Action.Read => item.response >= 0
            case _ => true
        }
      )
    )

  val initialState: Int = 0
  val stepAction: (Action, Int) => Step[Int, Int] = error
  val initialFormula: Formula[Info[Action, Int, Int]] = formula
  forAllNoShrink(model.gen) { actions =>
    val result = actions.foldLeft((initialState, initialFormula, Prop.Result(Prop.Undecided))) { case ((state, form, result), action) =>
      if (result.success || result.status == Prop.Undecided) {
        Try(stepAction(action, state)) match
          case Failure(t) =>
            val formulaStep = form.progress(Left(t))
            (state, formulaStep.next, formulaStep.result)
          case Success(value) =>
            val info = Info(action, state, value.state, value.response)
            val formulaStep = form.progress(Right(info))
            (value.state, formulaStep.next, formulaStep.result)
      } else (state, form, result)
    }
    Prop(result._2.done && result._3)
  }.check(_.withMinSize(100))

}
