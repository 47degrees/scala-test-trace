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
    always(() => holds[Info[Action, Int, Int]]("non-negative", item => {
      item.action match
        case Action.Read => item.response >= 0
        case _ => true
    }))

  val initialState = 0
  val stepAction: (Action, Int) => Step[Int, Int] = error
  forAllNoShrink(model.gen) { actions =>
    val result = actions.foldLeft((initialState, Prop.Result(Prop.True))) {
      case ((state, result), action) =>
        if (result.success) {
          Try(stepAction(action, state)) match
            case Failure(t) =>
              (state, Prop.Result(Prop.Exception(t)))
            case Success(value) =>
              val info = Info(action, state, value.state, value.response)
              val formulaStep = formula.progress(Right(info))
              (value.state, formulaStep.result)
        } else (state, result)
    }
    Prop(result._2)
  }.check(_.withMinSize(100))

}
