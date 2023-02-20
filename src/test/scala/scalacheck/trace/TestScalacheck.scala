package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.formula.*
import scalacheck.trace.formula.Formula.*
import scalacheck.trace.formula.syntax.*

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

object TestScalacheck extends Properties("Sample") {

  import org.scalacheck.Prop.forAll

  object Action extends Enumeration {
    type Action = Value
    val Increment, Read = Value
  }
  import Action.*

  val model: ArbModel[Unit, Action] = new StatelessArbModel[Action] {
    override def nexts(): Arbitrary[Option[Action]] = Arbitrary(Gen.some(Gen.oneOf(Action.Increment, Action.Read)))
  }

  val gen: Gen[TestScalacheck.Action.Value] = Gen.oneOf(Action.Increment, Action.Read)

  def right(action: Action, state: Int): Step[Int, Int] = action match {
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read => Step(state = state, response = state)
  }

  def wrong(action: Action, state: Int): Step[Int, Int] = action match {
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read => Step(state = state, response = if (state == 10) -1 else state)
  }

  def error(action: Action, state: Int): Step[Int, Int] = action match {
    case Action.Increment => Step(state = state + 1, response = 0)
    case Action.Read =>
      Step(
        state = state,
        response = {
          if (state == 10) throw new RuntimeException("ERROR!")
          state + 1
        }
      )
  }

  def formula: Formula[Info[Action, Int, Int]] =
    always {
      holds(
        "non-negative",
        item => {
          val status = item.action match {
            case Action.Read if item.response >= 0 => Prop.True
            case Action.Read => Prop.False
            case _ => Prop.True
          }
          Prop.Result(status)
        }
      )
    }

  val initialState: Int = 0
  val stepAction: (Action, Int) => Step[Int, Int] = right
  val initialFormula: Formula[Info[Action, Int, Int]] = formula

  property("checkRight") = forAll(model.gen) { actions =>
    initialFormula.check(actions, initialState, stepAction)
  }
//
//  property("checkWrong") = forAll(model.gen) { actions =>
//    initialFormula.check(actions, initialState, wrong)
//  }
//
//  property("checkThrow") = forAll(model.gen) { actions =>
//    initialFormula.check(actions, initialState, error)
//  }

}
