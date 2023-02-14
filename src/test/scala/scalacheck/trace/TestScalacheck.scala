package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.*
import scalacheck.trace.formula.*
import scalacheck.trace.formula.syntax.*

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

import scala.util.{Failure, Success, Try}

object TestScalacheck extends Properties("Sample") {

  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Prop.{forAll, forAllNoShrink}

  object Action extends Enumeration {
    type Action = Value
    val Increment, Read = Value
  }
  import Action.*

  val model: ArbModel[Unit, Action] = new StatelessArbModel[Action] {
    override def nexts(): Arbitrary[Option[Action]] = Arbitrary(Gen.some(Gen.oneOf(Action.Increment, Action.Read)))
  }

  val gen = Gen.oneOf(Action.Increment, Action.Read)

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
      if (state == 2) throw new RuntimeException("ERROR!")
      Step(state = state, response = state + 1)
  }

  def formula: Formula[Info[Action, Int, Int]] =
    Always.always {
      Predicate.holds(
        "non-negative",
        item => {
          val status = item.action match {
            case Action.Read if item.response.exists(_ >= 0) => Prop.True
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

}
