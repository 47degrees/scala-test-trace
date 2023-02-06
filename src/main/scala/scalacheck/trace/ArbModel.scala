package com.xebia.functional
package scalacheck.trace

import org.scalacheck.{Arbitrary, Gen, Prop, Shrink, Test}
import scalacheck.trace.formula.*

trait ArbModel[State, Action] {
  def initial: State
  def nexts(state: State): Arbitrary[Option[Action]]
  def step(state: State, action: Action): State
}

trait StatelessArbModel[Action] extends ArbModel[Unit, Action] {
  val initial: Unit = {}
  def nexts(state: Unit): Arbitrary[Option[Action]] = nexts()
  def step(state: Unit, action: Action): Unit = {}
  def nexts(): Arbitrary[Option[Action]]
}

extension [State, Action](arbModel: ArbModel[State, Action])
  def gen: Gen[List[Action]] = Gen.sized { size =>
    (1 to size)
      .foldLeft(Gen.const[(List[Action], State, Boolean)]((Nil, arbModel.initial, false))) { case (genState, _) =>
        genState.flatMap {
          case (l, state, true) => Gen.const((l, state, true))
          case (l, state, false) =>
            arbModel
              .nexts(state)
              .arbitrary
              .map(_.fold((l, state, true))(a => (l :+ a, arbModel.step(state, a), false)))
        }
      }.map(_._1)
  }
