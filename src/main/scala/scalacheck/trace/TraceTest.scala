package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.formula.*

import org.scalacheck.{Arbitrary, Prop, Test}

object TraceTest {

  def checkAgainst[AbstractState, ConcreteState, Action, Response](
      model: ArbModel[AbstractState, Action],
      initial: ConcreteState,
      step: (Action, ConcreteState) => Step[ConcreteState, Response],
      formula: () => Formula[Info[Action, ConcreteState, Response]]
  ) =
    Prop.forAll(model.gen) { actions =>
      Prop(formula().check(actions, initial, step))
    }

}
