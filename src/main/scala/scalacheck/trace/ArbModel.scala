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

extension[State, Action] (arbModel: ArbModel[State, Action])
  def gen(range: Range = Range(1, 100)): Gen[List[Action]] = 
    range.foldLeft(Gen.const[(List[Action], State, Boolean)]((Nil, arbModel.initial, false))) { 
      case (genState, _) => 
        genState.flatMap {
          case (l, state, true) => Gen.const((l, state, true))
          case (l, state, false) =>
            arbModel
              .nexts(state)
              .arbitrary
              .map(_.fold((l, state, true))(a => (l :+ a, arbModel.step(state, a), false)))
        }
    }.map(_._1)

def checkAgainst[AbstractState, ConcreteState, Action, Response](
  model: ArbModel[AbstractState, Action],
  initial: ConcreteState,
  step: (Action, ConcreteState) => Step[ConcreteState, Response],
range: Range = Range(1, 100),
formula: () => Formula[Info[Action, ConcreteState, Response]]
) = {
  Prop.forAll(model.gen(range)) { actions =>
    Prop(formula().check(actions, initial, step))
  }
}
/*



public fun <State, Action> ArbModel<State, Action>.gen(
  range: IntRange = 1 .. 100
): Arb<List<Action>> = arbitrary(shrinker = PrefixShrinker()) {
  buildList(range.last) {
    var current = initial
    for (step in range) {
      when (val action = nexts(current).bind()) {
        null -> break
        else -> {
          add(action)
          current = step(current, action)
        }
      }
    }
  }
}

public suspend fun <AbstractState, ConcreteState, Action, Response> checkAgainst(
  model: ArbModel<AbstractState, Action>,
  initial: ConcreteState,
  step: suspend (Action, ConcreteState) -> Step<ConcreteState, Response>,
  range: IntRange = 1 .. 100,
  formula: () -> Formula<Info<Action, ConcreteState, Response>>
) {
  checkAll(model.gen(range)) { actions ->
    formula().check(actions, initial, step)
  }
}
*/