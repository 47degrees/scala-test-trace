package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

object syntax {
  implicit class AtomicOps[A](private val a: Atomic[A]) {
    def contramap[B](f: B => A): Atomic[B] =
      internal.contramapAtomic(f)(a)
  }

  implicit class FormulaOps[A](private val formula: Formula[A]) {
    def contramap[B](f: B => A): Formula[B] =
      internal.contramapFormula(f)(formula)
  }

  implicit class FormulaCheckOps[Action, State, Response](private val f: Formula[Info[Action, State, Response]]) {
    def check(actions: List[Action], initial: State, step: (Action, State) => Step[State, Response]): Prop =
      internal.checkFormula(actions, initial, step)(f)
  }
}
