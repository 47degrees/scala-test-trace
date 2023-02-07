package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

object syntax:
  extension [A, B](a: Atomic[A])
    def contramap(f: B => A): Atomic[B] = 
      internal.contramapAtomic(f)(a)

  extension [A, B](formula: Formula[A])
    def contramap(f: B => A): Formula[B] =
      internal.contramapFormula(f)(formula)

  extension [Action, State, Response](f: Formula[Info[Action, State, Response]])
    def check(actions: List[Action], initial: State, step: (Action, State) => Step[State, Response]): Prop =
      internal.checkFormula(actions, initial, step)(f)
