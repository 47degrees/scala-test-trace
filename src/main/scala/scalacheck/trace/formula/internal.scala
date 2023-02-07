package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

import scala.util.{Failure, Success, Try}

object internal {

  def contramapAtomic[A, B](f: B => A): Atomic[A] => Atomic[B] = {
    case c: Constant => c
    case Predicate(m, t) => Predicate(m, b => t(f(b)))
    case t: Throws => t
  }

  def contramapFormula[A, B](f: B => A): Formula[A] => Formula[B] = {
    case a: Atomic[A] => contramapAtomic(f)(a)
    case Not(formula) => Not(contramapAtomic(f)(formula))
    case And(formulae) => And(formulae.map(contramapFormula(f)))
    case Or(formulae) => Or(formulae.map(contramapFormula(f)))
    case Implies(i, t) => Implies(contramapAtomic(f)(i), contramapFormula(f)(t))
    case Next(formula) => Next(contramapFormula(f)(formula))
    case DependentNext(formula) => DependentNext({ x => contramapFormula(f)(formula(f(x))) })
    case Always(formula) => Always(contramapFormula(f)(formula))
    case Eventually(formula) => Eventually(contramapFormula(f)(formula))
  }

  def checkFormula[Action, State, Response](actions: List[Action], initial: State, step: (Action, State) => Step[State, Response])(
      f: Formula[Info[Action, State, Response]]
  ): Prop = {

    val (_, pendingFormulas, result) = actions.foldLeft((initial, f, Prop.Result(Prop.Undecided))) { case ((state, form, result), action) =>
      if (result.success || result.status == Prop.Undecided) {
        Try(step(action, state)) match {
          case Failure(t) =>
            val formulaStep = FormulaStep.progress(Left(t))(form)
            (state, formulaStep.next, formulaStep.result)
          case Success(value) =>
            val info = Info(action, state, value.state, value.response)
            val formulaStep = FormulaStep.progress(Right(info))(form)
            (value.state, formulaStep.next, formulaStep.result)
        }
      } else (state, form, result)
    }
    Prop(result && FormulaStep.done(pendingFormulas))
  }
}
