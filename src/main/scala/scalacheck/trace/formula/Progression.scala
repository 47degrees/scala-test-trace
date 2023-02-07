package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

final case class FormulaStep[A](result: FormulaStepResult, next: Formula[A])
object FormulaStep {
  def atomicProgress[A](x: Result[A]): Atomic[A] => FormulaStepResult = {
    case TRUE => everythingOk
    case FALSE => problem("fail")
    case Predicate(message, test) =>
      x.fold(
        t => error(message, t),
        a => test(a).label(message)
      )
    case Throws(message, test) =>
      x.fold(
        a => test(a).label(message),
        t => problem("Unexpected item, expecting exception")
      )
  }

  def progress[A](x: Result[A]): Formula[A] => FormulaStep[A] = {
    case a: Atomic[A] => FormulaStep(atomicProgress(x)(a), TRUE)
    case Not(formula) =>
      if (atomicProgress(x)(formula).success) {
        FormulaStep(problem("Negated condition was true"), TRUE)
      } else {
        FormulaStep(everythingOk, TRUE)
      }
    case And(formulae) =>
      val steps = formulae.map(progress(x))
      val result = steps.map(_.result).reduce(_ && _)
      FormulaStep(result, And.and(steps.map(_.next)))
    case Or(formulae) =>
      val steps = formulae.map(progress(x))
      val result = steps.map(_.result).reduce(_ || _)
      FormulaStep(result, Or.or(steps.map(_.next)))
    case Implies(i, t) =>
      val leftResult = atomicProgress(x)(i)
      if (leftResult.success) {
        // if left is true, we check the right
        progress(x)(t)
      } else {
        // otherwise the formula is true (false => x == true)
        FormulaStep(everythingOk, TRUE)
      }
    case Next(formula) => FormulaStep(everythingOk, formula)
    case DependentNext(formula) =>
      x.fold(
        _ => FormulaStep(everythingOk, TRUE),
        a => FormulaStep(everythingOk, formula(a))
      )
    case a @ Always(formula) =>
      // when we have always it has to be true
      // 1. in this state,
      // 2. in any other next state
      val step = progress(x)(formula)
      FormulaStep(step.result, And.and(step.next, a))
    case e @ Eventually(formula) =>
      val step = progress(x)(formula)
      if (step.result.success) {
        // this one is true, so we're done
        FormulaStep(everythingOk, TRUE)
      } else {
        // we have to try in the next one
        // so if we are done we haven't proved it yet
        FormulaStep(everythingOk, e)
      }
  }

  // is there something missing to prove?
  // if we have 'eventually', we cannot conclude
  def done[A]: Formula[A] => FormulaStepResult = {
    case _: Atomic[_] => everythingOk
    case _: Not[_] => everythingOk
    case And(formulae) => formulae.map(done).reduce(_ && _)
    case Or(formulae) => formulae.map(done).reduce(_ || _)
    case Implies(i, t) => List(done(i), done(i)).reduce(_ && _)
    // we have nothing missing here
    case _: Next[_] => everythingOk
    case _: DependentNext[_] => everythingOk
    case _: Always[_] => everythingOk
    // we have an 'eventually' missing
    case Eventually(formula) =>
      problem(s"Should hold eventually: ${formula.pretty}")
  }
}
