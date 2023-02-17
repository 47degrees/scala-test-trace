package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop
import scalacheck.trace.formula.syntax.*
import scalacheck.trace.formula.syntax.given

import scala.language.implicitConversions

final case class FormulaStep[A](result: Prop.Result, next: Formula[A])
object FormulaStep {

  private[this] def problem(message: String): Prop.Result =
    Prop.Result(Prop.False).label(message)

  def atomicProgress[Action, State, Response](x: Info[Action, State, Response]): Atomic[Info[Action, State, Response]] => Prop.Result = {
    case TRUE => true
    case FALSE => problem("fail")
    case Predicate(message, test) =>
      test(x).label(message)
    case Throws(message, test) =>
      x.response.fold(
        a => test(a).label(message),
        _ => problem("Unexpected item, expecting exception")
      )
  }

  def progress[Action, State, Response](
      x: Info[Action, State, Response]
  ): Formula[Info[Action, State, Response]] => FormulaStep[Info[Action, State, Response]] = {
    case a: Atomic[Info[Action, State, Response]] => FormulaStep(atomicProgress(x)(a), TRUE)
    case Not(formula) =>
      if (atomicProgress(x)(formula).success) {
        FormulaStep(problem("Negated condition was true"), TRUE)
      } else {
        FormulaStep(true, TRUE)
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
        FormulaStep(true, TRUE)
      }
    case Next(formula) => FormulaStep(true, formula)
    case DependentNext(formula) =>
      FormulaStep(true, formula(x))
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
        FormulaStep(true, TRUE)
      } else {
        // we have to try in the next one
        // so if we are done we haven't proved it yet
        FormulaStep(true, e)
      }
  }

  // is there something missing to prove?
  // if we have 'eventually', we cannot conclude
  def done[A]: Formula[A] => Prop.Result = {
    case _: Atomic[?] => true
    case _: Not[?] => true
    case And(formulae) => formulae.map(done).reduce(_ && _)
    case Or(formulae) => formulae.map(done).reduce(_ || _)
    case Implies(i, t) => List(done(i), done(t)).reduce(_ && _)
    // we have nothing missing here
    case _: Next[?] => true
    case _: DependentNext[?] => true
    case _: Always[?] => true
    // we have an 'eventually' missing
    case Eventually(formula) =>
      problem(s"Should hold eventually: ${formula.pretty}")
  }
}
