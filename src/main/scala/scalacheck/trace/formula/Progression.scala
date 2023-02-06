package com.xebia.functional
package scalacheck.trace.formula

extension (l: FormulaStepResult) private[formula] def isOk: Boolean = l.success

extension (l: List[FormulaStepResult])
  private[formula] def andResults: FormulaStepResult =
    l.reduce(_ && _)
  private[formula] def orResults: FormulaStepResult =
    l.reduce(_ || _)

final case class FormulaStep[A](result: FormulaStepResult, next: Formula[A])

extension [A](atomic: Atomic[A])
  def atomicProgress(x: Result[A]): FormulaStepResult = atomic match
    case TRUE => everythingOk
    case FALSE => problem("fail")
    case Predicate(message, test) =>
      x.fold(
        _ => problem("Unexpected exception, expecting value"),
        a => if (test(a)) everythingOk else problem(message)
      )
    case Throws(message, test) =>
      x.fold(
        a => if (test(a)) everythingOk else problem(message),
        _ => problem("Unexpected item, expecting exception")
      )
extension [A](f: Formula[A])
  def progress(x: Result[A]): FormulaStep[A] = f match
    case a: Atomic[A] => FormulaStep(a.atomicProgress(x), TRUE)
    case Not(formula) =>
      if (formula.atomicProgress(x).isOk) {
        FormulaStep(problem("Negated condition was true"), TRUE)
      } else {
        FormulaStep(everythingOk, TRUE)
      }
    case And(formulae) =>
      val steps = formulae.map(_.progress(x))
      val result = steps.map(_.result).andResults
      FormulaStep(result, and(steps.map(_.next)))
    case Or(formulae) =>
      val steps = formulae.map(_.progress(x))
      val result = steps.map(_.result).orResults
      FormulaStep(result, or(steps.map(_.next)))
    case Implies(i, t) =>
      val leftResult = i.atomicProgress(x)
      if (leftResult.isOk) {
        // if left is true, we check the right
        t.progress(x)
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
      val step = formula.progress(x)
      FormulaStep(step.result, and(step.next, a))
    case e @ Eventually(formula) =>
      val step = formula.progress(x)
      if (step.result.isOk) {
        // this one is true, so we're done
        FormulaStep(everythingOk, TRUE)
      } else {
        // we have to try in the next one
        // so if we are done we haven't proved it yet
        FormulaStep(everythingOk, e)
      }

  // is there something missing to prove?
  // if we have 'eventually', we cannot conclude
  def done: FormulaStepResult = f match
    case _: Atomic[_] => everythingOk
    case _: Not[_] => everythingOk
    case And(formulae) => formulae.map(_.done).andResults
    case Or(formulae) => formulae.map(_.done).orResults
    case Implies(i, t) => List(i.done, t.done).andResults
    // we have nothing missing here
    case _: Next[_] => everythingOk
    case _: DependentNext[_] => everythingOk
    case _: Always[_] => everythingOk
    // we have an 'eventually' missing
    case Eventually(formula) =>
      problem(s"Should hold eventually: ${formula.pretty}")
