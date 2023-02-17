package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

import scala.reflect.ClassTag

sealed trait Formula[-A] {
  def negate: Formula[A] = this match {
    case c: Constant => c.negate
    case p: Predicate[?] => Not(p)
    case t: Throws => Not(t)
    case Not(formula) => formula
    case And(formulae) => Formula.or(formulae.map(_.negate))
    case Or(formulae) => Formula.and(formulae.map(_.negate))
    case Implies(i, t) =>
      // since not (A => B) = not (not(A) or B) = A and not(B)
      Formula.and(i, t.negate)
    case Next(formula) => Next(formula.negate)
    case DependentNext(formula) => DependentNext(f => formula(f).negate)
    case Always(formula) => Eventually(formula.negate)
    case Eventually(formula) => Always(formula.negate)
  }

  def pretty: String = this match {
    case c: Constant => c.pretty
    case Predicate(message, _) => s"{ $message }"
    case Throws(message, _) => s"{ $message }"
    case Not(formula) => s"!${formula.pretty}"
    case And(formulae) => formulae.mkString(" & ")
    case Or(formulae) => formulae.mkString(" | ")
    case Implies(i, t) => s"${i.pretty} => ${t.pretty}"
    case Next(formula) => s"next { ${formula.pretty} }"
    case DependentNext(_) => "next { * }"
    case Always(formula) => s"always { ${formula.pretty} }"
    case Eventually(formula) => s"eventually { ${formula.pretty} }"
  }
}

/**
 * Atomic formulae contain *no* temporal operators.
 */
sealed trait Atomic[-A] extends Formula[A]

/**
 * Constant formulae.
 */
private[trace] final case class Constant(status: Prop.Status) extends Atomic[Any] {
  override def pretty: String = status match {
    case Prop.Proof => "Proof"
    case Prop.True => "True"
    case Prop.False => "False"
    case Prop.Undecided => "Undecided"
    case Prop.Exception(e) => s"Exception(${e.getClass.getName})"
  }
  override def negate: Formula[Any] = {
    val newStatus = status match {
      case Prop.Proof => Prop.False
      case Prop.True => Prop.False
      case Prop.False => Prop.True
      case Prop.Undecided => Prop.Undecided
      case e: Prop.Exception => e
    }
    Constant(newStatus)
  }
}

private[trace] final case class Predicate[-A](message: String, test: A => Prop.Result) extends Atomic[A]
private[trace] final case class Throws(message: String, test: Throwable => Prop.Result) extends Atomic[Any]
private[trace] final case class Not[-A](formula: Atomic[A]) extends Formula[A]
private[trace] final case class And[-A](formulae: List[Formula[A]]) extends Formula[A]
private[trace] final case class Or[-A](formulae: List[Formula[A]]) extends Formula[A]
private[trace] final case class Implies[-A](`if`: Atomic[A], `then`: Formula[A]) extends Formula[A]
private[trace] final case class Always[-A](formula: Formula[A]) extends Formula[A]
private[trace] final case class Eventually[-A](formula: Formula[A]) extends Formula[A]
private[trace] final case class Next[-A](formula: Formula[A]) extends Formula[A]
private[trace] final case class DependentNext[-A](formula: A => Formula[A]) extends Formula[A]

object Formula {

  val TRUE: Constant = constant(Prop.True)
  val FALSE: Constant = constant(Prop.False)

  def constant(status: Prop.Status): Constant = Constant(status)

  /**
   * Basic formula which checks that an item is produced, and satisfies the [predicate].
   */
  def holds[A](message: String, predicate: A => Prop.Result): Atomic[A] =
    Predicate(message, predicate)

  /**
   * Basic formula which checks that an exception of type [T] has been thrown.
   */
  def throws[T <: Throwable: ClassTag](message: String, predicate: T => Prop.Result = (_: T) => Prop.Result(Prop.True)): Atomic[Any] =
    Throws(
      message,
      {
        case elem: T => predicate(elem)
        case t => Prop.Result(Prop.Exception(t))
      }
    )

  /**
   * Negation of a formula. Note that failure messages are not saved accross negation boundaries.
   */
  def not[A](formula: Formula[A]): Formula[A] = formula.negate

  /**
   * Conjunction, `&&` operator.
   */
  def and[A](formulae: Formula[A]*): Formula[A] = and(formulae.toList)

  /**
   * Conjunction, `&&` operator.
   */
  def and[A](formulae: List[Formula[A]]): Formula[A] = {
    val filtered = formulae.flatMap {
      case Constant(Prop.True) => Nil
      case And(f) => f
      case elem => List(elem)
    }
    if (filtered.isEmpty) Constant(Prop.True) else And(filtered)
  }

  /**
   * Disjunction, `||` operator.
   */
  def or[A](formulae: Formula[A]*): Formula[A] = or(formulae.toList)

  /**
   * Disjunction, `||` operator.
   */
  def or[A](formulae: List[Formula[A]]): Formula[A] = {
    val filtered = formulae.flatMap {
      case Constant(Prop.False) => Nil
      case Or(f) => f
      case elem => List(elem)
    }
    if (filtered.isEmpty) Constant(Prop.False) else Or(filtered)
  }

  /**
   * Implication. `implies(oneWay, orAnother)` is satisfied if either `oneWay` is `false`, or `oneWay && orAnother` is `true`.
   */
  def implies[A](`if`: Atomic[A], `then`: Formula[A]): Formula[A] = Implies(`if`, `then`)

  def implies[A](condition: A => Prop.Result, `then`: Formula[A]): Formula[A] =
    implies(Predicate("condition", condition), `then`)

  def implies[A](condition: A => Prop.Result, `then`: () => Formula[A]): Formula[A] =
    implies(Predicate("condition", condition), `then`())

  /**
   * Always, specifies that the formula should hold for every item in the sequence.
   */
  def always[A](block: => Formula[A]): Formula[A] = Always(block)

  /**
   * Eventually, specifies that the formula should hold for at least one item in the sequence, before the sequence finishes.
   */
  def eventually[A](block: => Formula[A]): Formula[A] = Eventually(block)

  /**
   * Next, specifies that the formula should hold in the next state.
   */
  def next[A](block: => Formula[A]): Formula[A] = Next(block)

  /**
   * Next, specifies that the formula should hold in the next state. That formula may depend on the current item being processed.
   */
  def next[A](block: A => Formula[A]): Formula[A] = DependentNext(block)

  /**
   * Specifies that the formula must be true in every state after the current one.
   */
  def afterwards[A](block: A => Formula[A]): Formula[A] =
    next(current => always(block(current)))
}
