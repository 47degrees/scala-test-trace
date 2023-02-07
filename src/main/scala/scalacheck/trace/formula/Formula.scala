package com.xebia.functional
package scalacheck.trace.formula

import org.scalacheck.Prop

import scala.reflect.ClassTag

sealed trait Formula[-A] {
  def negate: Formula[A] = this match {
    case TRUE => FALSE
    case FALSE => TRUE
    case p: Predicate[_] => Not(p)
    case t: Throws => Not(t)
    case Not(formula) => formula
    case And(formulae) => Or.or(formulae.map(_.negate))
    case Or(formulae) => And.and(formulae.map(_.negate))
    case Implies(i, t) =>
      // since not (A => B) = not (not(A) or B) = A and not(B)
      And.and(i, t.negate)
    case Next(formula) => Next(formula.negate)
    case DependentNext(formula) => DependentNext(f => formula(f).negate)
    case Always(formula) => Eventually(formula.negate)
    case Eventually(formula) => Always(formula.negate)
  }

  def pretty: String = this match {
    case TRUE => "true"
    case FALSE => "false"
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
sealed trait Constant extends Atomic[Any]
case object TRUE extends Constant
case object FALSE extends Constant

private[trace] final case class Predicate[-A](message: String, test: A => Prop.Result) extends Atomic[A]
object Predicate {

  /**
   * Basic formula which checks that an item is produced, and satisfies the [predicate].
   */
  def holds[A](message: String, predicate: A => Prop.Result): Atomic[A] =
    Predicate(message, predicate)
}

private[trace] final case class Throws(message: String, test: Throwable => Prop.Result) extends Atomic[Any]
object Throws {

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
}

// logical operators
private[trace] final case class Not[-A](formula: Atomic[A]) extends Formula[A]
object Not {

  /**
   * Negation of a formula. Note that failure messages are not saved accross negation boundaries.
   */
  def not[A](formula: Formula[A]): Formula[A] = formula.negate
}

private[trace] final case class And[-A](formulae: List[Formula[A]]) extends Formula[A]
object And {

  /**
   * Conjunction, `&&` operator.
   */
  def and[A](formulae: Formula[A]*): Formula[A] = and(formulae.toList)

  /**
   * Conjunction, `&&` operator.
   */
  def and[A](formulae: List[Formula[A]]): Formula[A] = {
    val filtered = formulae.flatMap {
      case TRUE => Nil
      case And(f) => f
      case elem => List(elem)
    }
    if (filtered.isEmpty) TRUE else And(filtered)
  }
}

private[trace] final case class Or[-A](formulae: List[Formula[A]]) extends Formula[A]
object Or {

  /**
   * Disjunction, `||` operator.
   */
  def or[A](formulae: Formula[A]*): Formula[A] = or(formulae.toList)

  /**
   * Disjunction, `||` operator.
   */
  def or[A](formulae: List[Formula[A]]): Formula[A] = {
    val filtered = formulae.flatMap {
      case FALSE => Nil
      case Or(f) => f
      case elem => List(elem)
    }
    if (filtered.isEmpty) FALSE else Or(filtered)
  }
}

// it's important that the left-hand side of => is atomic,
// because otherwise we cannot know at each step whether
// to go with the right-hand side or not
private[trace] final case class Implies[-A](`if`: Atomic[A], `then`: Formula[A]) extends Formula[A]
object Implies {

  /**
   * Implication. `implies(oneWay, orAnother)` is satisfied if either `oneWay` is `false`, or `oneWay && orAnother` is `true`.
   */
  def implies[A](`if`: Atomic[A], `then`: Formula[A]): Formula[A] = Implies(`if`, `then`)

  def implies[A](condition: A => Prop.Result, `then`: Formula[A]): Formula[A] =
    implies(Predicate.holds("condition", condition), `then`)

  def implies[A](condition: A => Prop.Result, `then`: () => Formula[A]): Formula[A] =
    implies(Predicate.holds("condition", condition), `then`())
}

// temporal operators
private[trace] final case class Always[-A](formula: Formula[A]) extends Formula[A]
object Always {

  /**
   * Always, specifies that the formula should hold for every item in the sequence.
   */
  def always[A](block: => Formula[A]): Formula[A] = Always(block)
}

private[trace] final case class Eventually[-A](formula: Formula[A]) extends Formula[A]
object Eventually {

  /**
   * Eventually, specifies that the formula should hold for at least one item in the sequence, before the sequence finishes.
   */
  def eventually[A](block: => Formula[A]): Formula[A] = Eventually(block)
}

private[trace] final case class Next[-A](formula: Formula[A]) extends Formula[A]
object Next {

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
    next(current => Always.always(block(current)))
}

// this is key in this development (and different from others)
// the 'next' formula can depend on the *current* state
// we can do that because 'next' is always kept until the next round
private[trace] final case class DependentNext[-A](formula: A => Formula[A]) extends Formula[A]
