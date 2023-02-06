package com.xebia.functional
package scalacheck.trace.formula

import scala.reflect.ClassTag

sealed trait Formula[-A]:
  def negate: Formula[A] = this match
    case TRUE => FALSE
    case FALSE => TRUE
    case p: Predicate[_] => Not(p)
    case t: Throws => Not(t)
    case Not(formula) => formula
    case And(formulae) => or(formulae.map(_.negate))
    case Or(formulae) => and(formulae.map(_.negate))
    case Implies(i, t) =>
      // since not (A => B) = not (not(A) or B) = A and not(B)
      and(i, t.negate)
    case Next(formula) => Next(formula.negate)
    case DependentNext(formula) => DependentNext(f => formula(f).negate)
    case Always(formula) => Eventually(formula.negate)
    case Eventually(formula) => Always(formula.negate)

  def pretty: String = this match
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

extension[A, B] (a: Atomic[A])
  def contramap(f: B => A): Atomic[B] = a match
    case c: Constant => c
    case Predicate(m, t) => Predicate(m, {b => t(f(b))})
    case t: Throws => t

extension[A, B] (formula: Formula[A])
  def contramap(f: B => A): Formula[B] = formula match
    case a: Atomic[A] => a.contramap(f)
    case Not(formula) => Not(formula.contramap(f))
    case And(formulae) => And(formulae.map(_.contramap(f)))
    case Or(formulae) => Or(formulae.map(_.contramap(f)))
    case Implies(i, t) => Implies(i.contramap(f), t.contramap(f))
    case Next(formula) => Next(formula.contramap(f))
    case DependentNext(formula) => DependentNext({ x => formula(f(x)).contramap(f) })
    case Always(formula) => Always(formula.contramap(f))
    case Eventually(formula) => Eventually(formula.contramap(f))

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

private[trace] final case class Predicate[-A](message: String, test: A => Boolean) extends Atomic[A]
private[trace] final case class Throws(message: String, test: Throwable => Boolean) extends Atomic[Any]

  // logical operators
private[trace] final case class Not[-A](formula: Atomic[A]) extends Formula[A]
private[trace] final case class And[-A](formulae: List[Formula[A]]) extends Formula[A]
private[trace] final case class Or[-A](formulae: List[Formula[A]]) extends Formula[A]
    // it's important that the left-hand side of => is atomic,
    // because otherwise we cannot know at each step whether
    // to go with the right-hand side or not
private[trace] final case class Implies[-A](`if`: Atomic[A], `then`: Formula[A]) extends Formula[A]

  // temporal operators
private[trace] final case class Always[-A](formula: Formula[A]) extends Formula[A]
private[trace] final case class Eventually[-A](formula: Formula[A]) extends Formula[A]
private[trace] final case class Next[-A](formula: Formula[A]) extends Formula[A]
    // this is key in this development (and different from others)
    // the 'next' formula can depend on the *current* state
    // we can do that because 'next' is always kept until the next round
private[trace] final case class DependentNext[-A](formula: A => Formula[A]) extends Formula[A]

/**
 * Basic formula which checks that an item is produced, and satisfies the [predicate].
 */
def holds[A](message: String, predicate: A => Boolean): Atomic[A] =
  Predicate(message, predicate)

/**
* Basic formula which checks that an exception of type [T] has been thrown.
*/
def throws[T <: Throwable: ClassTag](message: String, predicate: T => Boolean = (_: T) => true ): Atomic[Any] =
  Throws(
    message,
    {
      case elem: T => predicate(elem)
      case _ => false
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
    case TRUE => Nil
    case And(f) => f
    case elem => List(elem)
  }
  if (filtered.isEmpty) TRUE else And(filtered)
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
    case FALSE => Nil
    case Or(f) => f
    case elem => List(elem)
  }
  if (filtered.isEmpty) FALSE else Or(filtered)
}

/**
 * Implication. `implies(oneWay, orAnother)` is satisfied if either `oneWay` is `false`,
 * or `oneWay && orAnother` is `true`.
 */
def implies[A](`if`: Atomic[A], `then`: Formula[A]): Formula[A] = Implies(`if`, `then`)

def implies[A](condition: A => Boolean, `then`: Formula[A]): Formula[A] =
  implies(holds("condition", condition), `then`)

def implies[A](condition: A => Boolean, `then`: () => Formula[A]): Formula[A] =
  implies(holds("condition", condition), `then`())

/**
* Next, specifies that the formula should hold in the next state.
*/
def next[A](block: () => Formula[A]): Formula[A] = Next(block())

/**
* Next, specifies that the formula should hold in the next state.
* That formula may depend on the current item being processed.
*/
def next[A](block: A => Formula[A]): Formula[A] = DependentNext(block)

/**
* Specifies that the formula must be true in every state
* after the current one.
*/
def afterwards[A](block: A => Formula[A]): Formula[A] =
  next { current => always({() => block(current)}) }

/**
* Always, specifies that the formula should hold for every item in the sequence.
*/
def always[A](block: () => Formula[A]): Formula[A] = Always(block())

/**
* Always, specifies that the formula should hold for at least one item in the sequence,
* before the sequence finishes.
*/
def eventually[A](block: () => Formula[A]): Formula[A] = Eventually(block())