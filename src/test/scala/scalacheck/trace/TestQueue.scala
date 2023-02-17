package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.formula.*
import scalacheck.trace.formula.Formula.*
import scalacheck.trace.formula.syntax.*

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.*

trait SizedQueue[A] {
  def push(a: A): Unit
  def pop: Option[A]
  def clear(): Unit
}

object SizedQueue {

  case object PopOnEmptyError extends RuntimeException("")

  def of[A](size: Int): SizedQueue[A] = new SizedQueue[A] {

    val internal: scala.collection.mutable.ListBuffer[A] =
      scala.collection.mutable.ListBuffer.empty[A]

    override def push(a: A): Unit = {
      internal += a
      if (internal.size > size) {
        internal.remove(size)
      }
    }

    override def pop: Option[A] = {
      val first = internal.headOption
      if (first.nonEmpty) {
        internal.remove(0)
      } else throw PopOnEmptyError
      first
    }

    override def clear(): Unit =
      internal.clear()

  }
}

object TestQueue extends Properties("Test Queue") {

  sealed trait Action extends Product with Serializable
  object Action {
    case class Push(value: Int) extends Action
    case object Pop extends Action
    case object Clear extends Action
  }
  import Action.*

  sealed trait Response extends Product with Serializable
  object Response {
    case class MaybeValue(value: Option[Int]) extends Response
    case object UnitValue extends Response
  }
  import Response.*

  val model: ArbModel[Unit, Action] = new StatelessArbModel[Action] {
    override def nexts(): Arbitrary[Option[Action]] = Arbitrary {
      Gen
        .oneOf(
          Gen.posNum[Int].map(Push.apply),
          Gen.const(Pop),
          Gen.const(Clear)
        ).map(Some(_))
    }
  }

  val queue: SizedQueue[Int] = SizedQueue.of[Int](10)

  def behaviour(action: Action, queue: SizedQueue[Int]): Step[SizedQueue[Int], Response] = action match {
    case Action.Push(v) =>
      Step(
        state = queue,
        response = {
          queue.push(v)
          UnitValue
        }
      )
    case Action.Pop =>
      Step(
        state = queue,
        response = {
          val value = queue.pop
          MaybeValue(value)
        }
      )
    case Action.Clear =>
      Step(
        state = queue,
        response = {
          queue.clear()
          UnitValue
        }
      )
  }

  type InfoType = Info[Action, SizedQueue[Int], Response]

  def isPush(info: InfoType): Prop.Result =
    info.action match {
      case Push(_) => Prop.Result(Prop.True)
      case _ => Prop.Result(Prop.False)
    }

  def isPop(info: InfoType): Prop.Result =
    info.action match {
      case Pop => Prop.Result(Prop.True)
      case _ => Prop.Result(Prop.False)
    }

  def isClear(info: InfoType): Prop.Result =
    info.action match {
      case Clear => Prop.Result(Prop.True)
      case _ => Prop.Result(Prop.False)
    }

  def formula: Formula[InfoType] = {
    and(
      always(
        implies[InfoType](
          isPush(_), {
            next({
              implies[InfoType](
                isPop(_),
                Predicate[InfoType](
                  "Expected value",
                  _.response match {
                    case Right(MaybeValue(Some(_))) => Prop.Result(Prop.True)
                    case _ => Prop.Result(Prop.False)
                  }
                )
              )
            })
          }
        )
      ),
      always(
        implies[InfoType](
          isClear(_), {
            next(
              implies[InfoType](
                isPop(_),
                Predicate[InfoType](
                  "Expected non value",
                  _.response match {
                    case Right(MaybeValue(Some(_))) => Prop.Result(Prop.False)
                    case Right(MaybeValue(None)) => Prop.Result(Prop.True)
                    case _ => Prop.Result(Prop.False)
                  }
                )
              )
            )
          }
        )
      ) /*,
      always(
        implies[InfoType](
          isClear(_), {
            next(
              implies[InfoType](
                isPop(_),
                throws[SizedQueue.PopOnEmptyError.type]("PopOnEmptyError not thrown", _ => Prop.Result(Prop.True))
              )
            )
          }
        )
      )*/
    )
  }

  property("checkRight") = forAll(model.gen) { actions =>
    formula.check(actions, queue, behaviour)
  }

}
