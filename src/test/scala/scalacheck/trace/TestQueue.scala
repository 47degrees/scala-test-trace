package com.xebia.functional
package scalacheck.trace

import scalacheck.trace.*
import scalacheck.trace.formula.*
import scalacheck.trace.formula.syntax.*

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

import scala.util.{Failure, Success, Try}

trait SizedQueue[A] {
    def push(a: A): Unit
    def pop: Option[A]
    def clear: Unit
}

object SizedQueue {
    def of[A](size: Int): SizedQueue[A] = new SizedQueue[A] {

      val internal = 
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
        }
        first
      }

      override def clear: Unit = {
        internal.clear()
      }
        
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

  val model: ArbModel[Unit, Action] = new StatelessArbModel[Action] {
    override def nexts(): Arbitrary[Option[Action]] = Arbitrary {
      Gen.oneOf(
        Gen.posNum[Int].map(Push.apply),
        Gen.const(Pop), 
        Gen.const(Clear)
      ).map(Some(_))
    }
  }

  val queue = SizedQueue.of[Int](10)

  def behaviour(action: Action, queue: SizedQueue[Int]): Step[SizedQueue[Int], Option[Int]] = action match {
    case Action.Push(v) => 
      queue.push(v)
      Step(state = queue, response = None)
    case Action.Pop => 
      val value = queue.pop
      Step(state = queue, response = value)
    case Action.Clear => 
      queue.clear
      Step(state = queue, response = None)
  }

  def formula: Formula[Info[Action, SizedQueue[Int], Option[Int]]] =
    Always.always {
      // Implies.implies((info: Info[Action, SizedQueue[Int], Option[Int]]) => info.action match { 
      //   case Push(_) => Prop.Result(Prop.True)
      //   case _ => Prop.Result(Prop.False)
      // }, {
      //   Next.next({
      //     Implies.implies((info: Info[Action, SizedQueue[Int], Option[Int]]) => info.action match { 
      //       case Pop => Prop.Result(Prop.True)
      //       case _ => Prop.Result(Prop.False)
      //     },Predicate.holds("", (info: Info[Action, SizedQueue[Int], Option[Int]]) => {
      //       info.response.fold(Prop.Result(Prop.False))(_ => Prop.Result(Prop.True))
      //     })
      //     )
      //   })
      // })
      Implies.implies((info: Info[Action, SizedQueue[Int], Option[Int]]) => info.action match { 
        case Clear => Prop.Result(Prop.True)
        case _ => Prop.Result(Prop.False)
      }, {
        Next.next({
          Implies.implies((info: Info[Action, SizedQueue[Int], Option[Int]]) => info.action match { 
            case Pop => Prop.Result(Prop.True)
            case _ => Prop.Result(Prop.False)
          },Predicate.holds("", (info: Info[Action, SizedQueue[Int], Option[Int]]) => {
            info.response.fold(Prop.Result(Prop.True))(_ => Prop.Result(Prop.False))
          })
          )
        })
      })
    }

  property("checkRight") = forAll(model.gen) { actions =>
    formula.check(actions, SizedQueue.of(10), behaviour)
  }
  
}
