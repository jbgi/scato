package scato
package data
import scala.reflect.runtime.universe.TypeTag
import scato.clazz._
import scato.data.Disjunction.{L_, R_, \/}

trait DisjunctionInstances {
  implicit def monad[L:TypeTag]: TC[L \/ ?, Monad] = TC.capture[L \/ ?, Monad](new Monad[L \/ ?] {
    override val applicative = new Applicative[L \/ ?] {
      override val apply = new Apply[L \/ ?] {
        override val functor = new Functor[L \/ ?] {
          override def map[A, B](ma: L \/ A)(f: A => B): L \/ B =
            ma.fold[L \/ B](l => L_(l))(r => R_(f(r)))
        }
        override def ap[A, B](ma: L \/ A)(mf: L \/ (A => B)): L \/ B =
          ma.fold[L \/ B](l => L_(l))(a => functor.map[(A => B), B](mf)(f => f(a)))
      }
      override def pure[A](a: A): L \/ A =
        R_[A](a)
    }

    override val bind = new Bind[L \/ ?] {
      def apply = applicative.apply
      override def flatMap[A, B](oa: L \/ A)(f: A => L \/ B): L \/ B =
        oa.fold[L \/ B](l => L_(l))(a => f(a))
    }
  })

  implicit def applicativeTC[L:TypeTag]: TC[L \/ ?, Applicative] = TC[L \/ ?, Applicative](monad[L].instance.applicative)
  implicit def applyTC[L:TypeTag]: TC[L \/ ?, Apply] = TC[L \/ ?, Apply](monad[L].instance.applicative.apply)
  implicit def functorTC[L:TypeTag]: TC[L \/ ?, Functor] = TC[L \/ ?, Functor](monad[L].instance.applicative.apply.functor)
  implicit def bindTC[L:TypeTag]: TC[L \/ ?, Bind] = TC[L \/ ?, Bind](monad[L].instance.bind)
}
