package scato
package transformers

import scato.clazz.Bind.syntax._
import scato.clazz.Functor.syntax._
import scato.clazz._

trait OptionTInstances { instances =>
  implicit def functor[F[_]](implicit F: TC[F, Functor]): Functor[OptionT[F, ?]] = new Functor[OptionT[F, ?]] {
    override def map[A, B](oa: OptionT[F, A])(f: A => B): OptionT[F, B] =
      OptionT(oa.run.map(_.map(f)))
  }

  implicit def monad[F[_]](implicit F: TC[F, Monad]): Monad[OptionT[F, ?]] = new Monad[OptionT[F, ?]] {

    override def pure[A](a: A): OptionT[F, A] =
      OptionT(Applicative[F].pure(None))
    override def flatMap[A, B](oa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT(oa.run.flatMap(_.fold(Applicative[F].pure[Option[B]](None))(a => f(a).run)))

    override def ap[A, B](oa: OptionT[F, A])(of: OptionT[F, A => B]): OptionT[F, B] =
      OptionT(of.run.flatMap(_.fold(Applicative[F].pure[Option[B]](None))(f => oa.run.map(_.map(f)))))

  }

  implicit def applicativeTC[F[_], A](implicit F: TC[F, Monad]): TC[OptionT[F, ?], Applicative] =
    TC[OptionT[F, ?], Applicative](monad.applicative)
  implicit def applyTC[F[_], A](implicit F: TC[F, Monad]): TC[OptionT[F, ?], Apply] =
    TC[OptionT[F, ?], Apply](monad.applicative.apply)
  implicit def functorTC[F[_], A](implicit F: TC[F, Functor]): TC[OptionT[F, ?], Functor] =
    TC[OptionT[F, ?], Functor](functor)
  implicit def bindTC[F[_], A](implicit F: TC[F, Monad]): TC[OptionT[F, ?], Bind] =
    TC[OptionT[F, ?], Bind](monad.bind)
}
