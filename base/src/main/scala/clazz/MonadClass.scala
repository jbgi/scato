package scato
package clazz

trait MonadClass[F[_]] extends MonadTemplate[F] with MonadMap[F] with BindAp[F] {}

trait MonadTemplate[F[_]] extends Monad[F] with BindTemplate[F] with ApplicativeTemplate[F] {
  implicit final def monad: Monad[F] = this
}

trait MonadMap[F[_]] extends Functor[F] { self: Monad[F] =>
  override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = bind.flatMap(ma)(a => applicative.pure(f(a)))
}