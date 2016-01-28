package scato
package clazz

trait MonadClass[F[_]] extends Monad[F] with BindClass[F] with ApplicativeClass[F] {
  implicit final def monad: Monad[F] = this
}

trait DeriveMapMonad[F[_]] extends Functor[F] { self: Monad[F] =>
  override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = bind.flatMap(ma)(a => applicative.pure(f(a)))
}
