package scato
package clazz

trait Monad[M[_]] {
  def applicative: Applicative[M]
  def bind: Bind[M]
}

object Monad extends MonadInstances with MonadInstancesStdLib {
  def apply[M[_]](implicit M: Monad[M]): Monad[M] = M

  trait Class[F[_]] extends Template[F] with Map[F] with Bind.Ap[F] {}

  trait Template[F[_]] extends Monad[F] with Bind.Template[F] with Applicative.Template[F] {
    implicit final def monad: Monad[F] = this
  }

  trait Map[F[_]] extends Functor[F] { self: Monad[F] =>
    override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = bind.flatMap(ma)(a => applicative.pure(f(a)))
  }
}

