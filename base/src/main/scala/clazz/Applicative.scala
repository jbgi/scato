package scato
package clazz

trait Applicative[F[_]] {
  def apply: Apply[F]
  def pure[A](a: A): F[A]
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  trait Class[F[_]] extends Template[F] with Map[F] {}

  trait Template[F[_]] extends Applicative[F] with Apply.Class[F] {
    implicit final def applicative: Applicative[F] = this
  }

  trait Map[F[_]] extends Functor[F] { self: Applicative[F] =>
    override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = apply.ap(ma)(pure(f))
  }

  object syntax extends ApplicativeSyntax
}
