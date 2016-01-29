package scato
package clazz

trait Apply[F[_]] {
  def functor: Functor[F]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

object Apply {
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  trait Class[F[_]] extends Apply[F] with Functor.Class[F] {
    implicit final def apply: Apply[F] = this
  }

  object syntax extends ApplySyntax
}
