package scato
package clazz

trait Traversable[T[_]] {
  def functor: Functor[T]
  def foldable: Foldable[T]

  def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]]
  def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]]
}

object Traversable extends TraversableInstances {
  def apply[T[_]](implicit T: Traversable[T]): Traversable[T] = T

  trait Class[F[_]] extends Traversable[F] with Functor.Class[F] with Foldable.Class[F] {
    implicit final def traversable: Traversable[F] = this
  }
  
  object syntax extends TraversableSyntax
}
