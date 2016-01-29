package scato
package clazz

trait Functor[F[_]] {
  def map[A, B](ma: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  trait Class[F[_]] extends Functor[F]{
    implicit final def functor: Functor[F] = this
  }
  
  object syntax extends FunctorSyntax
}
