package scato
package clazz

trait Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object Bind extends BindInstances {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
  
  trait Class[F[_]] extends Template[F] with Ap[F] {}

  trait Template[F[_]] extends Bind[F] with Apply.Class[F] {
    implicit final def bind: Bind[F] = this
  }

  trait Ap[F[_]] extends Apply[F] { self: Bind[F] =>
    override final def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(functor.map(fa))
  }
  
  object syntax extends BindSyntax
}
