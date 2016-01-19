package scato
package clazz

abstract class Applicative[F[_]] {

  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))

  final val apply: Apply[F] = new Apply[F] {
    override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] = Applicative.this.ap(fa)(f)
    override def map[A, B](ma: F[A])(f: (A) => B): F[B] = Applicative.this.map(ma)(f)
  }
}

object Applicative {
  def apply[F[_]](implicit F: TC[F, Applicative]): Applicative[F] = F.instance
  implicit def applicative[F[_]](implicit F: Applicative[F]): TC[F, Applicative] = TC[F, Applicative](F)

  object syntax extends ApplicativeSyntax
}
