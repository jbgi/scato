package scato
package clazz

abstract class Apply[F[_]] {

  def map[A, B](ma: F[A])(f: A => B): F[B]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  final val functor: Functor[F] = new Functor[F] {
    override def map[A, B](ma: F[A])(f: (A) => B)= Apply.this.map(ma)(f)
  }
}

object Apply {
  def apply[F[_]](implicit F: TC[F, Apply]): Apply[F] = F.instance
  implicit def apply_[M[_]](implicit M: Apply[M]): TC[M, Apply] = TC[M, Apply](M)

  object syntax extends ApplySyntax
}
