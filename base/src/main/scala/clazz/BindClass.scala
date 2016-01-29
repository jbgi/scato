package scato
package clazz

trait BindClass[F[_]] extends BindTemplate[F] with BindAp[F] {}

trait BindTemplate[F[_]] extends Bind[F] with ApplyClass[F] {
  implicit final def bind: Bind[F] = this
}

trait BindAp[F[_]] extends Apply[F] { self: Bind[F] =>
  override final def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(functor.map(fa))
}