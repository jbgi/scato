package scato
package clazz


trait ApplicativeClass[F[_]] extends ApplicativeTemplate[F] with ApplicativeMap[F] {}

trait ApplicativeTemplate[F[_]] extends Applicative[F] with ApplyClass[F] {
  implicit final def applicative: Applicative[F] = this
}

trait ApplicativeMap[F[_]] extends Functor[F] { self: Applicative[F] =>
  override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = apply.ap(ma)(pure(f))
}

