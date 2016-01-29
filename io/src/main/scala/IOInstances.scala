package scato
package io

import scato.clazz.{BindAp, MonadTemplate}

import IO.syntax.constIO

trait IOInstances extends MonadTemplate[IO] with BindAp[IO] {
  override def flatMap[A, B](ioa: IO[A])(f: A => IO[B]): IO[B] = ioa.flatMap(f)
  override def map[A, B](ioa: IO[A])(f: A => B): IO[B] = ioa.map(f)
  override def pure[A](a: A): IO[A] = constIO[A](a)
}
