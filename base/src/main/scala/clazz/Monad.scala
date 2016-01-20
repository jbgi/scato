package scato
package clazz

abstract class Monad[M[_]] {

  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = {
    lazy val fa0 = fa
    flatMap(f)(map(fa0))
  }

  final val applicative = new Applicative[M] {
    override def pure[A](a: A): M[A] = Monad.this.pure(a)
    override def ap[A, B](fa: M[A])(f: M[(A) => B]): M[B] = Monad.this.ap(fa)(f)
    override def map[A, B](ma: M[A])(f: (A) => B): M[B] = Monad.this.map(ma)(f)
  }
  final val bind = new Bind[M] {
    override def map[A, B](ma: M[A])(f: (A) => B): M[B] = Monad.this.map(ma)(f)
    override def flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] = Monad.this.flatMap(ma)(f)
    override def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = Monad.this.ap(fa)(f)
  }
}

object Monad extends MonadInstances with MonadInstancesStdLib {
  def apply[M[_]](implicit M: TC[M, Monad]): Monad[M] = M.instance
  implicit def monad[M[_]](implicit M: Monad[M]): TC[M, Monad] = TC[M, Monad](M)
}

