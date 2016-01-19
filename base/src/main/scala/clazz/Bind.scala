package scato
package clazz

abstract class Bind[M[_]] {

  def map[A, B](ma: M[A])(f: A => B): M[B]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = {
    lazy val fa0 = fa
    flatMap(f)(map(fa0))
  }

  final val apply = new Apply[M] {
    override def map[A, B](ma: M[A])(f: (A) => B): M[B] = Bind.this.map(ma)(f)
    override def ap[A, B](fa: M[A])(f: M[(A) => B]): M[B] = Bind.this.ap(fa)(f)
  }
}

object Bind extends BindInstances {
  def apply[F[_]](implicit F: TC[F, Bind]): Bind[F] = F.instance
  implicit def bind[M[_]](implicit M: Bind[M]): TC[M, Bind] = TC[M, Bind](M)

  object syntax extends BindSyntax
}
