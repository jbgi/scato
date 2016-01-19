package scato
package clazz

trait MonadInstancesStdLib {
  implicit val option: Monad[Option] = new Monad[Option] {

    override def pure[A](a: A): Option[A] = Some(a)
    override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)

    override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)
  }

  implicit val list: Monad[List] = new Monad[List] {

    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)

    override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
  }
}
