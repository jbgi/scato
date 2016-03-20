package typeclasses.subtyping

// Type classes hierarchy

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait Bind[F[_]] extends Apply[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] extends Applicative[F] with Bind[F] {}

// Instances

trait MonadInstancesStd {

  implicit val list: Monad[List] = new Monad[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B] = xs.flatMap(a => f.map(_ (a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    override def pure[A](a: A): List[A] = List(a)
  }

}

trait BindInstancesStd extends MonadInstancesStd
trait ApplicativeInstancesStd extends MonadInstancesStd
trait ApplyInstancesStd extends ApplicativeInstancesStd with BindInstancesStd
trait FunctorInstancesStd extends ApplyInstancesStd

// Companion objects


object Functor extends FunctorInstancesStd {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}

object Apply extends ApplyInstancesStd{
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F
}

object Applicative extends ApplicativeInstancesStd{
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}

object Bind extends BindInstancesStd {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
}

object Monad extends MonadInstancesStd {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}
