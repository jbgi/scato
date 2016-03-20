package typeclasses.tc

// the classic type classes, but will not be used directly in constraints/implicits:

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

// So... instead of encoding our typeclass hierarchy with subtyping,
// We wrap our instances in a newtype `TC[T[_], C[_[_]]]` that give use some structure
// and an opportunity to override the default implicits resolution mechanism.

// => the BH0 > BH1 > BH2 hierarchy is there only to order the implicits priority and affect the resolution.

trait BH0 extends BH1 {
  implicit def monadBind[F[_]](implicit F: TC[F, Monad]): TC[F, Bind] = TC(F.instance)
  implicit def monadApplicative[F[_]](implicit F: TC[F, Monad]): TC[F, Applicative] = TC(F.instance)
  implicit def monadApply[F[_]](implicit F: TC[F, Monad]): TC[F, Apply] = TC(F.instance)
  implicit def monadFunctor[F[_]](implicit F: TC[F, Monad]): TC[F, Functor] = TC(F.instance)
}

trait BH1 extends BH2 {
  implicit def applicativeApply[F[_]](implicit F: TC[F, Applicative]): TC[F, Apply] = TC(F.instance)
  implicit def applicativeFunctor[F[_]](implicit F: TC[F, Applicative]): TC[F, Functor] = TC(F.instance)
}

trait BH2 {
  implicit def applyFunctor[F[_]](implicit F: TC[F, Apply]): TC[F, Functor] = TC(F.instance)
}


// Instances

trait MonadInstancesStd {

  implicit val listMonad: TC[List, Monad] = TC(new Monad[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B] = xs.flatMap(a => f.map(_ (a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    override def pure[A](a: A): List[A] = List(a)
  })

}

// Companion objects

object Functor {
  def apply[F[_]](implicit F: TC[F, Functor]): Functor[F] = F.instance
}

object Apply {
  def apply[F[_]](implicit F: TC[F, Apply]): Apply[F] = F.instance
}

object Applicative {
  def apply[F[_]](implicit F: TC[F, Applicative]): Applicative[F] = F.instance
}

object Bind {
  def apply[F[_]](implicit F: TC[F, Bind]): Bind[F] = F.instance
}

object Monad extends MonadInstancesStd {
  def apply[F[_]](implicit F: TC[F, Monad]): Monad[F] = F.instance
}
