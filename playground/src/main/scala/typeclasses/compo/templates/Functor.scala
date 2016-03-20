package typeclasses.compo.templates


// type classes, no subtyping but composition:

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] {
  def functor: Functor[F]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

trait Applicative[F[_]] {
  def apply: Apply[F]

  def pure[A](a: A): F[A]
}

trait Bind[F[_]] {
  def apply: Apply[F]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] {
  def applicative: Applicative[F]

  def bind: Bind[F]
}

// Like in the TC wrapper design, avoiding subtyping allow us to take over the default implicits resolution mechanism.
// => the BH0 > BH1 > BH2 hierarchy is there only to order the implicits priority and affect the resolution.

trait BH0 extends BH1 {
  implicit def monadBind[F[_]](implicit F: Monad[F]): Bind[F] = F.bind

  implicit def monadApplicative[F[_]](implicit F: Monad[F]): Applicative[F] = F.applicative

  implicit def monadApply[F[_]](implicit F: Monad[F]): Apply[F] = F.applicative.apply

  implicit def monadFunctor[F[_]](implicit F: Monad[F]): Functor[F] = F.applicative.apply.functor
}

trait BH1 extends BH2 {
  implicit def applicativeApply[F[_]](implicit F: Applicative[F]): Apply[F] = F.apply

  implicit def applicativeFunctor[F[_]](implicit F: Applicative[F]): Functor[F] = F.apply.functor
}

trait BH2 {
  implicit def applyFunctor[F[_]](implicit F: Apply[F]): Functor[F] = F.functor
}

// Introduce template to simplify instance creation, template do use subtyping (better perf) but are never referenced as implicit

trait FunctorClass[F[_]] extends Functor[F]{
  implicit final def functor: Functor[F] = this
}

trait ApplyClass[F[_]] extends Apply[F] with FunctorClass[F] {
  implicit final def apply: Apply[F] = this
}

trait BindClass[F[_]] extends Bind[F] with ApplyClass[F] {
  implicit final def bind: Bind[F] = this

  // derive default implementation!!
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(functor.map(fa))
}

trait ApplicativeClass[F[_]] extends Applicative[F] with ApplyClass[F] {
  implicit final def applicative: Applicative[F] = this
  // derive default implementation!!
  override def map[A, B](fa: F[A])(f: (A) => B): F[B] = apply.ap(fa)(pure(f))
}

trait MonadClass[F[_]] extends Monad[F] with BindClass[F] with ApplicativeClass[F] {
  implicit final def monad: Monad[F] = this

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] = bind.flatMap(fa)(a => applicative.pure(f(a)))
}

// Instances, now very simple:

trait MonadInstancesStd {

  // nice minimal definition
   val list: Monad[List] = new MonadClass[List] {
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def pure[A](a: A): List[A] = List(a)
  }

  // or full definition
  implicit val listFull: Monad[List] = new MonadClass[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B] = xs.flatMap(a => f.map(_ (a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    override def pure[A](a: A): List[A] = List(a)
  }

}

// Companion objects

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}

object Apply {
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F
}


object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}

object Bind {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
}

object Monad extends MonadInstancesStd {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}
