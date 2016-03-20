package typeclasses.tc

object AmbiguityTest {

  def u[A, F[_]](fa: F[A])(implicit F: TC[F, Functor]) = Functor[F].map(fa)(_ => ())

  def x[A, F[_] : TC[?[_], Applicative] : TC[?[_], Bind]](fa: F[A]): F[Unit] = u(fa)

  def main(args: Array[String]): Unit = {

    println(u(List(1, 2, 3)))

    println(x(List(1, 2, 3)))
  }

}
