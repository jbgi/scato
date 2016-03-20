package typeclasses.compo

object AmbiguityTest {

  def u[A, F[_] : Functor](fa: F[A]) = Functor[F].map(fa)(_ => ())

  def x[A, F[_] : Applicative : Bind](fa: F[A]): F[Unit] = u(fa)

  def main(args: Array[String]): Unit = {

    println(u(List(1, 2, 3)))

    println(x(List(1, 2, 3)))
  }

}
