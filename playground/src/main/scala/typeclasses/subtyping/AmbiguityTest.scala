package typeclasses.subtyping

object AmbiguityTest {

  def u[A, F[_] : Functor](fa: F[A]) = Functor[F].map(fa)(_ => ())

  /*
  Error:(7, 61) ambiguous implicit values:
    both value evidence$3 of type typeclasses.subtyping.Bind[F]
  and value evidence$2 of type typeclasses.subtyping.Applicative[F]
  match expected type typeclasses.subtyping.Functor[F]
  */
  //def x[A, F[_] : Applicative : Bind](fa: F[A]): F[Unit] = u(fa)

  def main(args: Array[String]): Unit = {

    println(u(List(1, 2, 3)))

    //println(x(List(1, 2, 3)))
  }

}
