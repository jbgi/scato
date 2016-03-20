package typeclasses.tc

class TC[T[_], C[_[_]]](val instance: C[T]) extends AnyVal

object TC {
  def apply[T[_], C[_[_]]](i: C[T]): TC[T, C] = new TC(i)
}