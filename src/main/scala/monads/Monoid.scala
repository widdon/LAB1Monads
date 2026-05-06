package monads

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {

  implicit val vectorStringMonoid: Monoid[Vector[String]] =
    new Monoid[Vector[String]] {

      override def empty: Vector[String] =
        Vector.empty

      override def combine(
                            x: Vector[String],
                            y: Vector[String]
                          ): Vector[String] =
        x ++ y
    }

  implicit val listStringMonoid: Monoid[List[String]] =
    new Monoid[List[String]] {

      override def empty: List[String] =
        List.empty

      override def combine(
                            x: List[String],
                            y: List[String]
                          ): List[String] =
        x ++ y
    }
}