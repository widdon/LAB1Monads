package monads

final case class IO[A](unsafeRun: () => A) {

  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())
}

object IO {

  def pure[A](value: A): IO[A] =
    IO(() => value)
  
  def delay[A](thunk: => A): IO[A] =
    IO(() => thunk)
  
  def readLine: IO[String] =
    IO(() => scala.io.StdIn.readLine())
  
  def printLine(line: String): IO[Unit] =
    IO(() => println(line))

  implicit val ioMonad: Monad[IO] =
    new Monad[IO] {

      override def pure[A](value: A): IO[A] =
        IO.pure(value)

      override def flatMap[A, B](ma: IO[A])(
        f: A => IO[B]
      ): IO[B] =
        ma.flatMap(f)
    }
}