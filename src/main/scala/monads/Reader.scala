package monads

final case class Reader[Env, A](run: Env => A) {

  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))
}

object Reader {

  def pure[Env, A](value: A): Reader[Env, A] =
    Reader(_ => value)

  // Получение всего окружение
  def ask[Env]: Reader[Env, Env] =
    Reader(env => env)

  // И его локальное преображение
  def local[Env, A](reader: Reader[Env, A])(modify: Env => Env): Reader[Env, A] =
    Reader(env => reader.run(modify(env)))

  implicit def readerMonad[Env]: Monad[({ type L[A] = Reader[Env, A] })#L] =
    new Monad[({ type L[A] = Reader[Env, A] })#L] {

      override def pure[A](value: A): Reader[Env, A] =
        Reader.pure(value)

      override def flatMap[A, B](ma: Reader[Env, A])(
        f: A => Reader[Env, B]
      ): Reader[Env, B] =
        ma.flatMap(f)
    }
}