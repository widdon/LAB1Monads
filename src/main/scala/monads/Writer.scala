package monads

final case class Writer[Log, A](log: Log, value: A) {

  def map[B](f: A => B): Writer[Log, B] =
    Writer(log, f(value))

  def flatMap[B](f: A => Writer[Log, B])(implicit
                                         monoid: Monoid[Log]
  ): Writer[Log, B] = {
    val next = f(value)
    Writer(
      monoid.combine(log, next.log),
      next.value
    )
  }
}


object Writer {

  def pure[Log: Monoid, A](value: A): Writer[Log, A] =
    Writer(implicitly[Monoid[Log]].empty, value)

  // Добавит запись в лог, без значения (UNIT)
  def tell[Log](log: Log): Writer[Log, Unit] =
    Writer(log, ())

  implicit def writerMonad[Log: Monoid]: Monad[({ type L[A] = Writer[Log, A] })#L] =
    new Monad[({ type L[A] = Writer[Log, A] })#L] {

      override def pure[A](value: A): Writer[Log, A] =
        Writer.pure(value)

      override def flatMap[A, B](ma: Writer[Log, A])(
        f: A => Writer[Log, B]
      ): Writer[Log, B] =
        ma.flatMap(f)
    }
}