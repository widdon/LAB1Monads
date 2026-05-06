package monads

final case class State[S, A](run: S => (S, A)) {

  def map[B](f: A => B): State[S, B] =
    State { state =>
      val (newState, value) = run(state)
      (newState, f(value))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { state =>
      val (newState, value) = run(state)
      f(value).run(newState)
    }
}

object State {

  def pure[S, A](value: A): State[S, A] =
    State(state => (state, value))
  
  def get[S]: State[S, S] =
    State(state => (state, state))
  
  def set[S](newState: S): State[S, Unit] =
    State(_ => (newState, ()))
  
  def modify[S](f: S => S): State[S, Unit] =
    State(state => {
      val updated = f(state)
      (updated, ())
    })

  implicit def stateMonad[S]: Monad[({ type L[A] = State[S, A] })#L] =
    new Monad[({ type L[A] = State[S, A] })#L] {

      override def pure[A](value: A): State[S, A] =
        State.pure(value)

      override def flatMap[A, B](ma: State[S, A])(
        f: A => State[S, B]
      ): State[S, B] =
        ma.flatMap(f)
    }
}