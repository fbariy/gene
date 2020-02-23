package gene

case class State[S, A](run: S => (S, A)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (newS, a) = run(s)
      f(a).run(newS)
    }

  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }

  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- s
    } yield f(a, b)
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State { s => (s, a) }

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = states match {
    case List() => unit(List())
    case a :: rest => a.map2(sequence(rest))((a, b) => b :+ a)
  }
}