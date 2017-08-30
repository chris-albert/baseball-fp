package monoids

import category.CategoryMain.Functor


object FreeMonad {

  sealed trait Free[F[_],A]

  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Suspend[F[_],A](s: F[Free[F,A]]) extends Free[F,A]

  //The Yoneda Lamma

  abstract class Yoneda[F[_],A] {
    def apply[B](f: A => B): F[B]
  }

  def toYoneda[F[_]: Functor, A](fa: F[A]) =
    new Yoneda[F,A] {
      def apply[B](f: A =>B) = ??? // Functor[F].map(fa)(f)
    }

  def fromYoneda[F[_],A](yo: Yoneda[F,A]) = yo.apply(identity)

  sealed abstract class Coyoneda[F[_],A] {
    type I
    val fi: F[I]
    val k: I => A
  }

  //More monads for understanding
  //Generic type function
  def f[A]: A => A = ???
  def g[A]: A => A = ???
  //to combine f and g
  val fFirst = f(g(1))
  val gFirst = g(f(1))
  //or more genericly
  def h[A]: A => A = f compose g
  //Now ^^ is monoid, it's just function composition
}
