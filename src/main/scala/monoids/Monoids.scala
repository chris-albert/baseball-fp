package monoids

import scala.concurrent.Future

object Monoids {

  case class Box[A](value: A)

  object Box {
    //Category Theory
    //http://alissapajer.github.io/conferenceslides/craftconf2014/#/

    //Category: Types + Functions + Laws = Category
    //Laws
    // 1. Function composition is associative
    // 2. For every type, we have an identity function A => A
    def id[A](a: A): A = a

    //Functor: with a function A => B you can create a function of Box[A] => Box[B]
    def map[A,B](f: A => B): Box[A] => Box[B] = (a: Box[A]) => Box(f(a.value))

    //Monad: with a function A => Box[B] you can create a function of Box[A] => Box[B]
    def flatMap[A,B](f: A => Box[B]): Box[A] => Box[B] = (a: Box[A]) => f(a.value)

    //Applicative: with a function Box[A => B] you can create a function Box[A] => Box[B]
    def apply[A,B](b: Box[A => B]): Box[A] => Box[B] = (a: Box[A]) => Box(b.value(a.value))

    //Endofunctors: Transforms a Category into itself
    //A becomes Box[A]: The constructor of Box
    //A => B becomes Box[A] => Box[B]: The map method of Box

    //Functor: Transforms one category into another
    //Functor: Category + Transformations + Laws = Functor
    //Functor law 1: Functors preserve the identity function
    def functorLaw1 = {
      val box: Box[Int] = Box(1)
      val a: Box[Int] = map(id[Int])(box)
      val b: Box[Int] = id[Box[Int]](box)
      assert(a == b)
    }

    //Functor law 2: Functors preserve functions composition
    def functorLaw2 = {
      val box: Box[Int] = Box(1)
      def incr: Int => Double = (a: Int) => a + 0.5
      def pos: Double => Boolean = (d: Double) => d > 0

      val a: Box[Boolean] = map(incr andThen pos)(box)
      val b: Box[Boolean] = (map(incr) andThen map(pos))(box)
      assert(a == b)
    }
  }

  def main(args: Array[String]): Unit = {
    Box.functorLaw1
    Box.functorLaw2
  }
}
