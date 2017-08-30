package category

object CategoryMain {


  def main(args: Array[String]) = {
    println("Category Theory Playground")
    Box.categoryLaw1
    Box.functorLaw1
    Box.functorLaw2
  }

  case class Box[A](value: A)

  object Box {
    //Category Theory
    //http://alissapajer.github.io/conferenceslides/craftconf2014/#/

    //Category: Types + Functions + Laws = Category
    //Laws
    // 1. Function composition is associative
    def categoryLaw1 = {
      def incr: Int => Double = (a: Int) => a + 0.5
      def pos: Double => Boolean = (d: Double) => d > 0
      def ts: Boolean => String = (b: Boolean) => b.toString

      def left[Int,String] = incr andThen (pos andThen ts)
      def right[Int,String] = (incr andThen pos) andThen ts
      assert(left == right)
    }
    // 2. For every type, we have an identity function A => A
    def id[A](a: A): A = a

    //Functor: with a function A => B you can create a function of Box[A] => Box[B]
    def map[A,B](f: A => B): Box[A] => Box[B] = (a: Box[A]) => Box(f(a.value))

    //Monad: with a function A => Box[B] you can create a function of Box[A] => Box[B]
    //A Monad must have the functions id and flatMap
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


    //Monoid: Follows the monoid rules
    //Things and ways to combine them, so like Int and addition and multiplication, not Int and subtraction
    //Monoid law 1: Closure - the result of combining 2 things is always another one of the things
    def monoidLaw1 = {
      val r = 1 + 2 + 3
      assert(r.isInstanceOf[Int])
      //since Int + Int is an Int
    }
    //Monoid law 2: Associativity - when combining more than 2 things,
    // which pairwise combination you do first doesn't matter
    def monoidLaw2 = {
      val a = (1 + 2) + 3
      val b = 1 + (2 + 3)
      assert(a == b)
    }
    //Monoid law 3: Identity - There is a special thing called zero, such that when you
    //combine any thing with zero you get the original back
    def monoidLaw3 = {
      val a = 1 + 0
      assert(a == 1)
      val b = 1 * 1
      assert(b == 1)
    }

    //Semigroups: A monoid that does not follow the identity rule
  }

  trait Functor[F[_]] {
    def map[A,B](f: A => B): F[A] => F[B]
  }



  trait GenericCategory[->>[_,_]] {
    def id[A]: A ->> A
    def compose[A, B, C](g: B ->> C, f: A ->> B): A ->> C
  }

  object Category extends GenericCategory[Function] {
    def id[A]: A => A = a => a
    def compose[A,B,C](g: B => C,f: A => B): A => C =
      g compose f
  }
}
