package category

object CategoryForProgrammers {

  trait Monoid[A] {
    def empty: A
    def append: A => A => A
  }

  trait MonoidString extends Monoid[String] {
    def empty: String = ""
    def append: String => String => String = (l: String) => (r: String) => l + r
  }

  //Kleisli Categories
  //Writer Monad
  type Writer[A] = (A, String)

  def negate(b: Boolean): Writer[Boolean] = !b -> "Not so! "

  def isEven(n: Int): Writer[Boolean] = (n % 2 == 0) -> "isEven "

  def isOdd(n: Int): Writer[Boolean] = {
    val p1 = isEven(n)
    val p2 = negate(p1._1)
    p2._1 -> (p1._2 + p2._2)
  }

  def isOddCompose(n: Int): Writer[Boolean] =
    compose(isEven,negate)(n)

  def compose[A,B,C](m1: A => Writer[B],
                     m2: B => Writer[C]): A => Writer[C] =
    (a: A) => {
      val p1 = m1(a)
      val p2 = m2(p1._1)
      p2._1 -> (p1._2 + p2._2)
    }

  def writerId[A](a: A): Writer[A] = a -> ""

  trait WriterMonoid extends Monoid[Writer[_]] {
//    override def empty: Writer[_] = ???
//    override def append: Writer
  }

  //Initial Object:
  //The initial object is the object that has ONE and
  //only ONE morphism going to any object in the category.
  //
  //The start of a category

  type Absurd[A] = () => A

  //Terminal Object:
  //The terminal object is the object with ONE and
  //only ONE morphism coming to it from any object in the category.
  //
  //The end of a category

  type UnitC[A] = A => Unit

  //Duality:
  //Every category has dual category with all the arrows reversed, the dual category
  //has the Initial and Terminal objects reversed, if you reverse the reverse you get what you started with.
  //`co` is used to denote a categories opposite category
  //Ex. coproudct,comonad

  //Product:
  //A product of two objects a and b is the object c equipped with two projections
  // such that for any other object c’ equipped with two projections there is
  // a unique morphism m from c’ to c that factorizes those projections.

//  type FST[A,B] = (A,B) => A
//  type SND[A,B] = (A,B) => B

  def fst[A,B](a: A,b: B): A = a
  def snd[A,B](a: A,b: B): B = b


//  type FSTW[A] = (A,_) => A
//  type SNDW[B] = (_,B) => B


}
