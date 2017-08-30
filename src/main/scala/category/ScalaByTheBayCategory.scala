package category

object ScalaByTheBayCategory {

  def main(args: Array[String]) = {
    functorIdentity()
  }

  case class Box[A](value: A)

  trait Functor2[F[_]] {
    def map[A, B](f : A => B): F[A] => F[B]
  }

  trait F[A] {
    def map[B](f: A => B): F[B]
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(ab: A => B): F[B]
  }

  val boxFunctor = new Functor[Box] {
    override def map[A, B](fa: Box[A])
                          (ab: (A) => B): Box[B] = Box(ab(fa.value))
  }

  //Functor Laws

  def functorIdentity() = {
    val box1 = Box(1)
    assert(boxFunctor.map(box1)(identity) == box1)
  }

  def functorComposition() = {

  }


  trait Applicative

  //Applicative laws


  trait Monad

  //Monad Laws
}
