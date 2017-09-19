object PlayGround extends App {

  val a = List(1,2,3)
  val b = List(4,5)
  val c = List(6,7,8)
  val abc = List(a,b,c)

  def loop(accu: List[List[Int]], input: List[List[Int]]): List[List[Int]] = {
    ???
  }

  def combine(a: Int, list: List[Int]): List[List[Int]] =
    list.map(List(a,_))

  def combine(aList: List[Int], bList: List[Int]): List[List[Int]] =
    aList.flatMap(combine(_,bList))

  def combine(lists: List[List[Int]]): List[List[Int]] = {

  }

//  val result = loop(List(List()),List(a,b,c))
//  println(result)
  println(combine(abc))
}
