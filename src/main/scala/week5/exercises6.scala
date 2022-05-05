package week5

object exercises6 extends App {

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => y * y :: squareList(ys)
    }

  def squareListMap(xs: List[Int]): List[Int] =
    xs map (x => x * x)


  /**
   * Write a function pack that packs consecutive duplicates of list elements into sublists. For instance,
   */
  pack(List("a", "a", "a", "b", "c", "c", "a"))

  /**
   * should give
   */
  List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

  /**
   * You can use the following template:
   * @param xs
   * @tparam T
   * @return
   */
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }
  val data = List("a", "a", "a", "b", "c", "c", "a")
  pack(data)


  /**
   * Using pack, write a function encode that produces the run-length encoding of a list.
   * The idea is to encode n consecutive duplicates of an element x as a pair (x, n). For instance,
   */
  encode(List("a", "a", "a", "b", "c", "c", "a"))

  /**
   * should give
   */
  List(("a", 3), ("b", 1), ("c", 2), ("a", 1))

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  encode(data)
}

