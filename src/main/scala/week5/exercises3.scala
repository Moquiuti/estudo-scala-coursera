package week5

object exercises3 extends App {

  /**
   * The merge function as given uses a nested pattern match.
   * This does not reflect the inherent symmetry of the merge algorithm.
   * Rewrite merge using a pattern matching over pairs.
   * Ordenação
   * @param xs
   * @param ys
   * @return
   */
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana", "avocado")

  println(msort(nums)((x: Int, y: Int) => x < y))
  println(msort(nums)((x, y) => x < y))
  println(msort(fruits)((x: String, y: String) => x.compareTo(y) < 0))
}
