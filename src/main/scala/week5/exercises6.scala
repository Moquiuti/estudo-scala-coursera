package week5

object exercises6 extends App {

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => y * y :: squareList(ys)
    }

  def squareListMap(xs: List[Int]): List[Int] =
    xs map (x => x * x)
}
