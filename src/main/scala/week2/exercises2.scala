package week2

object exercises2 extends App {
  def fatorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }
  println(fatorial(4))
}
