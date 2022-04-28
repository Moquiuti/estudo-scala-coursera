package week2

object exercises extends App {
  def fatorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }
  println(fatorial(4))

  /**
   * Função de ordem superior como blocos de contrução essenciais de programas funcionais.
   * @param f
   * @param a
   * @param b
   * @return
   */
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
  println(sum(x => x * x, 3, 5))
}
