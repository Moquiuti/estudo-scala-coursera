package week6

object prime extends App {

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
}
