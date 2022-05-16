package week6

object pairs extends App {
  /**
   *  Então a tarefa é que, queremos encontrar todos os pares de inteiros positivos i e j,
   *  de tal forma que j é menor que i e i é limitado por algum inteiro positivo n e i + j é primo.
   *  Então, por exemplo, se n = 7, os pares que queremos encontrar são 2 1, 3 2, 4 1, 4 3,
   *  e assim por diante, porque a soma de i e j em cada caso é um número primo.
   *  Então, na linguagem de programação imperativa eu provavelmente usaria dois loops aninhados,
   *  um para o Is um para os Js juntamente com um teste se a soma de i mais j é um número primo
   *  e o buffer de soma para coletar os resultados. Mas o que seria uma maneira puramente funcional de
   *  alcançar a mesma coisa? Então, uma maneira funcional natural de fazer isso seria gerar estruturas
   *  de dados bit a bit até que tenhamos gerado a estrutura de dados que precisamos para o resultado final.
   *  Assim, a primeira estrutura de dados que queremos gerar é a sequência de todos os pares de inteiros i, j.
   *  tal que o j e o i estão dentro dos limites que nós especificamos.
   *  Reproduza o vídeo começando em :1:40 e siga a transcrição
   *  E então, uma vez que temos essa sequência, podemos filtrá-la para manter apenas os pares para os
   *  quais a soma i+j é prime, e então estamos prontos.
   *  Reproduza o vídeo começando em :1:51 e siga a transcrição
   *  Então ficamos com o problema de como gerar a sequência de pares de inteiros.
   *  E lá a maneira natural de fazer isso seria gerar primeiro os inteiros i entre 1 e n, n excluídos.
   *  E então para cada inteiro i, gere a lista de pares (i, 1), (i, 2) e assim por diante até i, i menos 1.
   *  Assim que tivermos isso, podemos colocá-lo em código. O último bit pode ser alcançado combinando até e mapa.
   *  Então vamos tentar 1 até n que nos dá uma lista de i de 1 a n excluídos.
   *  E, em seguida, para cada um desses inteiros, nós mapeá-lo, chamá-lo de i para um intervalo
   *  que vai de um até i, i excluído. Chame o índice aqui, j e para cada combinação de i e j, que
   *  produzimos dessa forma, retornamos o par i e j.
   * @param n
   * @return
   */
  def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

  val n = 7

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

  /**
   * write a version of scalaProduct (see last version) that makes use
   * of a for:
   * @param xs
   * @param ys
   * @return
   */
  def scalaProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((y, x) <- xs zip ys) yield x * y).sum
}
