package week4

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

// a diferença entre val e def é que val é avaliada quando o método é inicializado pela primeira vez
// e def é avaliado cada vez que é referenciado
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

/**
 * Define an object List{...} with 3 functions in it so that users can create lists of length 0-2 using syntax
 * List() //the empty list
 * List(1) //the list with single element 1.
 * List(2, 3) // the list with elements 2 and 3
 * */
object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T]() = new Nil
}