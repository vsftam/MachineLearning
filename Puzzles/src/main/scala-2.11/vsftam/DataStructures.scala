package vsftam
/**
  * Created by vincenttam on 2/25/17.
  */
object MyList {

  trait List[+T]

  case object Nil extends List[Nothing]

  case class Cons[+T](h: T, t: List[T]) extends List[T]

  def apply[T](l: T*): List[T] =
    if(l.isEmpty) Nil
    else Cons(l.head, apply(l.tail : _*)) // adapt a sequence for a vararg field

  def append[T](a: List[T], b: List[T]): List[T] = a match {
    case Nil => b
    case Cons(h, t) => Cons(h, append(t, b))
  }
}

object ListNode {

  case class ListNode[+T](h: T, t: ListNode[T]) {
    def head: T = h
    def tail: ListNode[T] = t
    def prepend[U >: T](elem: U): ListNode[U] = {
      ListNode(elem, this)
    }
  }

  val empty: ListNode[Null] = ListNode(null, null)

  def main(args: Array[String]): Unit = {
    val a: ListNode[String] = empty.prepend("Apple").prepend("Orange")
    val b: ListNode[Any] = a.prepend(123)
  }
}

object MyTree {

  trait Tree[+T]
  case class Leaf[T](e: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  def size[T](t: Tree[T]) : Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(e) => e
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[T](t: Tree[T]) : Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(zero: A => B)(combine: (B, B) => B): B = t match {
    case Leaf(a) => zero(a)
    case Branch(l, r) => combine(fold(l)(zero)(combine), fold(r)(zero)(combine))
  }

  def sizeAsFold[T](t: Tree[T]): Int = fold(t)(_ => 1)(_ + _)

  def maximumAsFold(t: Tree[Int]): Int = fold[Int, Int](t)(e => e)(_ max _)

  def depthAsFold[T](t: Tree[T]): Int = fold(t)(_ => 0)(_ max _ + 1)

  // Scala fails to infer Left[B] as Tree[B]
  def mapAsFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def main(args: Array[String]): Unit = {
    val l1 = Leaf(1)
    val l2 = Leaf(2)
    val l = Branch(l1, l2)
    val la = Leaf("A")
    val lx: Tree[Any] = Branch(l, la)
    println( "Size for lx is " + size(lx))
  }
}
