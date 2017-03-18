package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // QUESTIONS:
  // what exactly occurs when I do t(), but wrap that in a () => thingy??
  def take(n: Int): Stream[A] = {
    def go(i: Int, as: Stream[A]): Stream[A] = as match {
      case Cons(h, t) if i < n => Cons(h, () => go(i + 1, t()))
      case _ => Empty
    }

    go(0, this)
  }

  // QUESTIONS:
  // Why does the answer have a separate case for n == 1?
  // Would the answer suffer from stack overflow?
  def takeAlt(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().takeAlt(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(as: Stream[A]): Stream[A] = as match {
      case Cons(h, t) if p(h()) => Cons(h, () => go(t()))
      case _ => Empty
    }

    go(this)
  }

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???

  // QUESTIONS:
  // why does type param [A] on method break things?
  // why can I not put () => Stream[A] as the type of as in go?
  def toList: List[A] = {
    def go(as: Stream[A]): List[A] = as match {
      case Empty => Nil
      case Cons(h, t) => h() :: go(t())
    }

    go(this)
  }

  def toListTailRecursive: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], list: List[A]): List[A] = stream match {
      case Empty => list
      case Cons(h, t) => go(t(), h() :: list)
    }

    go(this, List()).reverse
  }

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}