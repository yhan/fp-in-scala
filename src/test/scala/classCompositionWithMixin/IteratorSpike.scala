package classCompositionWithMixin

class IteratorSpike {
  def test {
    val richStringIter = new StringIterationExecutor
    richStringIter foreach println
  }
}


abstract class Iterator {
  type T

  def hasNext: Boolean

  def next(): T
}


trait IteratedExecutor extends Iterator {
  // Trait extends without implementing abstract methods
  // Defining a behaviour in addition to iteration

  def foreach(f: T => Unit): Unit = while (hasNext) f(next())
}


class StringIterator(s: String) extends Iterator {
  type T = Char
  private var i = 0

  def hasNext = i < s.length

  def next() = {
    val ch = s charAt i
    i += 1
    ch
  }
}

/*
  It seems that we "combine" the function of `StringIterator` and `RichIterator`
  `RichIterator` being a trait, which has an implementation.

   StringIterator iterate and yields element; RichIterator give
 */
class StringIterationExecutor extends StringIterator("Scala") with IteratedExecutor
