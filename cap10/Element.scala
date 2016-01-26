import Element.elem

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length

  def width: Int = contents(0).length

  def above(that: Element): Element =  {
    val this1 = this widen that.width
    var that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height

    elem(
      for (
           (line1, line2) <- this1.contents zip that1.contents
         ) yield line1 + line2
       )
  }

  def widen(w: Int): Element =
    if(w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, height)
      var right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }

  def heighten(h:Int): Element =
    if(h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }

  override def toString = contents mkString "\n"
}

object Element {
  //private classes
  private class ArrayElement(
    val contents: Array[String]
  ) extends Element

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override def width = s.length
    override def height = 1
  }

  private class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int
  ) extends Element {
    private val line = ch.toString * width
    //def contents = Array.make(height, line)
    def contents = Array.fill(height)(line)
  }
  //factory methods
  def elem(contents: Array[String]): Element = 
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element =
    new LineElement(line)
}
