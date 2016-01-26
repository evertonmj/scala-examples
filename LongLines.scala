import scala.io.Source

object LongLines {
  def processFile(filename: String, width: Int) {
    def processLine(line: String) {
      if(line.length > width)
        print(filename + ": " + line)
    }
    val source = Source.fromFile(filename)

    for (line <- source.getLines)
      processLine(line)
  }
}

