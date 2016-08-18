import java.io.{BufferedReader, FileReader}
import java.nio.file.Paths

/**
  * Author: Michal Szynkiewicz, michal.l.szynkiewicz@gmail.com
  * Date: 8/18/16
  * Time: 8:40 AM
  */
object JStackParser {

  val path = Paths.get("/home/michal/job/300threads-log.txt")

  def readEntry(in: BufferedReader) : Option[String] = {
    var line : String = null
    do {
      line = in.readLine()
    } while (line != null && line.trim.isEmpty)
    if (line == null) None
    else {
      val entryBuilder = new StringBuilder
      while (line != null && !line.trim.isEmpty) {
        entryBuilder.append(line).append("\n")
        line = in.readLine()
      }
      Some(entryBuilder.toString())
    }
  }

  def entryList(in: BufferedReader): List[String] = {
    val maybeEntry = readEntry(in)
    maybeEntry match {
      case None => Nil
      case Some(e) => e :: entryList(in)
    }
  }

  def read() : Filterable = {
    val in = new BufferedReader(new FileReader(path.toFile))
    Filterable(entryList(in))
  }

  def main(args : Array[String]): Unit = {
    val list: Filterable = read()
    val filtered: Filterable = list
      .dropContaining("nio")
      .dropContaining("buildQUeue")
      .dropContaining("parallelgc")
      .dropContaining("reference handler")
      .dropContaining("finalizer")
      .dropContaining("service thread")
      .dropContaining("signal dispatcher")
      .dropContaining("compilerthread")

//    println(s"entries without nio: $woNio")
    println(s"no of entries left: ${filtered.count}")

    println(s"buildQueueEntries: ${list.getContaining("buildqueue")}")
  }

}

case class Filterable(entries: List[String]) {

  def dropContaining(text : String) : Filterable = {
    println(s"count before dropping $text: $count")
    Filterable(entries.filter(t => !matchIgnoringCase(text, t)))
  }

  def getContaining(text: String) : Filterable = {
    Filterable(entries.filter(t => matchIgnoringCase(text, t)))
  }

  def count = entries.size

  private def matchIgnoringCase(text: String, t: String): Boolean = {
    t.toUpperCase().contains(text.toUpperCase)
  }
}
