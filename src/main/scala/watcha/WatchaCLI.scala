package watcha

import java.util.Date

import scala.io.Source

/**
  * Created by pierre on 5/15/17.
  */
object WatchaCLI {

  def main(args: Array[String]): Unit = {
    val url = "file:///tmp/pad.txt" //args(1)
    val g = GraphState.fromURL(url)
    println(g.toString)

  }
}
