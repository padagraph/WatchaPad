import fastparse.all._

/**
  * Created by pierre on 5/14/17.
  */
package object watcha {
  abstract class Data

  object ParsingCommon {
    val allowedChars = "[a-z -]".r
    val urlRE =  "(https?|file)://".r

    val EOL = P("\n")
    val IsKey = P("#".!.?.map(_.nonEmpty))
    val Word = P(CharsWhile(!";:#\n".contains(_)).!)
    val CellContent = P( IsKey ~ Word.!)
    val URL = P(CharsWhile(!" \n".contains(_)).!).filter(urlRE.findPrefixOf(_).nonEmpty)
  }

}
