package watcha


/**
  * Created by pierre on 5/14/17.
  */
case class Node(override val ID: Option[String], attr: Map[String, String], key: String, nodeType: DataType) extends Data with Identifiable{

  override def toString: String = {
    nodeType.attribs.map(_._1).map {a => attr(a)} mkString " ; "
  }

}


object Node {
  import fastparse.all._
  import ParsingCommon._

  val Line = P(Word.rep(sep=";") ~ EOL)
  val File = P( Start ~ EOL.rep ~ Line.rep ~ EOL.rep ~ End)

  def parse(nodeType: DataType, input: String): Seq[Node] = {
    File.parse(input) match {
      case Parsed.Success(data, _) =>
        val attrKeys = nodeType.attribs.map(_._1)
        for (row <- data) yield {
          require(row.length == attrKeys.length) // todo: error reporting
          val attrs = attrKeys.zip(row).toMap
          val key = nodeType.key.map(attrs) mkString ":"
          Node(None, attrs, key, nodeType)
        }
      case Parsed.Failure(_, _, _) => Nil
    }
  }
}
