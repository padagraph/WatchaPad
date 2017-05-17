package watcha

/**
  * Created by pierre on 5/14/17.
  */
case class Edge(override val ID: Option[String], sourceKey: String, targetKey: String, attr: Map[String, String], key: String, edgeType: DataType) extends Data with Identifiable {
  override def toString: String = {
    (sourceKey :: targetKey ::  edgeType.attribs.map(_._1).map{a => attr(a)}) mkString ";"
  }
}

object Edge {

  import fastparse.all._
  import ParsingCommon._

  val Line = P(Word.rep(2, sep = ";") ~ EOL)
  val File = P(Start ~ EOL.rep ~ Line.rep ~ EOL.rep ~ End)

  def parse(edgeType: DataType, input: String): Seq[Edge] = {
    File.parse(input) match {
      case Parsed.Success(data, _) =>
        val attrKeys = edgeType.attribs.map(_._1)
        for (row <- data) yield {
          require(row.length == attrKeys.length + 2) // todo: error reporting
          val src :: tgt :: values = row.toList
          val attrs = attrKeys.zip(values).toMap
          val key = edgeType.key.map(attrs) mkString ":"
          Edge(None, src, tgt, attrs, key, edgeType)
        }
      case Parsed.Failure(_, _, _) => Nil
    }
  }
}