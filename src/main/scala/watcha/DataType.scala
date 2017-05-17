package watcha

import fastparse.core.Parsed.Success
import watcha.DataType.{EdgeType, NodeType, TypeOf}

/**
  * Created by pierre on 5/14/17.
  */
case class DataType(override val ID: Option[String], typeOf: TypeOf, name: String, attribs: List[(String, String)], key: List[String], url: String ) extends Identifiable {

  override def toString: String = {
    val typeChar = typeOf match {
      case EdgeType => "_"
      case NodeType => "@"
    }
    s"""
       |$typeChar $name
       |${attribs.map(_._1) mkString ";"}
       |$url
     """.stripMargin
  }


}

object DataType {

  sealed abstract class TypeOf
  case object NodeType extends TypeOf
  case object EdgeType extends TypeOf

  import fastparse.all._
  import ParsingCommon._

  val N:Parser[TypeOf] = P("@".!.map(_ => NodeType))
  val E:Parser[TypeOf] = P("_".!.map(_ => EdgeType))
  val T = P( N | E)
  val DefLine = P( CellContent.rep(sep=";") ~ EOL).opaque("DefLine")
  val Type = P( T ~ Word ~ EOL ~ DefLine ~ URL)
  val File = P(Start ~ EOL.rep ~ Type.rep(sep=EOL.rep(1)) ~ EOL.rep ~ End )

  def parse(input:String): List[DataType]= {
    File.parse(input) match {
      case Parsed.Success(seq, i) =>
        println(i)
        seq.map { case (t, name, cells, url) =>
          val attribs = cells.map(_._2).map(_.trim -> "Text").toList
          val keys = cells.filter(_._1).map(_._2.trim).toList
          require(attribs.map(_._1).toSet.size == attribs.size) // todo: better error reporting on non uniq key
          DataType(None, t, name, attribs, keys, url.trim)
        } .toList
      case Parsed.Failure(a,b,c) =>
        println(input.take(b))
        println("X")
        println(input.drop(b))
        println(a,b,c)
        Nil
    }
  }


  val testCase =
    """
      |monType
      |#a;#b ;c;d
      |http://www.bubu.com/
      |monType2
      |#a;#b ;c;d
      |http://www.bubu2.com/
      """.stripMargin.trim

}
