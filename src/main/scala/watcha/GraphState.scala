package watcha

import java.util.Date

import io.padagraph.client.NodeOrEdge
import watcha.DataType.{EdgeType, NodeType}

import scala.io.Source
import io.padagraph.{client => pdg}

/**
  * Created by pierre on 5/14/17.
  */
case class GraphState(types: Map[String, DataType], data: Map[DataType, Seq[Data]], timestamp: Date) extends Serializable {

  override def toString: String = {
    s"""
    | ${types.values.map(_.toString()) mkString("\n\n")}
    | ${data.map {case (t,d) => s"${t.name}\n${d.map(_.toString) mkString "\n"}\n"} mkString }
    """.stripMargin
  }


  def buildPDGraph(): pdg.Graph[pdg.Node, pdg.Edge[pdg.Node, pdg.Node]] = {

    val pdgTypes = types.map { case (name, dt) =>
      val T = dt.typeOf match {
        case DataType.NodeType =>
          object T extends pdg.DataType[pdg.Node] (factory = (_, _) => new N () ) {
            override val name: String = name
          }
          class N extends pdg.Node () {
            override var dataType: pdg.DataType[NodeOrEdge] = T
          }
          T
        case DataType.EdgeType =>
          object T extends pdg.DataType[pdg.Edge[pdg.Node, pdg.Node]](factory = {case (Some(src), Some(tgt)) => new E(src,tgt)}) {
            override val name: String = name
          }

          class E(src: pdg.Node, tgt: pdg.Node) extends pdg.Edge[pdg.Node, pdg.Node] {
            override val source: pdg.Node = src
            override val target: pdg.Node = tgt
            override var dataType: pdg.DataType[NodeOrEdge] = T
          }
          T
      }
      name -> (T)
    }

    val pdgNodes: Map[String, pdg.Node] =
      data.filter {case (dt, _) => dt.typeOf == DataType.NodeType}
        .flatMap { case (dt, nodes) =>
          val pdgT = pdgTypes(dt.name)
          nodes.collect { case n: Node =>
            val node = pdgT.factory(None, None).asInstanceOf[pdg.Node]
            n.attr.foreach { case (k, v) => pdg.Text.set(node.properties, k, v) }
            n.key -> node
          }
        }

    val pdgEdges: Map[String, pdg.Edge[pdg.Node, pdg.Node]] =
      data.filter {case (dt,_) => dt.typeOf == DataType.EdgeType}
        .flatMap { case (dt, edges) =>
          val pdgT = pdgTypes(dt.name)
          edges.collect {case e: Edge =>
              val src = pdgNodes(e.sourceKey)
              val tgt = pdgNodes(e.targetKey)
              val edge = pdgT.factory(Some(src), Some(tgt)).asInstanceOf[pdg.Edge[pdg.Node,pdg.Node]]
              e.attr.foreach {case (k,v) => pdg.Text.set(edge.properties, k, v)}
              e.key -> edge
          }
        }



    object PDG extends pdg.Graph[pdg.Node, pdg.Edge[pdg.Node, pdg.Node]] {
      override val name: String = "faudraitUnNom"
      override val owner: String = "pierre"
      override val description = "not implemented"
      override val tags = Array("social", "peoples", "friendship")
      override val image = ""
      override val nodes:Set[pdg.Node] = pdgNodes.values.toSet
      override val edges:Set[pdg.Edge[pdg.Node, pdg.Node]] = pdgEdges.values.toSet
      rebuildTypeMappings()
    }

    PDG
  }

}


object GraphState {
  def fromURL(url: String): GraphState = {
    println(s"fetching $url ...")
    val pad = Source.fromURL(url) mkString ""
    val schema = DataType.parse(pad)
    val data = for( t <- schema) yield {
      val calc = Source.fromURL(t.url) mkString ""
      t -> (t.typeOf match {
        case NodeType => Node.parse(t, calc)
        case EdgeType => Edge.parse(t, calc)
      })
    }
    new GraphState(schema.map(t => t.name -> t).toMap, data.toMap, new Date())
  }
}