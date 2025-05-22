package graphgen

import scala.util.Try
import java.io.{File, FileWriter}

object Writers:

  // Формирует строку в формате GraphML
  private object StringGraphMLWriter extends GraphMLWriter[String]:
    def write(graph: Graph, directed: Boolean): String =
      val header =
        """<?xml version="1.0" encoding="UTF-8"?>
          |<graphml xmlns="http://graphml.graphdrawing.org/xmlns">""".stripMargin

      // Устанавливаем тип графа: directed / undirected
      val graphOpen = s"""<graph id="G" edgedefault="${if directed then "directed" else "undirected"}">"""

      // Формируем XML-узлы
      val nodesXml = graph.nodes.toSeq.sorted.map(n => s"""  <node id="n$n"/>""").mkString("\n")

      // Формируем XML-рёбра с опциональными весами
      val edgesXml = graph.edges.toSeq.map { e =>
        val edgeTag = s"""  <edge source="n${e.u}" target="n${e.v}">"""
        val weight = graph.weights.get(e).map(w => s"""    <data key="weight">$w</data>""").getOrElse("")
        s"$edgeTag\n$weight\n  </edge>"
      }.mkString("\n")

      val footer = "</graph>\n</graphml>"
      s"$header\n$graphOpen\n$nodesXml\n$edgesXml\n$footer"

  // Сохраняет граф в файл output.graphml
  object FileGraphMLWriter extends GraphMLWriter[Try[Unit]]:
    def write(graph: Graph, directed: Boolean): Try[Unit] =
      Try {
        val xml = StringGraphMLWriter.write(graph, directed)
        val writer = FileWriter(File("output.graphml"))
        try writer.write(xml)
        finally writer.close()
      }
