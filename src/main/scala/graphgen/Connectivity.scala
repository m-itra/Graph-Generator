package graphgen

import scala.annotation.tailrec
import scala.util.Random

object Connectivity {

  // Матрица смежности
  private def buildAdjMatrix(nodes: Seq[Int], edges: Set[Edge]): Vector[Vector[Boolean]] = {
    val indexOf = nodes.zipWithIndex.toMap
    val size = nodes.size

    edges.foldLeft(Vector.fill(size, size)(false)) {
      case (matrix, Edge(u, v)) =>
        val i = indexOf(u)
        val j = indexOf(v)
        matrix
          .updated(i, matrix(i).updated(j, true))
          .updated(j, matrix(j).updated(i, true))
    }
  }

  // Обход матрицы
  @tailrec
  private def bfsMatrix(
                         matrix: Vector[Vector[Boolean]],
                         visited: Set[Int] = Set.empty,
                         toVisit: List[Int] = Nil  // Исправлено имя параметра с queue на toVisit
                       ): Set[Int] = toVisit match {
    case Nil => visited
    case head :: tail if visited(head) =>
      bfsMatrix(matrix, visited, tail)
    case head :: tail =>
      val neighbors = matrix(head).indices
        .filter(i => matrix(head)(i) && !visited(i))
      bfsMatrix(matrix, visited + head, tail ++ neighbors)
  }

  private def isConnected(g: Graph): Boolean = {
    val sortedNodes = g.nodes.toList.sorted
    sortedNodes.isEmpty || {
      val matrix = buildAdjMatrix(sortedNodes, g.edges)
      bfsMatrix(matrix, toVisit = List(0)).size == sortedNodes.size
    }
  }

  // Ищем связные цепочки
  private def findComponents(g: Graph): List[Set[Int]] = {
    val sortedNodes = g.nodes.toList.sorted
    val indexOf = sortedNodes.zipWithIndex.toMap
    val matrix = buildAdjMatrix(sortedNodes, g.edges)

    @tailrec
    def discover(
                  unvisited: List[Int],
                  visited: Set[Int] = Set.empty,
                  components: List[Set[Int]] = Nil
                ): List[Set[Int]] = unvisited match {
      case Nil => components
      case node :: rest if visited(node) =>
        discover(rest, visited, components)
      case node :: rest =>
        val idx = indexOf(node)
        val compIndices = bfsMatrix(matrix, toVisit = List(idx))
        val component = compIndices.map(sortedNodes)
        discover(rest, visited ++ component, component :: components)
    }

    discover(sortedNodes)
  }

  private def randomWeight(minW: Double, maxW: Double, rng: Random): Double =
    BigDecimal(minW + (maxW - minW) * rng.nextDouble())
      .setScale(3, BigDecimal.RoundingMode.HALF_UP)
      .toDouble

  private def updateWeights(
                             edges: Set[Edge],
                             existingWeights: Map[Edge, Double],
                             rng: Random,
                             cfg: GraphConfig
                           ): Map[Edge, Double] = {
    if (!cfg.weighted) Map.empty
    else {
      val (min, max) = cfg.weightRange
      edges.map { e =>
        e -> existingWeights.getOrElse(e, randomWeight(min, max, rng))
      }.toMap
    }
  }

  def ensureConnected(g: Graph, rng: Random, cfg: GraphConfig): Graph = {
    if (isConnected(g)) {g}
    else {
      val components = findComponents(g).reverse
      println(components)

      val newEdges = components
        .sliding(2)
        .collect { case Seq(c1, c2) => Edge(c1.head, c2.head) }
        .toSet

      val allEdges = g.edges ++ newEdges
      val weights = updateWeights(allEdges, g.weights, rng, cfg)
      Graph(g.nodes, allEdges, weights)
    }
  }
}