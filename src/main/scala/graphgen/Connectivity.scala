package graphgen

import scala.annotation.tailrec
import scala.util.Random

object Connectivity:

  // Проверка связности графа (проверяется слабая связность)
  private def neighbors(g: Graph, node: Int): Set[Int] =
    g.edges.collect {
      case Edge(`node`, v) => v
      case Edge(u, `node`) => u
    }

  @tailrec
  private def bfs(g: Graph, visited: Set[Int], toVisit: List[Int]): Set[Int] =
    toVisit match
      case Nil => visited
      case current :: rest if visited.contains(current) =>
        bfs(g, visited, rest)
      case current :: rest =>
        val newNeighbors = neighbors(g, current).diff(visited)
        bfs(g, visited + current, rest ++ newNeighbors)

  private def isConnected(g: Graph): Boolean =
    if g.nodes.isEmpty then true
    else bfs(g, Set.empty, List(g.nodes.head)).size == g.nodes.size
  
  // Обеспечивает связность: добавляет минимальный набор рёбер
  private def chainEdges(nodes: List[Int]): Set[Edge] =
    nodes.sliding(2).collect { case List(a, b) => Edge(a, b) }.toSet

  private def updateWeights(
                     edges: Set[Edge],
                     existingWeights: Map[Edge, Double],
                     rng: Random
                   ): Map[Edge, Double] =
    if existingWeights.isEmpty then existingWeights
    else
      edges.map { e =>
        e -> existingWeights.getOrElse(
          e,
          BigDecimal(rng.nextDouble()).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
        )
      }.toMap

  def ensureConnected(g: Graph, rng: Random): Graph =
    if isConnected(g) then g
    else
      val nodeList = g.nodes.toList.sorted
      val newEdges = chainEdges(nodeList)
      val allEdges = g.edges ++ newEdges
      val weights = updateWeights(allEdges, g.weights, rng)
      Graph(g.nodes, allEdges, weights)
