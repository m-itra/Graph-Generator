package graphgen

import scala.collection.mutable
import scala.util.Random

object Connectivity:

  // Проверка связности графа (проверяется слабая связность)
  private def isConnected(g: Graph): Boolean =
    if g.nodes.isEmpty then true
    else
      val visited = mutable.Set[Int]()
      val toVisit = mutable.Queue[Int](g.nodes.head)

      while toVisit.nonEmpty do
        val current = toVisit.dequeue()
        if !visited.contains(current) then
          visited += current

          val neighbors =
            g.edges.collect {
              case Edge(`current`, v) => v
              case Edge(u, `current`) => u
            }

          neighbors.foreach(n => if !visited.contains(n) then toVisit.enqueue(n))

      visited.size == g.nodes.size

  // Обеспечивает связность: добавляет минимальный набор рёбер, если нужно
  def ensureConnected(g: Graph, rng: Random): Graph =
    if isConnected(g) then g
    else
      val nodeList = g.nodes.toList.sorted // Упорядочиваем узлы
      val newEdges = nodeList.sliding(2).collect { case List(a, b) => Edge(a, b) }.toSet // Создаём цепочку рёбер между соседними узлами

      val allEdges = g.edges ++ newEdges

      val weights =
        if g.weights.nonEmpty then
          // Назначаем веса, если они уже есть
          allEdges.map(e =>
            e -> g.weights.getOrElse(
              e,
              BigDecimal(rng.nextDouble()).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
            )
          ).toMap
        else g.weights // если веса не нужны — оставляем как есть

      Graph(g.nodes, allEdges, weights) // возвращаем новый граф
