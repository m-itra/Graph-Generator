package graphgen

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Builders:
  // Генерация графа по модели Эрдёша–Реньи
  object ErdosRenyiBuilder extends GraphBuilder[Graph]:
    def build(cfg: GraphConfig): Either[String, Graph] =
      if cfg.n <= 0 then Left("ErdosRenyi: n must be > 0")
      else if cfg.p < 0 || cfg.p > 1 then Left("ErdosRenyi: p must be in [0, 1]")
      else
        val rng = Random(cfg.seed)
        val nodes = (0 until cfg.n).toSet

        // Генерируем рёбра случайно с вероятностью p
        val edgeStream =
          for
            u <- LazyList.range(0, cfg.n)
            v <- LazyList.range(if cfg.directed then 0 else u + 1, cfg.n)
            if !cfg.directed || u != v
            if rng.nextDouble() < cfg.p
          yield Edge(u, v)
        val edges = edgeStream.toSet

        // Добавляем веса, если граф взвешенный
        val weights =
          if cfg.weighted then
            edges.map(e => e -> BigDecimal(rng.nextDouble()).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble).toMap
          else Map.empty
        val graph0 = Graph(nodes, edges, weights)

        // Обеспечиваем связность, если указано
        val result = if cfg.ensureConnected then Connectivity.ensureConnected(graph0, rng) else graph0
        Right(result)
  
  
  // Генерация графа по модели Барабаши–Альберт
  object BarabasiAlbertBuilder extends GraphBuilder[Graph]:
    def build(cfg: GraphConfig): Either[String, Graph] =
      if cfg.n <= 0 then Left("BarabasiAlbert: n must be > 0")
      else if cfg.m <= 0 || cfg.m >= cfg.n then Left("BarabasiAlbert: m must be > 0 and < n")
      else
        val rng = new Random(cfg.seed)
        val nodes = (0 until cfg.n).toSet

        // Инициализируем начальный полный граф
        val initialSize = if cfg.m == 1 then 2 else cfg.m
        val initial = (0 until initialSize).toList
        val initialEdges = initial.combinations(2).collect {
          case List(u, v) => Edge(u, v)
        }.toSet

        // Начальные степени узлов
        val initialDegreeMap = initial.map(n => n -> (initialSize - 1)).toMap

        // Выбираем m узлов пропорционально их степени
        def selectNodes(toSelect: Int, degreeMap: Map[Int, Int], rng: Random): Set[Int] =
          val nodes = degreeMap.keys.toList
          val degrees = nodes.map(degreeMap)
          val total = degrees.sum.toDouble

          val selected = ArrayBuffer.empty[Int]
          val candidates = ListBuffer(nodes: _*)

          while selected.size < toSelect && candidates.nonEmpty do
            val rand = rng.nextDouble() * total
            var accum = 0.0
            var index = 0
            while index < candidates.size && accum <= rand do
              accum += degreeMap(candidates(index))
              index += 1

            val chosenIndex = (index - 1).max(0)
            val chosen = candidates(chosenIndex)
            selected += chosen
            candidates.remove(chosenIndex)

          selected.toSet

        // Рекурсивно добавляем новые узлы
        @tailrec
        def attach(
                    newNodeId: Int,
                    maxId: Int,
                    edges: Set[Edge],
                    degreeMap: Map[Int, Int]
                  ): (Set[Edge], Map[Int, Int]) =
          if newNodeId > maxId then (edges, degreeMap)
          else
            val selected = selectNodes(cfg.m, degreeMap, rng)
            val newEdges = selected.map(old => Edge(newNodeId, old))
            val updatedEdges = edges ++ newEdges
            val updatedDegreeMap = selected.foldLeft(degreeMap)((map, old) => map.updated(old, map(old) + 1))
              .updated(newNodeId, cfg.m)
            attach(newNodeId + 1, maxId, updatedEdges, updatedDegreeMap)

        val (finalEdges, _) = attach(initialSize, cfg.n - 1, initialEdges, initialDegreeMap)

        // Генерация весов, если нужно
        val weights =
          if cfg.weighted then
            finalEdges.map(e =>
              e -> BigDecimal(rng.nextDouble()).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
            ).toMap
          else Map.empty
        val graph0 = Graph(nodes, finalEdges, weights)
        
        // Обеспечиваем связность, если нужно
        val result = if cfg.ensureConnected then Connectivity.ensureConnected(graph0, rng) else graph0
        Right(result)