package graphgen

import scala.util.Random
import scala.annotation.tailrec

object Builders:
  // Генерация графа по модели Эрдёша–Реньи
  object ErdosRenyiBuilder extends GraphBuilder[Graph]:

    def build(cfg: GraphConfig): Either[String, Graph] =
      validateConfig(cfg).map { _ =>
        val rng = Random(cfg.seed)
        val nodes = (0 until cfg.n).toSet
        val pairs = allNodePairs(cfg)
        val edges = filterEdges(pairs, cfg.p, rng)
        val weights = generateWeights(edges, cfg.weighted, rng, cfg.weightRange)
        val graph0 = Graph(nodes, edges, weights)

        Option.when(cfg.ensureConnected)(Connectivity.ensureConnected(graph0, rng, cfg)).getOrElse(graph0)
      }

    private def validateConfig(cfg: GraphConfig): Either[String, Unit] =
      Either
        .cond(cfg.n > 0, (), "ErdosRenyi: n must be > 0")
        .flatMap(_ => Either.cond(cfg.p >= 0 && cfg.p <= 1, (), "ErdosRenyi: p must be in [0, 1]"))

    private def allNodePairs(cfg: GraphConfig): Seq[(Int, Int)] =
      for {
        u <- 0 until cfg.n
        v <- 0 until cfg.n
        if (cfg.selfLoops || u != v) && (cfg.directed || u < v || (cfg.selfLoops && u == v))
      } yield (u, v)

    private def filterEdges(pairs: Seq[(Int, Int)], p: Double, rng: Random): Set[Edge] =
      pairs
        .filter(_ => rng.nextDouble() < p)
        .map { case (u, v) => Edge(u, v) }
        .toSet

    private def generateWeights(
                                 edges: Set[Edge],
                                 weighted: Boolean,
                                 rng: Random,
                                 weightRange: (Double, Double)
                               ): Map[Edge, Double] = {
      if (!weighted) Map.empty
      else {
        val (minWeight, maxWeight) = weightRange
        edges.map { e =>
          val raw = rng.nextDouble()
          val scaled = minWeight + raw * (maxWeight - minWeight)
          val weight = BigDecimal(scaled).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
          e -> weight
        }.toMap
      }
    }



  // Генерация графа по модели Барабаши–Альберт
  object BarabasiAlbertBuilder extends GraphBuilder[Graph]:

    def build(cfg: GraphConfig): Either[String, Graph] =
      validateConfig(cfg).map { _ =>
        val rng = Random(cfg.seed)
        val nodes = (0 until cfg.n).toSet

        val (initialEdges, initialDegreeMap, initialSize) = createInitialGraph(cfg)
        val (finalEdges, _) = growGraph(cfg, initialSize, cfg.n - 1, initialEdges, initialDegreeMap, rng)
        val weights = generateWeights(finalEdges, cfg.weighted, cfg.weightRange, rng)
        val graph0 = Graph(nodes, finalEdges, weights)

        Option.when(cfg.ensureConnected)(Connectivity.ensureConnected(graph0, rng, cfg)).getOrElse(graph0)
      }

    private def validateConfig(cfg: GraphConfig): Either[String, Unit] =
      Either.cond(cfg.n > 0, (), "BarabasiAlbert: n must be > 0")
        .flatMap(_ => Either.cond(cfg.m > 0 && cfg.m < cfg.n, (), "BarabasiAlbert: m must be > 0 and < n"))

    private def createInitialGraph(cfg: GraphConfig): (Set[Edge], Map[Int, Int], Int) =
      val initialSize = if cfg.m == 1 then 2 else cfg.m
      val initialNodes = (0 until initialSize).toList
      val initialEdges = initialNodes.combinations(2).collect {
        case List(u, v) => Edge(u, v)
      }.toSet
      val initialDegrees = initialNodes.map(n => n -> (initialSize - 1)).toMap
      (initialEdges, initialDegrees, initialSize)

    private def growGraph(
                           cfg: GraphConfig,
                           from: Int,
                           to: Int,
                           edges: Set[Edge],
                           degreeMap: Map[Int, Int],
                           rng: Random
                         ): (Set[Edge], Map[Int, Int]) =
      (from to to).foldLeft((edges, degreeMap)) { case ((curEdges, curDegrees), newNodeId) =>
        val selected = selectNodes(cfg.m, curDegrees, rng)
        val newEdges = selected.map(old => Edge(newNodeId, old))
        val updatedEdges = curEdges ++ newEdges
        val updatedDegrees = selected.foldLeft(curDegrees)((m, old) => m.updated(old, m(old) + 1))
          .updated(newNodeId, cfg.m)
        (updatedEdges, updatedDegrees)
      }

    private def selectNodes(
                             toSelect: Int,
                             degreeMap: Map[Int, Int],
                             rng: Random
                           ): Set[Int] =
      val nodes = degreeMap.keys.toList
      val degrees = nodes.map(degreeMap)
      val total = degrees.sum.toDouble

      @tailrec
      def loop(selected: Set[Int], remaining: List[Int]): Set[Int] =
        if selected.size >= toSelect || remaining.isEmpty then selected
        else
          val rand = rng.nextDouble() * total
          val chosen = remaining
            .scanLeft((0.0, -1)) { case ((acc, _), node) => (acc + degreeMap(node), node) }
            .drop(1)
            .find(_._1 >= rand)
            .map(_._2)
            .getOrElse(remaining.head)
          loop(selected + chosen, remaining.filterNot(_ == chosen))

      loop(Set.empty, nodes)

    private def generateWeights(
                                 edges: Set[Edge],
                                 weighted: Boolean,
                                 weightRange: (Double, Double),
                                 rng: Random
                               ): Map[Edge, Double] =
      if !weighted then Map.empty
      else
        val (minWeight, maxWeight) = weightRange
        edges.map { e =>
          val raw = rng.nextDouble()
          val scaled = minWeight + raw * (maxWeight - minWeight)
          val weight = BigDecimal(scaled).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
          e -> weight
        }.toMap