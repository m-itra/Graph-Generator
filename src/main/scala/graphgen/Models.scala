package graphgen

import upickle.default.{ReadWriter, macroRW}

case class GraphConfig(
                        n: Int,                         // число узлов
                        model: String,                  // модель генерации: "ErdosRenyi", "BarabasiAlbert"
                        p: Double,                      // вероятность ребра (для Erdos–Rényi)
                        m: Int,                         // число рёбер для нового узла (для Barabási–Albert)
                        directed: Boolean,              // направленный или нет
                        weighted: Boolean,              // взвешенный или нет
                        weightRange: (Double, Double),  // Диапазон генерации веса
                        ensureConnected: Boolean,       // проверка на связность
                        seed: Long,                     // зерно для генератора случайных чисел
                        selfLoops:  Boolean,            // Разрешить петли
                      )

// Ребро графа
case class Edge(u: Int, v: Int)

// Структура графа
case class Graph(nodes: Set[Int], edges: Set[Edge], weights: Map[Edge, Double])

// Сериализация GraphConfig через upickle
given ReadWriter[GraphConfig] = macroRW

// Интерфейс построителя графа
trait GraphBuilder[X]:
  def build(cfg: GraphConfig): Either[String, X]

// Интерфейс записи графа в формате GraphML
trait GraphMLWriter[X]:
  def write(graph: Graph, directed: Boolean): X
