package graphgen

import scala.util.Try
import upickle.default.read

def generateGraph[X](cfg: GraphConfig, builder: GraphBuilder[X]): Either[String, X] =
  builder.build(cfg)

def writeGraph[X](graph: Graph, directed: Boolean, writer: GraphMLWriter[X]): X =
  writer.write(graph, directed)

def fullPipeline(cfg: GraphConfig): Either[String, Unit] =
  val builder = cfg.model match
    case "ErdosRenyi"     => Right(Builders.ErdosRenyiBuilder)
    case "BarabasiAlbert" => Right(Builders.BarabasiAlbertBuilder)
    case other            => Left(s"Unknown model: $other")

  for
    b <- builder
    graph <- generateGraph(cfg, b)
    _ <- writeGraph(graph, cfg.directed, Writers.FileGraphMLWriter).toEither.left.map(_.getMessage)
  yield ()


@main def run(): Unit =
  val result = for {
    configJson <- Try(os.read(os.pwd / "config.json")).toEither.left.map(_.getMessage) // Чтение конфигурации из файла
    cfg        <- Try(read[GraphConfig](configJson)).toEither.left.map(_.getMessage)   // Десериализация JSON в объект конфигурации
    _          <- fullPipeline(cfg)
  } yield ()

  result match
    case Left(err)  => println(s"Error: $err")
    case Right(_)   => println("Graph generation completed.")
