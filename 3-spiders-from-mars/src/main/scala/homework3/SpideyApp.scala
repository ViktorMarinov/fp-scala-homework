package homework3

import java.util.concurrent.ForkJoinPool

import homework3.http.AsyncHttpClient
import homework3.processors.{BrokenLinkDetector, FileOutput, WordCount, WordCounter}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object SpideyApp {
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool)
  val blockingExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool(4))

  val httpClient = new AsyncHttpClient
  val spidey = new Spidey(httpClient)

  def printUsage: Unit = {
    println(
      """
        |Usage:
        |
        |SpideyApp <url> <max-depth> <processor> [processor-config]
        |
        |Possible processors and their config are:
        |
        |file-output <target-dir>
        |word-counter
        |broken-link-detector
      """.stripMargin)
  }

  def processor[O](name: String, config: Option[String]) = name match {
    case "file-output" => new FileOutput(config.get)(blockingExecutionContext)
    case "word-counter" => WordCounter
    case "broken-link-detector" => BrokenLinkDetector
  }

  def checkProcessor(args: Array[String]): Boolean = {
    val processor = args(2)
    processor match {
      case "file-output" => args.length == 4
      case "word-counter" => args.length == 3
      case "broken-link-detector" => args.length == 3
      case _ => false
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 3 || !checkProcessor(args)) printUsage
    else {

      val url = args(0)
//      val maxDepth = args(1).toInt
      val maxDepth = 1

//      val processor = WordCounter
//      val processor = BrokenLinkDetector
      val processor = new FileOutput("/tmp/3-spiders-from-mars")(blockingExecutionContext)


      // run Spidey
      val fResult = spidey.crawl(url, SpideyConfig(maxDepth))(processor)
      val res = Await.result(fResult, Duration.Inf)

      // output result
      println(res)
    }

    httpClient.shutdown()
  }
}
