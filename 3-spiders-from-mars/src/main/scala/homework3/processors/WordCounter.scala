package homework3.processors

import homework3.Processor
import homework3.html.HtmlUtils
import homework3.http.{ContentType, HttpResponse}

import scala.concurrent.{Future, Promise}

case class WordCount(wordToCount: Map[String, Int])

object WordCount {
  def wordsOf(text: String): List[String] =
    text.split("\\W+").toList.filter(_.nonEmpty)

  def fromText(text: String): WordCount = WordCount(
    WordCount.wordsOf(text)
      .groupBy(identity)
      .mapValues(_.size)
  )
}

object WordCounter extends Processor[WordCount] {
  def apply(url: String, response: HttpResponse): Future[WordCount] = Promise[WordCount].success {
    if (response.isSuccess) {
      WordCount.fromText(getText(response))
    } else {
      WordCount(Map.empty)
    }
  }.future

  private def getText(httpResponse: HttpResponse) =
    httpResponse.contentType match {
      case Some(ContentType(ContentType.Html, _)) =>
        HtmlUtils.toText(httpResponse.body)
      case Some(ContentType(ContentType.PlainText, _)) =>
        httpResponse.body
      case _ => ""
    }
}
