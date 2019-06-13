package homework3.processors

import homework3.Processor
import homework3.http.HttpResponse

import scala.concurrent.{Future, Promise}

case class StatusCodeCount(codeToCount: Map[Int, Int])

object StatusCodeCounter extends Processor[StatusCodeCount] {
  def apply(url: String, response: HttpResponse): Future[StatusCodeCount] = Promise[StatusCodeCount].success {
    StatusCodeCount(Map(response.status -> 1))
  }.future
}
