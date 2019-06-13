package homework3

import homework3.http.{ContentType, HttpResponse}

object Mocks {
  val notFoundResponse = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 404
  }

  val okResponse = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def body: String = ???

    override def status: Int = 200
  }

  val htmlResponse = new HttpResponse {
    def headers: Map[String, String] = ???
    def bodyAsBytes: Array[Byte] = ???

    def status: Int = 200

    override def body: String =
      """
        |<html>
        | <body>
        |   just some words to be counted
        | </body>
        |</html>
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }

  val plainTextResponse = new HttpResponse {
    def headers: Map[String, String] = ???
    def bodyAsBytes: Array[Byte] = ???

    def status: Int = 200

    override def body: String =
      """
        |test response body for counting words
        |here we have the word test again
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.PlainText, None))
  }

  val pngResponse = new HttpResponse {
    def headers: Map[String, String] = ???
    def bodyAsBytes: Array[Byte] = ???

    def status: Int = 200

    override def contentType: Option[ContentType] =
      Some(ContentType("image/png", None))
  }

  val responseWithLinks = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 200

    override def body: String =
      """
        |<html>
        | <body>
            |<link rel="dns-prefetch" href="https://github.githubassets.com">
            |<a href="https://user-images.githubusercontent.com/" />
            |<img src="https://github.githubassets.com/images/search-key-slash.svg" alt="" class="mr-2 header-search-key-slash">
            |<a href="htt://wrong.example.com/" />
        | </body>
        |</html>
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }

  def responseWithSameDomainLinks(domain: String) = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 200

    override def body: String =
      s"""
        |<html>
        | <body>
        |   <link rel="dns-prefetch" href="https://github.githubassets.com">
        |   <a href="${domain}/users" />
        |   <img src="${domain}/images/search-key-slash.svg" alt="" class="mr-2 header-search-key-slash">
        |   <a href="htt://wrong.example.com/" />
        | </body>
        |</html>
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }
}
