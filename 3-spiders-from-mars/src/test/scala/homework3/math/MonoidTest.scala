package homework3.math

import java.nio.file.Paths

import homework3.processors.{SavedFiles, WordCount}
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  import homework3.math.Monoid.ops._

  "wordCountMonoid" should "sum the counts for same words" in {
    val a = WordCount(Map("example" -> 1))
    val b = WordCount(Map("example" -> 2, "test" -> 1))

    val c = a |+| b

    c.wordToCount.get("example") shouldEqual Some(3)
    c.wordToCount.get("test") shouldEqual Some(1)
  }

  it should "include all the words from both WordCounts" in {
    val a = WordCount(Map("apple" -> 1))
    val b = WordCount(Map("cake" -> 2, "muffin" -> 1))

    val c = a |+| b

    c.wordToCount.keys should contain allElementsOf Seq(
      "apple",
      "cake",
      "muffin"
    )
  }

  "savedFilesMonoid" should "include all files from both SavedFiles" in {
    val a = SavedFiles(Map(
      "example.com" -> Paths.get("path/to/file1")
    ))
    val b = SavedFiles(Map(
      "example2.com" -> Paths.get("path/to/file2")
    ))

    val c = a |+| b
    c.urlToPath.keys should contain allElementsOf Seq(
      "example.com",
      "example2.com"
    )
  }

  it should "include duplicate urls only once" in {
    val a = SavedFiles(Map(
      "example.com" -> Paths.get("path/to/file1")
    ))

    val c = a |+| a
    c.urlToPath.keys should contain only "example.com"
  }
}
