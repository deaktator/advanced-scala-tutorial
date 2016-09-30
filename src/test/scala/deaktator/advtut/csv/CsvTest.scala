package deaktator.advtut.csv

import deaktator.advtut.csv.CSV.CSVParser
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.BlockJUnit4ClassRunner

@RunWith(classOf[BlockJUnit4ClassRunner])
class CsvTest {

  @Test def testIterator(): Unit = {
    val rawLines = Iterator(
      "0x1,the first line",
      "0o2,the second line",
      "0b11,the third line"
    )

    val expected = Seq(
      CSV2(1, "the first line"),
      CSV2(2, "the second line"),
      CSV2(3, "the third line")
    )

    // : Either[::[String], Iterator[Option[CSV2[Int, String]]]]
    val linesEither = CSV[CSV2[Int, String]](rawLines)

    // : Vector[CSV2[Int, String]]
    val lines = linesEither.right.get.flatten.toVector

    assertEquals(expected, lines)
  }

  @Test def testInstance(): Unit = {
    val parser = CSV.CSVParser.parser[CSV3[Int, String, String]].right.get
    val CSV3(c1, c2, c3) = CSV.CSVParser.parseAll(parser, "1, two, three").get
    assertEquals(1, c1)
    assertEquals("two", c2)
    assertEquals("three", c3)
  }
}
