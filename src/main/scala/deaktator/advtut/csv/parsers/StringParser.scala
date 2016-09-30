package deaktator.advtut.csv.parsers

import scala.util.parsing.combinator.RegexParsers

/**
  * A parser for strings using the parser combinator library.
  *
  * The only special thing here is the escaping of commas so
  * that the
  *
  * Created by ryan on 9/23/16.
  */
trait StringParser { self: RegexParsers =>
  lazy val string: Parser[String] = """(\\,|[^,]*|)*""".r
}
