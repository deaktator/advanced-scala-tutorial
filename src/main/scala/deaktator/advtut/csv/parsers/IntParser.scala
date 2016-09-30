package deaktator.advtut.csv.parsers

import scala.util.parsing.combinator.RegexParsers

/**
  * A parser that can parse integer in different formats:
  - '''binary''' ''like'' `0b010110`
  - '''octal''' ''like'' `0o0123512`
  - '''hex''' ''like'' `0x1235abce`
  - '''decimal''' ''like'' `12512`
  *
  * Notice that the provided self-type below necessitating that this
  * trait be mixed into something that extends `RegexParsers`.  This allows
  * the code to use APIs specified in the `RegexParsers`.
  */
trait IntParser { self: RegexParsers =>
  // Notice that we have different formats here.  These are just regular Scala
  // Regexs.

  protected[this] val binaryStr = """0b([01]+)""".r
  protected[this] val octalStr = """0o([0-7]+)""".r
  protected[this] val hexStr = """0x([\da-fA-F]+)""".r

  /**
    * This is the top-level parser that is exposed by this trait.  As such,
    * it's a good practice to explicitly list the type.  This parser just does
    * ordinary alternation and constructs a new parser with the lowest common
    * subtype.  Since all of the constituent parsers are `Parser[Int]` the
    * result will therefore also be `Parser[Int]`.
    *
    * Note that we are OK in this situation because the first two characters
    * of the parse are a unique indicator of which parser is used.  If
    * multiple parsers could have parsed the data, we would have what's known
    * as an ambiguous parse.  This would be bad, but it's not applicable here.
    */
  lazy val int: Parser[Int] = binary | octal | hex | decimal

  // The following parsers work similarly, just on different bases.

  protected[this] lazy val binary = binaryStr ^^ { case binaryStr(s) => Integer.parseInt(s, 2) }
  protected[this] lazy val octal = octalStr ^^ { case octalStr(s) => Integer.parseInt(s, 8) }
  protected[this] lazy val hex = hexStr ^^ { case hexStr(s) => Integer.parseInt(s, 16) }

  // This is a parser for integral values.  Notice that we can specify a regular
  // expression inline in the parser.  After the regular expression there is the
  // `^^` combinator.  This takes a function with an input type the same as the
  // type that parametrizes the parser on whom `^^` is called.  The result is a
  // parser with a new type.  In this case the original parser was a
  // `Parser[String]` and the function is a `(String => Int)` so the parser that
  // is created will be a `Parser[Int]` which is exactly what we want.
  protected[this] lazy val decimal = """0|[1-9][0-9]*""".r ^^ { case s => Integer.parseInt(s) }
}
