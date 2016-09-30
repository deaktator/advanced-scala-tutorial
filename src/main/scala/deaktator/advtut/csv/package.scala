package deaktator.advtut.csv

import deaktator.advtut.csv.parsers.{StringParser, IntParser}

import scala.util.parsing.combinator.RegexParsers

// Below, is the CSV algebraic data type.  We create this so we can later use it
// to parameterize the `parser` method inside CSV.CSVParser.  Notice the f-bound
// polymorphic type.  This is so that we can have a closed set of concrete types
// accepted in `parser`.  One should note that this is just a simple product type.

sealed trait CSV[+Impl <: CSV[Impl]] extends Product

case class CSV1[+A](_1: A) extends CSV[CSV1[A]]
case class CSV2[+A, +B](_1: A, _2: B) extends CSV[CSV2[A, B]]
case class CSV3[+A, +B, +C](_1: A, _2: B, _3: C) extends CSV[CSV3[A, B, C]]

object CSV {
  object CSVParser extends RegexParsers
                      with IntParser
                      with StringParser {
    /**
      * A mapping from a string represent of the column type to a parser that
      * knows how to parse that type of column.  For expository purposes we
      * just concentrate on strings and integers but this could easily be
      * extended.
      */
    private[this] val ParserMap = Map(
      manifest[Int].toString -> int,
      manifest[String].toString -> string
    )

    /**
      * Because there's a bijection between the subclasses of CSV and the set
      * `{1, 2, 3}`, a mapping from the number of columns to parser for that
      * many columns can easily be created.  Note the type parameter for the
      * value type in the map uses an existential type because we can't provide
      * a more specific type without the use of something like a polymorphic
      * map.  This is "OK" (but not great).  We will need to do some casting
      * unfortunately.
      */
    private[this] val FnMap: Map[Int, PartialFunction[Any, CSV[_]]] = Map(
      1 -> { case a => CSV1(a) },
      2 -> { case a ~ b => CSV2(a, b) },
      3 -> { case a ~ b ~ c => CSV3(a, b, c) }
    )

    /**
      * Given a type of CSV data, `Impl`, create a parser for that type of data.
      *
      * Uses an implicit manifest to perform reflection.  The Manifest is injected
      * by the Scala compiler at compile time.  This means that one just has to
      * provide the proper '''type''' to get the right parser, not something more
      * like a `Class` or super token type like in Java.
      *
      * '''NOTE''': `Manifest`s are deprecated but I think they're better in Scala
      * 2.10 than things like `TypeTag`s due to Scala thread-safety issues in the
      * reflection library in 2.10.  In 2.11, I'd definitely recommend using the
      * new reflection APIs as they are thread-safe.
      *
      * {{{
      * val errorOrParser = CSV.CSVParser.parser[CSV[Int]]
      *
      * // CSV.CSVParser.Parser[CSV[Int]]
      * val parser = errorOrParser.right.get  // For simplicity, throw eagerly.
      * }}}
      *
      * @tparam Impl The type of CSV information the parser should extract from
      *              the raw string data.
      * @return On failure, return a non-empty list of the ''Left'' or a
      *         `Parser[Impl]` on the ''Right''.
      */
    private[csv] def parser[Impl <: CSV[Impl]: Manifest]: Either[::[String], Parser[Impl]] = {
      // NOTE: This isn't necessarily the most type-safe code as there is some
      //       casting and there are some places where the compile will complain
      //       about missing cases in match statements but it does work.  The
      //       goal here is interface simplicity and showing off a little of
      //       Scala's abilities to simplify interfaces.

      // Get a list of the parsers for each column.  This is done via reflection
      // on the information that is injected by the compiler.
      val columnParsers = parsersForColumns[Impl]

      if (columnParsers.exists(p => p.isLeft)) {
        // At least one element must exist because IF's predicate succeeds
        // so there is at least one error in one of the column parsers. Collect
        // the error information and turn it into a non-empty list and wrap in
        // a Left (to represent an error condition, as is idiomatic in Scala).
        columnParsers.collect {
          case Left((m, i)) => s"In column $i, unrecognized type: $m"
        } match {
          case h :: t => Left(::(h, t))
        }
      }
      else {
        // Notice that since all elements of `columnParsers` are `Right` (b/c IF's
        // predicate fails), `columnParsers.size` is the same as the number of columns
        // expected in the CSV output.  Additionally, there is at least one column
        // because we don't have a CSV0 class.  Therefore, we can use reduceLeft
        // instead of foldLeft.
        //
        // The first flatMap is just to produce a list without `Right` wrapping the
        // parsers.
        //
        // The reduce says that we want to build up a parser of many columns from
        // a sequence of parsers of individual columns.  `c` represents the currently
        // built up parser on many columns.  To inductively create a parser that
        // parsers one additional column, we use the `~` combinator that creates a
        // parser of a product of two items from two parses that parse those items.
        // The `~>` combinator is similar but it throws away the things to the left
        // of the arrow while retaining the data that was parsed by the parser on the
        // right.
        //
        // A subtlety to note here is that on each loop of the `reduceLeft`, the type
        // of `c` is changed because when `c` is composed with the new column parser,
        // `p`, a new parser is made that can parse all of the columns that `c` could
        // and one additional column.  Therefore the parser can't be the same "shape"
        // across loops.  So if you tried to infer the type of `c`, it wouldn't make
        // sense, but `combined` is a `Parser[Any]`.
        val combined = columnParsers.flatMap{ case Right(p) => Option(p) }.
                                     reduceLeft((c, p) => c ~ ("," ~> p))

        // Take the combined parser, which we know to be composed of column types
        // that can be parsed (string, int) and has the appropriate number of columns
        // (1, 2, or 3).  Then we can perform a lookup in the based on the number of
        // columns and modify `combined` to take the product of values and construct
        // a subtype of CSV.  This must then be casted to the proper type.
        //
        // Casting probably isn't avoidable without some encoding of products like
        // tuples, HLists, etc.
        Right((combined ^^ FnMap(columnParsers.size)).asInstanceOf[Parser[Impl]])
      }
    }

    /**
      * Constructor a list of coproducts of error or parser for the column types
      * determined by the implicitly supplied `Manifest` of type `Impl`.
      * @tparam Impl the subtype of CSV to return
      * @return
      */
    // List[Either[(Manifest[_], Int), Parser[Any]]]
    private[this] def parsersForColumns[Impl <: CSV[Impl]: Manifest] = {
      // Since manifest[Impl] is the same as implicitly[Manifest[Impl]], it just
      // materializes the Manifest provided by the compiler to overcome type
      // erasure in the JVM.

      // Note the each column has a concrete type and those are also encoded in the
      // Manifest within the `typeArguments` list.  We zip with the indices of the
      // list for reporting.  If we can get the parser for the string representation
      // of the column type, return it wrapped in a `Right`.  If the ParserMap
      // doesn't contain a parser for the type, we return the manifest for the column
      // and the column index wrapped inside a `Left`.  This will then be used for
      // error reporting in the `parser` method.
      manifest[Impl].typeArguments.zipWithIndex.map { case x@(m, i) =>
        ParserMap.get(m.toString).toRight(x)
      }
    }
  }

  // =============================================================================
  //  The public methods to use for transforming string data to CSV data.
  // =============================================================================


  /**
    * Transform an iterator of strings to an iterator of optional CSV data.
    * Option is used in case of parse errors in an individual line.  This would
    * obviously include errors if this were "real" production code.
    * {{{
    * val errOrCsvIter = CSV[CSV2[Int, String]](Iterator("1,two","3,four","5,six"))
    * val csvIter = errOrCsvIter.right.get // fail fast.
    * val validData: Iterator[CSV2[Int, String]] = csvIter.flatten
    * }}}
    * @param lines iterator of string data to parse.
    * @tparam Impl the type of data expected back.
    * @return
    */
  // Either[::[String], Iterator[Option[Impl]]]
  def apply[Impl <: CSV[Impl]: Manifest](lines: Iterator[String]) =
    CSV.CSVParser.parser[Impl].right.map { p =>
      lines.map { line => applyParser(p, line) }
    }

  /**
    * Transform a string to an option of CSV data.
    * {{{
    * val errOrCsv = CSV[CSV2[Int, String]]("0x1d,two")
    * val csv = errOrCsv.right.get // fail fast.
    * val validData: CSV2[Int, String] = csv.get // Shouldn't call get on Option.
    * }}}
    * @param line string data to parse.
    * @tparam Impl the type of data expected back.
    * @return
    */
  // Either[::[String], Option[Impl]]
  def apply[Impl <: CSV[Impl]: Manifest](line: String) =
    CSV.CSVParser.parser[Impl].right.map { p =>
      applyParser(p, line)
    }

  // Option[Impl]
  private[this] def applyParser[Impl <: CSV[Impl]](parser: CSVParser.Parser[Impl],
                                                   line: String) = {
    CSV.CSVParser.parseAll(parser, line) match {
      case CSV.CSVParser.Success(s, _) => Option(s)
      case _ => None
    }
  }
}
