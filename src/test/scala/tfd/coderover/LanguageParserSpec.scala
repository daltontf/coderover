package tfd.coderover

import org.specs.Specification
import org.specs.util.DataTables
import org.junit.Assert._

class LanguageParserSpec extends Specification with DataTables {
  private[this] val languageParser = new LanguageParser();

  def parsingListProducesExpression(parser:languageParser.Parser[_])(codeList:List[String], expectedAst:Expression) {
    val f = parsingProducesExpression(parser) _
    for (code <- codeList) {
      f(code, expectedAst)
    }
  }

  def parsingProducesExpression(parser:languageParser.Parser[_])(code:String, expectedAst:Expression) {
      val parseResult = languageParser.parseAll(parser, code)
      parseResult.successful mustEqual true
      parseResult.get mustEqual expectedAst
  }

  "Parsing Integer expression" should {
    "handle superfluous parens" in {
      "code list"                                                      | "expression" |>
      List("1", "01", "(1)", "((1))", "(((01)))", "((0000001))")       ! Constant(1)  |
      List("-1", "-01", "(-1)", "((-01))", "((-00001))")               ! Constant(-1) |
      parsingListProducesExpression(languageParser.intExpression) _
    }
  }

  "Parsing with parsers" should {
      "parse comparisons" in {
      "code list"                                                       |  "expression"                                 |>
      List( "1 = 2", "(1) = 2", "1 = (2)", "(1) = (2)")                 !  Equal(Constant(1), Constant(2))              |
      List("-1 < 1")                                                    !  LessThan(Constant(-1), Constant(1))          |
      List(" 3 >-2")                                                    !  GreaterThan(Constant(3), Constant(-2))       |
      List(" 3 >= 2")                                                   !  GreaterThanOrEqual(Constant(3), Constant(2)) |
      List("-4 <= -9")                                                  !  LessThanOrEqual(Constant(-4), Constant(-9))  |
      List(" 7 <> 3")                                                   !  NotEqual(Constant(7), Constant(3))           |
      parsingListProducesExpression(languageParser.comparison) _
    }
  }

  "Parsing program" should {
    "parse empty file" in {
      for (code <- List("", " ","\n", "\n \n")) {
        val parseResult = languageParser.parse(code)
        parseResult.successful mustEqual true
        parseResult.get mustEqual List()
      }
    }

    "parse COUNT function" in {
      val parseResult = languageParser.parse("PUSH COUNT(FOO)")
      parseResult.successful must == (true)
      parseResult.get mustEqual List(Push(Count("FOO")))
    }

    "parse ParamCount" in {
      val parseResult = languageParser.parse("PUSH $COUNT")
      parseResult.successful must == (true)
      parseResult.get mustEqual List(Push(ParamCount()))
    }
  }
}