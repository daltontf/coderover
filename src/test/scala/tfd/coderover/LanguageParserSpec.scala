package tfd.coderover

import org.specs.Specification
import org.specs.util.DataTables

class LanguageParserSpec extends Specification with DataTables {
  private[this] val languageParser = new LanguageParser();

  def parsingProducesExpression(parser:languageParser.Parser[_])(codeList:List[String], expectedAst:Expression) {
    for (code <- codeList) {
      val parseResult = languageParser.parseAll(parser, code)
      parseResult.successful must == (true)
      parseResult.get must == (expectedAst)
    }
  }

  "Parsing Integer expression" should {
    "handle superfluous parens" in {
      "code list"                                                      | "expression" |>
      List("1", "01", "(1)", "((1))", "(((01)))", "((0000001))")       ! Constant(1)  |
      List("-1", "-01", "(-1)", "((-01))", "((-00001))")               ! Constant(-1) |
      parsingProducesExpression(languageParser.intExpression) _
    }
  }

  "Parsing program" should {
    "parse empty file" in {
      for (code <- List("", " ","\n", "\n \n")) {
        val parseResult = languageParser.parse(code)
        parseResult.successful must == (true)
        parseResult.get must == (List())
      }
    }

    "parse COUNT function" in {
      val parseResult = languageParser.parse("PUSH COUNT(FOO)")
      parseResult.successful must == (true)
      parseResult.get must == (List(Push(Count("FOO"))))
    }
  }
}