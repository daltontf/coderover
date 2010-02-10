package tfd.coderover

import org.specs.runner.{JUnit4, ConsoleRunner}
import org.specs.{Specification}

class EvaluatorSpecTest extends JUnit4(EvaluatorSpec)

object EvaluatorSpec extends Specification {
  private[this] val languageParser = new LanguageParser()
  import languageParser._

  //val failureCases = List("PUSH (1/0)")

  //def defaultEvaluator = new Evaluator(DefaultEnvironment).evaluate(parse("PUSH (2/0)").get, State(2, 2, 0))

  "Evaluator" should {
    "failed with DivideByZero abend" in {
      List("PUSH (1/0)",
           "PUSH 2 WHILE(TOP >= 0) { PRINT (10/TOP) REPLACE(TOP-1) } ").foreach {
        code:String =>
          new Evaluator().evaluate(parse(code).get, new Controller(State(2, 2, 0))) == ResultOrAbend(None, Some(DivideByZero))
        }
    }
  }
}