package tfd.coderover

import junit.framework._
import org.junit.Assert._

class EvaluatorTest extends TestCase {
  import LanguageParser._

  private def executeConstantTest(stringInput: String, expectedConstant: Constant, expectedInt: Int) {
    val ast = parseAll(constant, stringInput).get
    assertEquals(expectedConstant, ast)
    assertEquals(expectedInt, new Evaluator(DefaultEnvironment).evaluate(ast, State(0, 0, 0)))
  }

  private def executeMathematicalTest(stringInput: String, expectedMathematical: Mathematical, expectedInt: Int) {
    val ast = parseAll(mathematical, stringInput).get
    assertEquals(expectedMathematical, ast)
    assertEquals(expectedInt, new Evaluator(DefaultEnvironment).evaluate(ast, State(0, 0, 0)))
  }

  private def executeComparisonTest(stringInput: String, expectedComparison: Comparison, expectedBoolean: Boolean) {
    val ast = parseAll(comparison, stringInput).get
    assertEquals(expectedComparison, ast)
    assertEquals(expectedBoolean, new Evaluator(DefaultEnvironment).evaluate(ast, State(0, 0, 0)))
  }

  private def executeBooleanLogicTest(stringInput: String, expectedBooleanLogic: BooleanExpression, expectedBoolean: Boolean) {
    val ast = parseAll(nestedBoolean, stringInput).get
    assertEquals(expectedBooleanLogic, ast)
    assertEquals(expectedBoolean, new Evaluator(DefaultEnvironment).evaluate(ast, State(0, 0, 0)))
  }

  private def executeIntExpressionTest(stringInput: String, expectedAst: Expression, expectedIntResult: Int) {
    val ast = parseAll(intExpression, stringInput).get
    assertEquals(expectedAst, ast)
    assertEquals(expectedIntResult, new Evaluator(DefaultEnvironment).evaluate(ast, State(0, 0, 0)))
  }


  def testConstant() {
    executeConstantTest("42", Constant(42), 42)
    executeConstantTest("-273", Constant(-273), -273)
  }

  def testMathematical() {
    executeMathematicalTest("2 + 2", Add(List(Constant(2), Constant(2))), 4)
    executeMathematicalTest("2 + -2", Add(List(Constant(2), Constant(-2))), 0)
    executeMathematicalTest("-2 + 2", Add(List(Constant(-2), Constant(2))), 0)
    executeMathematicalTest("-2 + -2", Add(List(Constant(-2), Constant(-2))), -4)
    executeMathematicalTest("1 + -2 + -3", Add(List(Constant(1), Constant(-2), Constant(-3))), -4)

    executeMathematicalTest("2 - 2", Subtract(List(Constant(2), Constant(2))), 0)
    executeMathematicalTest("2 - -2", Subtract(List(Constant(2), Constant(-2))), 4)
    executeMathematicalTest("-2 - 2", Subtract(List(Constant(-2), Constant(2))), -4)
    executeMathematicalTest("-2 - -2", Subtract(List(Constant(-2), Constant(-2))), 0)
    executeMathematicalTest("-2 - -2 - 3", Subtract(List(Constant(-2), Constant(-2), Constant(3))), -3)

    executeMathematicalTest("-2 * -2 * 3", Multiply(List(Constant(-2), Constant(-2), Constant(3))), 12)

    executeMathematicalTest("24 / 2 / 3", Divide(List(Constant(24), Constant(2), Constant(3))), 4)

    executeMathematicalTest("24 % 5 % 3", Modulus(List(Constant(24), Constant(5), Constant(3))), 1)

    executeMathematicalTest("(2 - 3) + 4", Add(List(Subtract(List(Constant(2), Constant(3))), Constant(4))), 3)
    executeMathematicalTest("2 - (3 + 4)", Subtract(List(Constant(2), Add(List(Constant(3), Constant(4))))), -5)

    executeMathematicalTest("(2 * 3) - 4", Subtract(List(Multiply(List(Constant(2), Constant(3))), Constant(4))), 2)
    executeMathematicalTest("2 - (10 / 3)", Subtract(List(Constant(2), Divide(List(Constant(10), Constant(3))))), -1)
    executeMathematicalTest("4 + (10 % 3)", Add(List(Constant(4), Modulus(List(Constant(10), Constant(3))))), 5)
  }

  def testExpression() {
    executeIntExpressionTest("-(2 + 2)", Negate(Add(List(Constant(2), Constant(2)))), -4)
  }

  def testComparison() {
    executeComparisonTest("2 = 2", Equal(Constant(2), Constant(2)), true)
    executeComparisonTest("2 = -2", Equal(Constant(2), Constant(-2)), false)

    executeComparisonTest("2 < 2", LessThan(Constant(2), Constant(2)), false)
    executeComparisonTest("-2 < 2", LessThan(Constant(-2), Constant(2)), true)

    executeComparisonTest("2 > 2", GreaterThan(Constant(2), Constant(2)), false)
    executeComparisonTest("2 > -2", GreaterThan(Constant(2), Constant(-2)), true)

    executeComparisonTest("2 <= 2", LessThanOrEqual(Constant(2), Constant(2)), true)
    executeComparisonTest("2 <= -2", LessThanOrEqual(Constant(2), Constant(-2)), false)

    executeComparisonTest("2 >= 2", GreaterThanOrEqual(Constant(2), Constant(2)), true)
    executeComparisonTest("-2 >= 2", GreaterThanOrEqual(Constant(-2), Constant(2)), false)

    executeComparisonTest("2 <> 2", NotEqual(Constant(2), Constant(2)), false)
    executeComparisonTest("2 <> -2", NotEqual(Constant(2), Constant(-2)), true)
  }

  def testBooleanLogic() {
    executeBooleanLogicTest("((1 + 3) = (0 - -4))", Equal(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4)))), true)
    executeBooleanLogicTest("((1 + 3) <> (0 - -4))", NotEqual(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4)))), false)
    executeBooleanLogicTest("((2 > 3) OR (4 > 3))", Or(List(GreaterThan(Constant(2), Constant(3)), GreaterThan(Constant(4), Constant(3)))), true)
    executeBooleanLogicTest("((2 > 3) OR (4 > 5))", Or(List(GreaterThan(Constant(2), Constant(3)), GreaterThan(Constant(4), Constant(5)))), false)
    executeBooleanLogicTest("((4 > 3) AND (5 > 3))", And(List(GreaterThan(Constant(4), Constant(3)), GreaterThan(Constant(5), Constant(3)))), true)
    executeBooleanLogicTest("((2 > 3) AND (5 > 3))", And(List(GreaterThan(Constant(2), Constant(3)), GreaterThan(Constant(5), Constant(3)))), false)
    executeBooleanLogicTest("((2 > 3) OR (4 > 3) OR (2 < 1))", Or(List(GreaterThan(Constant(2), Constant(3)),
      GreaterThan(Constant(4), Constant(3)),
      LessThan(Constant(2), Constant(1)))), true)
    executeBooleanLogicTest("((2 > 3) OR (4 > 5) OR (2 < 1))", Or(List(GreaterThan(Constant(2), Constant(3)),
      GreaterThan(Constant(4), Constant(5)),
      LessThan(Constant(2), Constant(1)))), false)
    executeBooleanLogicTest("((4 > 3) AND (5 > 3) AND (1 < 2))", And(List(GreaterThan(Constant(4), Constant(3)),
      GreaterThan(Constant(5), Constant(3)),
      LessThan(Constant(1), Constant(2)))), true)
  }

  def testSimple() {
    val state = State(2, 2, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("FORWARD").get, state)
    assertEquals(State(2, 1, 0), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 2), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 3), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 0), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 3), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 2), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 0), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("FORWARD 2").get, state)
    assertEquals(State(4, 1, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("FORWARD (1+1)").get, state)
    assertEquals(State(6, 1, 1), state)
  }

  def testIfThenElse() {
    val state = State(2, 2, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE { RIGHT }").get, state)
    assertEquals(State(2, 1, 0), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE { RIGHT }").get, state)
    assertEquals(State(2, 1, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDX < 3) { FORWARD } ELSE { LEFT }").get, state)
    assertEquals(State(3, 1, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDX < 3) { FORWARD } ELSE { LEFT }").get, state)
    assertEquals(State(3, 1, 0), state)
  }

  def testIfElseIf() {
    val state = State(2, 2, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY <> 2) { FORWARD } ELSE IF (GRIDY = 2) { RIGHT }").get, state)
    assertEquals(State(2, 2, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE IF (GRIDY <> 2) { RIGHT }").get, state)
    assertEquals(State(3, 2, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY <> 2) { FORWARD } ELSE IF (GRIDY = 2) { RIGHT }").get, state)
    assertEquals(State(3, 2, 2), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE IF (GRIDY <> 2) { RIGHT }").get, state)
    assertEquals(State(3, 3, 2), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE IF (GRIDX = 2) { RIGHT }").get, state)
    assertEquals(State(3, 3, 2), state)
  }

  def testStack() {
    val state = State(2, 2, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("PUSH (1+2)").get, state)
    assertEquals(3, state.top)
    new Evaluator(DefaultEnvironment).evaluate(parse("PUSH (TOP + 1)").get, state)
    assertEquals(4, state.top)
    new Evaluator(DefaultEnvironment).evaluate(parse("POP").get, state)
    assertEquals(3, state.top)
    new Evaluator(DefaultEnvironment).evaluate(parse("REPLACE (TOP * 2)").get, state)
    assertEquals(6, state.top)
  }

  def testWhileStack() {
    val state = State(2, 2, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("""|PUSH 10
    				  |WHILE (TOP > 3) {
    			      |  REPLACE (TOP - 1)
                      |}""".stripMargin).get, state)
    assertEquals(3, state.top)
  }

  def testGridXY() {
    val state = State(2, 3, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("""|PUSH GRIDX
    				  |PUSH GRIDY""".stripMargin).get, state)
    assertEquals(3, state.pop)
    assertEquals(2, state.pop)
  }

  def testWhileGrid() {
    val state = State(2, 2, 1)
    new Evaluator(DefaultEnvironment).evaluate(parse("""|WHILE (GRIDX < 10) {
		  			    | FORWARD 
			  			|}""".stripMargin).get, state)
    assertEquals(State(10, 2, 1), state)
    new Evaluator(DefaultEnvironment).evaluate(parse("""|RIGHT
			  		    |WHILE (GRIDY < 10) {
		  			    | FORWARD 
			  			|}""".stripMargin).get, state)
    assertEquals(State(10, 10, 2), state)
  }

  def testDeltaXY() {
    val state = State(2, 3, 0)
    new Evaluator(DefaultEnvironment).evaluate(parse("PUSH DELTAX").get, state)
    assertEquals(0, state.top)
    new Evaluator(DefaultEnvironment).evaluate(parse("PUSH DELTAY").get, state)
    assertEquals(-1, state.top)
    new Evaluator(DefaultEnvironment).evaluate(parse("""|
    			      |RIGHT
    			      |PUSH DELTAX""".stripMargin).get, state)
    assertEquals(1, state.top)
    new Evaluator(DefaultEnvironment).evaluate(parse("PUSH DELTAY").get, state)
    assertEquals(0, state.top)

  }

  def testAbs() {
    executeIntExpressionTest("ABS(-1)", Abs(Constant(-1)), 1)
    executeIntExpressionTest("ABS(1)", Abs(Constant(1)), 1)
    executeIntExpressionTest("ABS(3-5)", Abs(Subtract(List(Constant(3), Constant(5)))), 2)
    executeIntExpressionTest("-ABS(3-5)", Negate(Abs(Subtract(List(Constant(3), Constant(5))))), -2)
  }

  def testMax() {
    executeIntExpressionTest("MAX(1, 2)", Max(Constant(1), Constant(2)), 2)
    executeIntExpressionTest("MAX(1, -2)", Max(Constant(1), Constant(-2)), 1)
    executeIntExpressionTest("MAX(-1, -2)", Max(Constant(-1), Constant(-2)), -1)
    executeIntExpressionTest("-MAX(-1, -2)", Negate(Max(Constant(-1), Constant(-2))), 1)
  }

  def testMin() {
    executeIntExpressionTest("MIN(1, 2)", Min(Constant(1), Constant(2)), 1)
    executeIntExpressionTest("MIN(1, -2)", Min(Constant(1), Constant(-2)), -2)
    executeIntExpressionTest("MIN(-1, -2)", Min(Constant(-1), Constant(-2)), -2)
    executeIntExpressionTest("-MIN(-1, -2)", Negate(Min(Constant(-1), Constant(-2))), 2)
  }

  def testBoundedEnvironment() {
    val evaluator = new Evaluator(new BoundedEnvironment(10, 10))
    val state = State(2, 2, 0)
    evaluator.evaluate(parse("FORWARD").get, state)
    assertEquals(State(2, 1, 0), state)
    evaluator.evaluate(parse("FORWARD").get, state)
    assertEquals(State(2, 0, 0), state)
    evaluator.evaluate(parse("FORWARD").get, state)
    assertEquals(State(2, 0, 0), state)
    evaluator.evaluate(parse("LEFT FORWARD 2").get, state)
    assertEquals(State(0, 0, 3), state)
    evaluator.evaluate(parse("FORWARD 2").get, state)
    assertEquals(State(0, 0, 3), state)
    evaluator.evaluate(parse("LEFT FORWARD 9").get, state)
    assertEquals(State(0, 9, 2), state)
    evaluator.evaluate(parse("FORWARD").get, state)
    assertEquals(State(0, 9, 2), state)
    evaluator.evaluate(parse("LEFT FORWARD 9").get, state)
    assertEquals(State(9, 9, 1), state)
    evaluator.evaluate(parse("FORWARD").get, state)
    assertEquals(State(9, 9, 1), state)
    evaluator.evaluate(parse("LEFT FORWARD 99").get, state)
    assertEquals(State(9, 0, 0), state)
  }

  def testPaint() {

    val environment = new Environment {
      import scala.collection.mutable.ListBuffer

      val paintedTuples = new ListBuffer[(Int, Int, Int)]

      override def paint(color: Int, x: Int, y: Int) {
        paintedTuples += (x, y, color)
      }
    }
    val evaluator = new Evaluator(environment)
    evaluator.evaluate(parse("PAINT 1").get, new State(2, 2, 0))
    assertEquals((2, 2, 1), environment.paintedTuples(0))
    evaluator.evaluate(parse("PAINT (1 + 2)").get, new State(4, 4, 0))
    assertEquals((4, 4, 3), environment.paintedTuples(1))
    evaluator.evaluate(parse("PAINT").get, new State(5, 5, 0))
    assertEquals((5, 5, 0), environment.paintedTuples(2))
  }

  def testPainted() {

    val environment = new Environment {
      private val painted = (3, 4)

      override def isPainted(x: Int, y: Int) = (x, y) == painted
    }
    val evaluator = new Evaluator(environment)
    val state = new State(2, 2, 0)
    evaluator.evaluate(parse("""IF (PAINTED(1,2)) { FORWARD }""").get, state)
    assertEquals(State(2, 2, 0), state)
    evaluator.evaluate(parse("""IF (PAINTED(3,4)) { FORWARD }""").get, state)
    assertEquals(State(2, 1, 0), state)
  }

  def testNot() {
    executeBooleanLogicTest("(NOT(4 > 3))", Not(GreaterThan(Constant(4), Constant(3))), false)
    executeBooleanLogicTest("(NOT((1 + 3) <> (0 - -4)))", Not(NotEqual(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4))))), true)
  }

  def testPopOnEmptyStack() {
    val state = new State(2, 2, 0)
    val evaluator = new Evaluator(DefaultEnvironment)
    evaluator.evaluate(parse("POP").get, state)
    assertEquals(Some(IllegalOperationOnEmptyStack), state.abend)
  }

  def testTopOnEmptyStack() {
    val state = new State(2, 2, 0)
    val evaluator = new Evaluator(DefaultEnvironment)
    evaluator.evaluate(parse("PUSH TOP").get, state)
    assertEquals(Some(IllegalOperationOnEmptyStack), state.abend)
  }

  def testAdajacent() {
    val state = new State(2, 2, 1)
    val evaluator = new Evaluator(new Environment {
      override def adjacent(entity: String, state: State) =
        "ROCK" == entity && ((Math.abs(3 - state.gridX) + Math.abs(3 - state.gridY)) == 1)
    })
    evaluator.evaluate(parse("""IF (ADJACENT(ROCK)) { FORWARD }""").get, state)
    assertEquals(State(2, 2, 1), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(2, 2, 1), state)
    evaluator.evaluate(parse("""IF (NOT(ADJACENT(ROCK))) { FORWARD }""").get, state)
    assertEquals(State(3, 2, 1), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(3, 2, 1), state)
    evaluator.evaluate(parse("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""").get, state)
    assertEquals(State(4, 2, 2), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(4, 2, 2), state)
    evaluator.evaluate(parse("""IF (NOT(ADJACENT(ROCK))) { FORWARD  }""").get, state)
    assertEquals(State(4, 3, 2), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(4, 3, 2), state)
    evaluator.evaluate(parse("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""").get, state)
    assertEquals(State(4, 4, 3), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(4, 4, 3), state)
    evaluator.evaluate(parse("""IF (NOT(ADJACENT(ROCK))) { FORWARD  }""").get, state)
    assertEquals(State(3, 4, 3), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(3, 4, 3), state)
    evaluator.evaluate(parse("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""").get, state)
    assertEquals(State(2, 4, 0), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(2, 4, 0), state)
    evaluator.evaluate(parse("""IF (NOT(ADJACENT(ROCK))) { FORWARD }""").get, state)
    assertEquals(State(2, 3, 0), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(2, 3, 0), state)
    evaluator.evaluate(parse("""IF (ADJACENT(ROCK)) { FORWARD }""").get, state)
    assertEquals(State(2, 2, 0), state)
    evaluator.evaluate(parse("""IF (ADJACENT(FLAG)) { FORWARD }""").get, state)
    assertEquals(State(2, 2, 0), state)
  }

  def testDistances() {
    val state = new State(2, 3, 1)
    val evaluator = new Evaluator(new Environment {
      private val entityMap = Map("ROCK" -> (5, 5),
        "FLAG" -> (1, 1))

      override def distanceX(entity: String, state: State) = if (entityMap.contains(entity)) {Some(entityMap(entity)._1 - state.gridX)} else {None}

      override def distanceY(entity: String, state: State) = if (entityMap.contains(entity)) {Some(entityMap(entity)._2 - state.gridY)} else {None}
    })
    evaluator.evaluate(parse("PUSH DISTANCEX(ROCK)").get, state)
    assertEquals(3, state.top)
    assertEquals(false, state.stopped)
    assertEquals(None, state.abend)
    evaluator.evaluate(parse("PUSH DISTANCEY(ROCK)").get, state)
    assertEquals(2, state.top)
    assertEquals(false, state.stopped)
    assertEquals(None, state.abend)
    evaluator.evaluate(parse("PUSH DISTANCEX(FLAG)").get, state)
    assertEquals(-1, state.top)
    assertEquals(false, state.stopped)
    assertEquals(None, state.abend)
    evaluator.evaluate(parse("PUSH DISTANCEY(FLAG)").get, state)
    assertEquals(-2, state.top)
    assertEquals(false, state.stopped)
    assertEquals(None, state.abend)
    evaluator.evaluate(parse("PUSH DISTANCEX(FOO)").get, state)
    assertEquals(true, state.stopped)
    assertEquals(Some(UnknownEntity("FOO")), state.abend)
  }

  def testDefCall {
    val state = State(2, 2, 0)
    val evaluator = new Evaluator(DefaultEnvironment)
    evaluator.evaluate(parse("""|DEF RIGHT_FORWARD { RIGHT FORWARD }
     							 |DEF LEFT_FORWARD { LEFT FORWARD }
     							 |DEF EMPTY { } 
     						     |CALL RIGHT_FORWARD""".stripMargin).get, state)
    assertEquals(State(3, 2, 1), state)
    evaluator.evaluate(parse("CALL LEFT_FORWARD").get, state)
    assertEquals(State(3, 1, 0), state)
    evaluator.evaluate(parse("CALL EMPTY").get, state)
    assertEquals(State(3, 1, 0), state)
    evaluator.evaluate(parse("CALL FOO").get, state)
    assertEquals(State(3, 1, 0), state)
    assertEquals(true, state.stopped)
    assertEquals(Some(UndefinedBlock("FOO")), state.abend)
  }

  def testPrint() {

    val controller = new Controller(DefaultEnvironment) {
      var lastPrint: String = null

      override def print(value: String) {lastPrint = value}
    }
    val evaluator = new Evaluator(DefaultEnvironment, controller)
    val state = new State(2, 3, 0)
    evaluator.evaluate(parse("""PRINT "GRIDX = " + GRIDX + " GRIDY = " + GRIDY + " " + ((2+2) = 4) + " foo" """).get, state)
    assertEquals("GRIDX = 2 GRIDY = 3 true foo", controller.lastPrint)
  }

  def testPaintColor() {
    val environment = new BoundedEnvironment(10,10) {
        override def paintColor(x:Int, y:Int, state:State) =
          if ((x,y) == (2,2)) {
            Some(3)
          } else {
            super.paintColor(x, y, state)
        }
    }
    val evaluator = new Evaluator(environment)
    val state = new State(2, 2, 0)
    evaluator.evaluate(parse("""PUSH PAINTCOLOR(GRIDX, GRIDY)""").get, state)
    assertEquals(None, state.abend)
    assertEquals(3, state.top)
    evaluator.evaluate(parse("""FORWARD PUSH PAINTCOLOR(GRIDX, GRIDY)""").get, state)
    assertEquals(Some(NotPainted), state.abend)
  }
}
