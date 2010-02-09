package tfd.coderover

import junit.framework._
import org.junit.Assert._

class EvaluatorTest extends TestCase {
  private[this] val languageParser = new LanguageParser()
  private[this] val evaluator = new Evaluator()

  import languageParser._

  private def evaluateInt(intExpression:IntExpression, args:Array[Int] = Array.empty[Int], controller:Controller):EvaluationResult[Int] =
      evaluator.evaluateInt(intExpression, args, controller)

  private def evaluateInt(intExpression:IntExpression, args:Array[Int], state:State):EvaluationResult[Int] =
      evaluateInt(intExpression, args, new Controller(state, DefaultEnvironment))

  private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int] = Array.empty[Int], controller:Controller):EvaluationResult[Boolean] =
      evaluator.evaluateBoolean(booleanExpression, args, controller)

  private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], state:State):EvaluationResult[Boolean] =
      evaluateBoolean(booleanExpression, args, new Controller(state, DefaultEnvironment))

  private def evaluate(instructions:String, controller:Controller):EvaluationResult[Unit] =
      evaluator.evaluate(parse(instructions).get, controller)

  private def evaluate(instructions:String, state:State):EvaluationResult[Unit] =
      evaluator.evaluate(parse(instructions).get, new Controller(state, DefaultEnvironment))

  private def evaluate(instructions:List[Instruction], state:State):EvaluationResult[Unit] = 
      evaluator.evaluate(instructions, new Controller(state, DefaultEnvironment))

  private def executeConstantTest(stringInput: String, expectedConstant: Constant, expectedInt: Int) {
    val ast = parseAll(constant, stringInput).get
    assertEquals(expectedConstant, ast)
    assertEquals(EvaluationResult(Some(expectedInt), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeMathematicalTest(stringInput: String, expectedMathematical: Mathematical, expectedInt: Int) {
    val ast = parseAll(mathematical, stringInput).get
    assertEquals(expectedMathematical, ast)
    assertEquals(EvaluationResult(Some(expectedInt), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeComparisonTest(stringInput: String, expectedComparison: Comparison, expectedBoolean: Boolean) {
    val ast = parseAll(comparison, stringInput).get
    assertEquals(expectedComparison, ast)
    assertEquals(EvaluationResult(Some(expectedBoolean), None), evaluateBoolean(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeBooleanLogicTest(stringInput: String, expectedBooleanLogic: BooleanExpression, expectedBoolean: Boolean) {
    val ast = parseAll(booleanExpression, stringInput).get
    assertEquals(expectedBooleanLogic, ast)
    assertEquals(EvaluationResult(Some(expectedBoolean), None), evaluateBoolean(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeIntExpressionTest(stringInput: String, expectedAst: Expression, expectedIntResult: Int) {
    val ast = parseAll(intExpression, stringInput).get
    assertEquals(expectedAst, ast)
    assertEquals(EvaluationResult(Some(expectedIntResult), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  def testEmpty() {
    assertEquals(EvaluationSuccessUnit, evaluate("", State(0,0,0)))
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

  def testDivByZero() {
    assertEquals(EvaluationResult(None, Some(DivideByZero)), evaluate("PUSH (2/0)", State(2, 2, 0)))
  }

  def testModByZero() {
    assertEquals(EvaluationResult(None, Some(DivideByZero)), evaluate("PUSH (2%0)", State(2, 2, 0)))
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
    evaluate("FORWARD", state)
    assertEquals(State(2, 1, 0), state)
    evaluate("RIGHT", state)
    assertEquals(State(2, 1, 1), state)
    evaluate("RIGHT", state)
    assertEquals(State(2, 1, 2), state)
    evaluate("RIGHT", state)
    assertEquals(State(2, 1, 3), state)
    evaluate("RIGHT", state)
    assertEquals(State(2, 1, 0), state)
    evaluate("LEFT", state)
    assertEquals(State(2, 1, 3), state)
    evaluate("LEFT", state)
    assertEquals(State(2, 1, 2), state)
    evaluate("LEFT", state)
    assertEquals(State(2, 1, 1), state)
    evaluate("LEFT", state)
    assertEquals(State(2, 1, 0), state)
    evaluate("RIGHT", state)
    assertEquals(State(2, 1, 1), state)
    evaluate("FORWARD 2", state)
    assertEquals(State(4, 1, 1), state)
    evaluate("FORWARD (1+1)", state)
    assertEquals(State(6, 1, 1), state)
  }

  def testIfThenElse() {
    val state = State(2, 2, 0)
    evaluate("IF (Y = 2) { FORWARD } ELSE { RIGHT }", state)
    assertEquals(State(2, 1, 0), state)
    evaluate("IF (Y = 2) { FORWARD } ELSE { RIGHT }", state)
    assertEquals(State(2, 1, 1), state)
    evaluate("IF (X < 3) { FORWARD } ELSE { LEFT }", state)
    assertEquals(State(3, 1, 1), state)
    evaluate("IF (X < 3) { FORWARD } ELSE { LEFT }", state)
    assertEquals(State(3, 1, 0), state)
  }

  def testIfElseIf() {
    val state = State(2, 2, 0)
    evaluate("IF (Y <> 2) { FORWARD } ELSE IF (Y = 2) { RIGHT }", state)
    assertEquals(State(2, 2, 1), state)
    evaluate("IF (Y = 2) { FORWARD } ELSE IF (Y <> 2) { RIGHT }", state)
    assertEquals(State(3, 2, 1), state)
    evaluate("IF (Y <> 2) { FORWARD } ELSE IF (Y = 2) { RIGHT }", state)
    assertEquals(State(3, 2, 2), state)
    evaluate("IF (Y = 2) { FORWARD } ELSE IF (Y <> 2) { RIGHT }", state)
    assertEquals(State(3, 3, 2), state)
    evaluate("IF (Y = 2) { FORWARD } ELSE IF (X = 2) { RIGHT }", state)
    assertEquals(State(3, 3, 2), state)
  }

  def testStack() {
    val state = State(2, 2, 0)
    evaluate("PUSH (1+2)", state)
    //assertEquals(3, state.top)
    evaluate("PUSH (TOP + 1)", state)
    //assertEquals(4, state.top)
    evaluate("POP", state)
    //assertEquals(3, state.top)
    evaluate("REPLACE (TOP * 2)", state)
    //assertEquals(6, state.top)
  }

  def testWhileStack() {
    val state = State(2, 2, 0)
    evaluate("""|PUSH 10
    				  |WHILE (TOP > 3) {
    			      |  REPLACE (TOP - 1)
                      |}""".stripMargin, state)
    //assertEquals(3, state.top)
  }

  def testGridXY() {
    val state = State(2, 3, 0)
    evaluate("""|PUSH X
    				  |PUSH Y""".stripMargin, state)
    //assertEquals(3, state.pop)
    //assertEquals(2, state.pop)
  }

  def testWhileGrid() {
    val state = State(2, 2, 1)
    evaluate("""|WHILE (X < 10) {
		  			    | FORWARD 
		  	  			|}""".stripMargin, new Controller(state, new Environment(11,11)))
    assertEquals(State(10, 2, 1), state)
    evaluate("""|RIGHT
			  		    |WHILE (Y < 10) {
		  			    | FORWARD 
		  	  			|}""".stripMargin, new Controller(state, new Environment(11,11)))
    assertEquals(State(10, 10, 2), state)
  }

  def testDeltaXY() {
    val state = State(2, 3, 0)
    evaluate("PUSH DX", state)
    //assertEquals(0, state.top)
    evaluate("PUSH DY", state)
    //assertEquals(-1, state.top)
    evaluate("""|
    			      |RIGHT
    			      |PUSH DX""".stripMargin, state)
    //assertEquals(1, state.top)
    evaluate("PUSH DY", state)
    //assertEquals(0, state.top)

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
    val state = State(2, 2, 0)
    evaluate("FORWARD", state)
    assertEquals(State(2, 1, 0), state)
    evaluate("FORWARD", state)
    assertEquals(State(2, 0, 0), state)
    evaluate("FORWARD", state)
    assertEquals(State(2, 0, 0), state)
    evaluate("LEFT FORWARD 2", state)
    assertEquals(State(0, 0, 3), state)
    evaluate("FORWARD 2", state)
    assertEquals(State(0, 0, 3), state)
    evaluate("LEFT FORWARD 9", state)
    assertEquals(State(0, 9, 2), state)
    evaluate("FORWARD", state)
    assertEquals(State(0, 9, 2), state)
    evaluate("LEFT FORWARD 9", state)
    assertEquals(State(9, 9, 1), state)
    evaluate("FORWARD", state)
    assertEquals(State(9, 9, 1), state)
    evaluate("LEFT FORWARD 99", state)
    assertEquals(State(9, 0, 0), state)
  }

  def testPaint() {

    val environment = new Environment(10,10) {
      import scala.collection.mutable.ListBuffer

      val paintedTuples = new ListBuffer[(Int, Int)]

      override def paint(state:State) {
        paintedTuples += ((state.gridX, state.gridY))
      }
    }
    val state = new State(2,2,0)
    val controller = new Controller(state, environment)
    evaluate("PAINT", controller)
    assertEquals((2, 2), environment.paintedTuples(0))
    evaluate("FORWARD PAINT", controller)
    assertEquals((2, 1), environment.paintedTuples(1))
    evaluate("RIGHT FORWARD PAINT", controller)
    assertEquals((3, 1), environment.paintedTuples(2))
  }

  def testPainted() {

    val environment = new Environment(10,10) {
      private val painted = (3, 4)

      override def isPainted(x: Int, y: Int, state:State) = (x, y) == painted
    }
    val state = new State(2, 2, 0)
    evaluate("""IF (PAINTED(1,2)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 2, 0), state)
    evaluate("""IF (PAINTED(3,4)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 1, 0), state)
  }

  def testNot() {
    executeBooleanLogicTest("NOT(4 > 3)", Not(GreaterThan(Constant(4), Constant(3))), false)
    executeBooleanLogicTest("NOT((1 + 3) <> (0 - -4))", Not(NotEqual(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4))))), true)
  }

  def testPopOnEmptyStack() {
    val state = new State(2, 2, 0)
    assertEquals(EvaluationResult(None, Some(IllegalOperationOnEmptyStack)),
          evaluate("POP", state))
  }

  def testTopOnEmptyStack() {
    val state = new State(2, 2, 0)
    assertEquals(EvaluationResult(None, Some(IllegalOperationOnEmptyStack)),
          evaluate("PUSH TOP", state))
  }

  def testAdjacent() {
    val state = new State(2, 2, 1)
    val environment = new Environment(10,10) {
      override def adjacent(entity: String, state: State) =
        "ROCK" == entity && ((Math.abs(3 - state.gridX) + Math.abs(3 - state.gridY)) == 1)
    }
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 2, 1), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 2, 1), state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(3, 2, 1), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(3, 2, 1), state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""", new Controller(state, environment))
    assertEquals(State(4, 2, 2), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(4, 2, 2), state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD  }""", new Controller(state, environment))
    assertEquals(State(4, 3, 2), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(4, 3, 2), state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""", new Controller(state, environment))
    assertEquals(State(4, 4, 3), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(4, 4, 3), state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD  }""", new Controller(state, environment))
    assertEquals(State(3, 4, 3), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(3, 4, 3), state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""", new Controller(state, environment))
    assertEquals(State(2, 4, 0), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 4, 0), state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 3, 0), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 3, 0), state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 2, 0), state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", new Controller(state, environment))
    assertEquals(State(2, 2, 0), state)
  }

  def testDistances() {
    val state = new State(2, 3, 1)
    val environment = new Environment(10,10) {
      private val entityMap = Map("ROCK" -> (5, 5),
        "FLAG" -> (1, 1))

      override def distanceX(entity: String, state: State) = if (entityMap.contains(entity)) {Some(entityMap(entity)._1 - state.gridX)} else {None}

      override def distanceY(entity: String, state: State) = if (entityMap.contains(entity)) {Some(entityMap(entity)._2 - state.gridY)} else {None}
    }
    evaluate("PUSH DISTANCEX(ROCK)", new Controller(state, environment))
    //assertEquals(3, state.top)
    //assertEquals(false, state.stopped)
    //assertEquals(None, state.abend)
    evaluate("PUSH DISTANCEY(ROCK)", new Controller(state, environment))
    //assertEquals(2, state.top)
    //assertEquals(false, state.stopped)
    //assertEquals(None, state.abend)
    evaluate("PUSH DISTANCEX(FLAG)", new Controller(state, environment))
    //assertEquals(-1, state.top)
    //assertEquals(false, state.stopped)
    //assertEquals(None, state.abend)
    evaluate("PUSH DISTANCEY(FLAG)", new Controller(state, environment))
    //assertEquals(-2, state.top)
    //assertEquals(false, state.stopped)
    //assertEquals(None, state.abend)
    //assertEquals(EvaluationResult(None, Some(UnknownEntity("FOO"))), evaluator.evaluate("PUSH DISTANCEX(FOO)", state))
  }

  def testDefCall {
    val state = State(2, 2, 0)
    val controller = new Controller(state)
    assertEquals(EvaluationSuccessUnit, evaluate(
      """|DEF RIGHT_FORWARD { RIGHT FORWARD }
         |DEF LEFT_FORWARD { LEFT FORWARD }
     		 |DEF EMPTY { }
     		 |CALL RIGHT_FORWARD""".stripMargin, controller))
    assertEquals(State(3, 2, 1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("CALL LEFT_FORWARD", controller))
    assertEquals(State(3, 1, 0), state)
    assertEquals(EvaluationSuccessUnit, evaluate("CALL EMPTY", controller))
    assertEquals(State(3, 1, 0), state)
    assertEquals(EvaluationResult(None, Some(UndefinedBlock("FOO"))), evaluate("CALL FOO", controller))
    assertEquals(State(3, 1, 0), state)
  }

  def testDefCallWithParams {
    val state = State(2, 2, 0)
    val controller = new Controller(state)
    assertEquals(EvaluationSuccessUnit, evaluate("""|DEF RIGHT_FORWARD { RIGHT FORWARD :1 }
     					  |DEF LEFT_FORWARD { LEFT FORWARD :1 }
     						|DEF EMPTY { }
     						|DEF LEFT_LEFT_FORWARD { LEFT FORWARD :1 LEFT FORWARD :2 }
     						|CALL RIGHT_FORWARD(1)""".stripMargin, controller))
    assertEquals(State(3, 2, 1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("CALL LEFT_FORWARD(2)", controller))
    assertEquals(State(3, 0, 0), state)
    assertEquals(EvaluationSuccessUnit, evaluate("CALL LEFT_FORWARD(3)", controller))
    assertEquals(State(0, 0, 3), state)
    assertEquals(EvaluationSuccessUnit, evaluate("CALL LEFT_LEFT_FORWARD(4, 5)", controller))
    assertEquals(State(5, 4, 1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("CALL EMPTY", controller))
    assertEquals(State(5, 4, 1), state)
    assertEquals(EvaluationResult(None, Some(UndefinedBlock("FOO"))), evaluate("CALL FOO", controller))
    assertEquals(State(5, 4, 1), state)
  }

  def testUnboundParams {
    assertEquals(EvaluationResult(None,Some(UnboundParameter(1))), evaluate("PUSH :1", State(2, 2, 0)))
  }

  def testPrint() {
    val state = new State(2, 3, 0)
    val controller = new Controller(state) {
      var lastPrint: String = null

      override def print(value: String) {lastPrint = value}
    }
    evaluate("""PRINT "X = " + X + " Y = " + Y + " " + ((2+2) = 4) + " foo" """, controller)
    assertEquals("X = 2 Y = 3 true foo", controller.lastPrint)
  }

  def testStoreMem() {
    val state = new State(2, 2, 0)
    val controller = new Controller(state)
    //, new Constraints(10, 10, 10)
    assertEquals(EvaluationSuccessUnit, evaluate("""STORE (3,42) PUSH MEM(3)""", controller))
    assertEquals(EvaluationResult(Some(42), None), controller.top)
    assertEquals(42, controller.memory(3))
    assertEquals(EvaluationSuccessUnit, evaluate("""STORE (10,42)""", controller))
    //assertEquals(Some(InvalidMEMAddress(10)), state.abend)
    //state.reset()
    assertEquals(EvaluationSuccessUnit, evaluate("""PUSH MEM(10)""", controller))
    //assertEquals(Some(InvalidMEMAddress(10)), state.abend)
  }

  def testMaxStack() {
    val state = new State(2, 2, 0)
    val controller = new Controller(state) //, new Constraints(10, 1, 10))
    evaluate("""PUSH 1""", controller)
    //assertEquals(None, state.abend)
    evaluate("""PUSH 1""", controller)
    //assertEquals(Some(StackOverflow), state.abend)
  }

  def testMaxCallStack() {
    val state = new State(2, 2, 0)
    val controller = new Controller(state) // new Constraints(10, 10, 1))
    evaluate("""
    |DEF FOO { PUSH 2 POP }
    |DEF BAR { PUSH 1 CALL FOO POP }
    |CALL BAR""".stripMargin, controller)
    //assertEquals(Some(CallStackOverflow), state.abend)
  }

  def testObstructions() {
    val state = new State(2, 2, 2)
    val environment = new Environment(10, 10, Set((2, 3)))
    val controller = new Controller(state, environment)
    assertEquals(EvaluationSuccessUnit, evaluate("FORWARD", controller))
    assertEquals(State(2,2,2), state)
  }

  def testObstructed() {
    val state = new State(0, 0, 0)
    val environment = new Environment(3, 3, Set((1, 1)))
    val controller = new Controller(state, environment)

    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,0,1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(1,0,1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,0,1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,0,2), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,1,2), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,2,2), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,2,3), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(1,2,3), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,2,3), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,2,0), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,1,0), state)
    // Face obstructed square first
    assertEquals(EvaluationSuccessUnit, evaluate("RIGHT IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,1,2), state)
    // Test outside grid boundaries
    assertEquals(EvaluationSuccessUnit, evaluate("IF (OBSTRUCTED(-1,0)) { RIGHT }", controller))
    assertEquals(State(0,1,3), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF (OBSTRUCTED(0,-1)) { RIGHT }", controller))
    assertEquals(State(0,1,0), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF (OBSTRUCTED(0,0)) { RIGHT }", controller))
    assertEquals(State(0,1,0), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF (OBSTRUCTED(2,3)) { RIGHT }", controller))
    assertEquals(State(0,1,1), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF (OBSTRUCTED(3,2)) { RIGHT }", controller))
    assertEquals(State(0,1,2), state)
    assertEquals(EvaluationSuccessUnit, evaluate("IF (OBSTRUCTED(2,2)) { RIGHT }", controller))
    assertEquals(State(0,1,2), state)
  }
}
