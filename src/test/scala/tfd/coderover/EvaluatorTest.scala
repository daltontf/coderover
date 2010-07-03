package tfd.coderover

import junit.framework._
import org.junit.Assert._

class EvaluatorTest extends TestCase {
  private[this] val languageParser = new LanguageParser()
  private[this] val evaluator = new Evaluator()

  import languageParser._

  private def evaluateInt(intExpression:IntExpression, args:Array[Int] = Array.empty[Int], controller:Controller):ResultOrAbend[Int] =
      evaluator.evaluateInt(intExpression, args, controller)

  private def evaluateInt(intExpression:IntExpression, args:Array[Int], state:State):ResultOrAbend[Int] =
      evaluateInt(intExpression, args, new Controller(state, DefaultEnvironment))

  private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int] = Array.empty[Int], controller:Controller):ResultOrAbend[Boolean] =
      evaluator.evaluateBoolean(booleanExpression, args, controller)

  private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], state:State):ResultOrAbend[Boolean] =
      evaluateBoolean(booleanExpression, args, new Controller(state, DefaultEnvironment))

  private def evaluate(instructions:String, controller:Controller):ResultOrAbend[Unit] =
      evaluator.evaluate(parse(instructions).get, controller)

  private def evaluate(instructions:String, state:State):ResultOrAbend[Unit] =
      evaluator.evaluate(parse(instructions).get, new Controller(state, DefaultEnvironment))

  private def evaluate(instructions:List[Instruction], state:State):ResultOrAbend[Unit] =
      evaluator.evaluate(instructions, new Controller(state, DefaultEnvironment))

  private def executeConstantTest(stringInput: String, expectedConstant: Constant, expectedInt: Int) {
    val ast = parseAll(constant, stringInput).get
    assertEquals(expectedConstant, ast)
    assertEquals(ResultOrAbend(Some(expectedInt), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeMathematicalTest(stringInput: String, expectedMathematical: Mathematical, expectedInt: Int) {
    val ast = parseAll(mathematical, stringInput).get
    assertEquals(expectedMathematical, ast)
    assertEquals(ResultOrAbend(Some(expectedInt), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeComparisonTest(stringInput: String, expectedComparison: Comparison, expectedBoolean: Boolean) {
    val ast = parseAll(comparison, stringInput).get
    assertEquals(expectedComparison, ast)
    assertEquals(ResultOrAbend(Some(expectedBoolean), None), evaluateBoolean(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeBooleanLogicTest(stringInput: String, expectedBooleanLogic: BooleanExpression, expectedBoolean: Boolean) {
    val ast = parseAll(booleanExpression, stringInput).get
    assertEquals(expectedBooleanLogic, ast)
    assertEquals(ResultOrAbend(Some(expectedBoolean), None), evaluateBoolean(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeIntExpressionTest(stringInput: String, expectedAst: Expression, expectedIntResult: Int) {
    val ast = parseAll(intExpression, stringInput).get
    assertEquals(expectedAst, ast)
    assertEquals(ResultOrAbend(Some(expectedIntResult), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  def testEmpty() {
    assertEquals(SuccessResultUnit, evaluate("", State(0,0,0)))
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
    List("PUSH (2/0)",
         "PRINT (2/0)"
      ).map { code:String =>
        assertEquals(ResultOrAbend(None, Some(DivideByZero)), evaluate(code, State(2, 2, 0)))
      }
  }

  def testModByZero() {
    assertEquals(ResultOrAbend(None, Some(DivideByZero)), evaluate("PUSH (2%0)", State(2, 2, 0)))
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
    val controller = new Controller(State(2, 2, 0))
    evaluate("PUSH (1+2)", controller)
    assertEquals(ResultOrAbend(3), controller.top)
    evaluate("PUSH (TOP + 1)", controller)
    assertEquals(ResultOrAbend(4), controller.top)
    evaluate("POP", controller)
    assertEquals(ResultOrAbend(3), controller.top)
    evaluate("REPLACE (TOP * 2)", controller)
    assertEquals(ResultOrAbend(6), controller.top)
  }

  def testWhileStack() {
    val controller = new Controller(State(2, 2, 0))
    evaluate("""|PUSH 10
    				    |WHILE (TOP > 3) {
    			      |  REPLACE (TOP - 1)
                |}""".stripMargin, controller)
    assertEquals(ResultOrAbend(3), controller.top)
  }

  def testGridXY() {
    val controller = new Controller(State(2, 3, 0))
    evaluate("PUSH X PUSH Y", controller)
    assertEquals(ResultOrAbend(3), controller.top)
    evaluate("POP", controller)
    assertEquals(ResultOrAbend(2), controller.top)
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
    val controller = new Controller(new State(2, 3, 0))
    evaluate("PUSH DX", controller)
    assertEquals(ResultOrAbend(0), controller.top)
    evaluate("PUSH DY", controller)
    assertEquals(ResultOrAbend(-1), controller.top)
    evaluate("""|
    			      |RIGHT
    			      |PUSH DX""".stripMargin, controller)
    assertEquals(ResultOrAbend(1), controller.top)
    evaluate("PUSH DY", controller)
    assertEquals(ResultOrAbend(0), controller.top)
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
    evaluate("LEFT REPEAT 2 { FORWARD }", state)
    assertEquals(State(0, 0, 3), state)
    evaluate("REPEAT 2 { FORWARD }", state)
    assertEquals(State(0, 0, 3), state)
    evaluate("LEFT REPEAT 9 { FORWARD }", state)
    assertEquals(State(0, 9, 2), state)
    evaluate("FORWARD", state)
    assertEquals(State(0, 9, 2), state)
    evaluate("LEFT REPEAT 9 { FORWARD }", state)
    assertEquals(State(9, 9, 1), state)
    evaluate("FORWARD", state)
    assertEquals(State(9, 9, 1), state)
    evaluate("LEFT REPEAT 99 { FORWARD }", state)
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

  def testIllegalOperationOnEmptyStack() {
    List("POP",
         "PUSH TOP",
         "REPLACE 2"
      ).map { code:String =>
     assertEquals(code, ResultOrAbend(None, Some(IllegalOperationOnEmptyStack)),
          evaluate(code, new State(2, 2, 0)))
    }
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
    val controller = new Controller(state, environment)
    evaluate("PUSH DISTANCEX(ROCK)", controller)
    assertEquals(ResultOrAbend(3), controller.top)
    evaluate("PUSH DISTANCEY(ROCK)", controller)
    assertEquals(ResultOrAbend(2), controller.top)
    evaluate("PUSH DISTANCEX(FLAG)", controller)
    assertEquals(ResultOrAbend(-1), controller.top)
    evaluate("PUSH DISTANCEY(FLAG)", controller)
    assertEquals(ResultOrAbend(-2), controller.top)
    assertEquals(ResultOrAbend(UnknownEntity("FOO")), evaluate("PUSH DISTANCEX(FOO)", controller))
  }

  def testDefCall {
    val state = State(2, 2, 0)
    val controller = new Controller(state)
    assertEquals(SuccessResultUnit, evaluate(
      """|PROC RIGHTFORWARD { RIGHT FORWARD }
         |PROC LEFTFORWARD { LEFT FORWARD }
     		 |PROC EMPTY { }
     		 |RIGHTFORWARD""".stripMargin, controller))
    assertEquals(State(3, 2, 1), state)
    assertEquals(SuccessResultUnit, evaluate("LEFTFORWARD", controller))
    assertEquals(State(3, 1, 0), state)
    assertEquals(SuccessResultUnit, evaluate("EMPTY", controller))
    assertEquals(State(3, 1, 0), state)
    assertEquals(ResultOrAbend(None, Some(UndefinedBlock("FOO"))), evaluate("FOO", controller))
    assertEquals(State(3, 1, 0), state)
  }

  def testProcCallWithParams {
    val state = State(2, 2, 0)
    val controller = new Controller(state)
    assertEquals(SuccessResultUnit, evaluate("""|PROC RFORWARD { RIGHT REPEAT $1 { FORWARD } }
     					  |PROC LFORWARD { LEFT REPEAT $1 { FORWARD } }
     						|PROC EMPTY { }
     						|PROC LLFORWARD { LEFT REPEAT $1 {FORWARD} LEFT REPEAT $2 {FORWARD} }
     						|RFORWARD(1)""".stripMargin, controller))
    assertEquals(State(3, 2, 1), state)
    assertEquals(SuccessResultUnit, evaluate("LFORWARD(2)", controller))
    assertEquals(State(3, 0, 0), state)
    assertEquals(SuccessResultUnit, evaluate("LFORWARD(3)", controller))
    assertEquals(State(0, 0, 3), state)
    assertEquals(SuccessResultUnit, evaluate("LLFORWARD(4, 5)", controller))
    assertEquals(State(5, 4, 1), state)
    assertEquals(SuccessResultUnit, evaluate("EMPTY", controller))
    assertEquals(State(5, 4, 1), state)
    assertEquals(ResultOrAbend(None, Some(UndefinedBlock("FOO"))), evaluate("FOO", controller))
    assertEquals(State(5, 4, 1), state)
  }

  def testFuncInvoke {
      val state = State(2, 2, 0)
      val controller = new Controller(state)
      assertEquals(SuccessResultUnit, evaluate("""
        |FUNC PLUSXY ( X + Y )
        |RIGHT
        |REPEAT PLUSXY {FORWARD}""".stripMargin, controller))
      assertEquals(State(6, 2, 1), state)    
  }

  def testUnboundParams {
    assertEquals(ResultOrAbend(None,Some(UnboundParameter(1))), evaluate("PUSH($1)", State(2, 2, 0)))
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
    val controller = new Controller(new State(2, 2, 0), DefaultEnvironment, new Constraints(10, 10, 10))
    assertEquals(SuccessResultUnit, evaluate("""STORE (3,42) PUSH MEM(3)""", controller))
    assertEquals(ResultOrAbend(42), controller.top)
    assertEquals(42, controller.memory(3))
    assertEquals(ResultOrAbend(InvalidMEMAddress(10)), evaluate("""STORE (10,42)""", controller))
    assertEquals(ResultOrAbend(InvalidMEMAddress(11)), evaluate("""PUSH MEM(11)""", controller))
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
    |PROC FOO { PUSH 2 POP }
    |PROC BAR { PUSH 1 FOO POP }
    |BAR""".stripMargin, controller)
    //assertEquals(Some(CallStackOverflow), state.abend)
  }

  def testObstructions() {
    val state = new State(2, 2, 2)
    val environment = new Environment(10, 10) {
      override def isObstructed(x: Int, y: Int) = (x,y) == (2,3)
    }
    val controller = new Controller(state, environment)
    assertEquals(SuccessResultUnit, evaluate("FORWARD", controller))
    assertEquals(State(2,2,2), state)
  }

  def testObstructed() {
    val state = new State(0, 0, 0)
    val environment = new Environment(3, 3) {
      override def isObstructed(x: Int, y: Int) = (x,y) == (1,1)
    }
    val controller = new Controller(state, environment)

    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,0,1), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(1,0,1), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,0,1), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,0,2), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,1,2), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,2,2), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,2,3), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(1,2,3), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,2,3), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,2,0), state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,1,0), state)
    // Face obstructed square first
    assertEquals(SuccessResultUnit, evaluate("RIGHT IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,1,2), state)
    // Test outside grid boundaries
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(-1,0)) { RIGHT }", controller))
    assertEquals(State(0,1,3), state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(0,-1)) { RIGHT }", controller))
    assertEquals(State(0,1,0), state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(0,0)) { RIGHT }", controller))
    assertEquals(State(0,1,0), state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(2,3)) { RIGHT }", controller))
    assertEquals(State(0,1,1), state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(3,2)) { RIGHT }", controller))
    assertEquals(State(0,1,2), state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(2,2)) { RIGHT }", controller))
    assertEquals(State(0,1,2), state)
  }

  def testPostMoveForward() {
    object Kablooey extends Abend("Kablooey")
    
    val state = new State(0, 0, 0)
    val environment = new Environment(3, 3) {


      override def postMoveForward(state:State):Option[Abend] = {

        state match {
          case State(1,1,_) => Some(Kablooey)
          case _ => None
        }
      }
    }
    val controller = new Controller(state, environment)
    assertEquals(SuccessResultUnit, evaluate("RIGHT FORWARD RIGHT", controller))
    assertEquals(State(1,0,2), state)
    assertEquals(ResultOrAbend[Unit](None, Some(Kablooey)), evaluate("FORWARD", controller))
  }

  def testTernary() {
    val controller = new Controller(State(2, 2, 0))
    evaluate("PUSH ((1 < 2) ? 1 : 2)", controller)
    assertEquals(ResultOrAbend(1), controller.top)
    evaluate("PUSH ((1 > 2) ? 1 : 2)", controller)
    assertEquals(ResultOrAbend(2), controller.top)  
  }

  def testRecursiveFunc() {
    val controller = new Controller(new State(2, 3, 0)) {
      var lastPrint: String = null

      override def print(value: String) {lastPrint = value}
    }
    evaluate("""
      |FUNC FACTORIAL ( ($1 = 1) ? 1 : $1 * FACTORIAL($1 - 1))
      |PRINT "FACTORIAL(6) = " + FACTORIAL(6)""".stripMargin, controller)
    assertEquals("FACTORIAL(6) = 720", controller.lastPrint)
  }

  def testPred() {
    val state = new State(2, 2, 0)
    val controller = new Controller(state)
    evaluate("""
      |PRED Y_EQUALS (Y = $1)
      |IF Y_EQUALS(2) { FORWARD }""".stripMargin, controller)
    assertEquals(State(2,1,0), state)
    evaluate("""IF Y_EQUALS(2) { FORWARD }""".stripMargin, controller)
    assertEquals(State(2,1,0), state)
    evaluate("""IF Y_EQUALS(1) { FORWARD RIGHT }""".stripMargin, controller)
    assertEquals(State(2,0,1), state)
  }
}
