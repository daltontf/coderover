package tfd.coderover

import junit.framework._
import org.junit.Assert._

class EvaluatorTest extends TestCase {
	import Evaluator._
	import LanguageParser._
    
 
 private def executeConstantTest(stringInput:String, expectedConstant:Constant, expectedInt:Int) {
	val ast = parseAll(constant, stringInput).get
    assertEquals(expectedConstant, ast)
    assertEquals(expectedInt, evaluate(ast, State(0,0,0)))
 }
 
 def testConstant() {
    executeConstantTest("42", Constant(42), 42)
    executeConstantTest("-273", Constant(-273), -273)
 }
 
 private def executeMathematicalTest(stringInput:String, expectedMathematical:Mathematical, expectedInt:Int) {
    val ast = parseAll(mathematical, stringInput).get
    assertEquals(expectedMathematical, ast)
    assertEquals(expectedInt, evaluate(ast, State(0,0,0))) 
 }
  
 def testMathematical() {
      executeMathematicalTest("2 + 2", Plus(Constant(2), Constant(2)), 4)
      executeMathematicalTest("2 + -2", Plus(Constant(2), Constant(-2)), 0)
      executeMathematicalTest("-2 + 2", Plus(Constant(-2), Constant(2)), 0)
      executeMathematicalTest("-2 + -2", Plus(Constant(-2), Constant(-2)), -4)
      
      executeMathematicalTest("2 - 2", Minus(Constant(2), Constant(2)), 0)
      executeMathematicalTest("2 - -2", Minus(Constant(2), Constant(-2)), 4)
      executeMathematicalTest("-2 - 2", Minus(Constant(-2), Constant(2)), -4)
      executeMathematicalTest("-2 - -2", Minus(Constant(-2), Constant(-2)), 0)
      
      executeMathematicalTest("(2 - 3) + 4", Plus(Minus(Constant(2), Constant(3)), Constant(4)), 3)
      executeMathematicalTest("2 - (3 + 4)", Minus(Constant(2), Plus(Constant(3), Constant(4))), -5)
 } 
 
 private def executeComparisonTest(stringInput:String, expectedComparison:Comparison, expectedBoolean:Boolean) {
   val ast = parseAll(comparison, stringInput).get
   assertEquals(expectedComparison, ast)
   assertEquals(expectedBoolean, evaluate(ast, State(0,0,0)))
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
 
 private def executeBooleanLogicTest(stringInput:String, expectedBooleanLogic:BooleanLogic, expectedBoolean:Boolean) {
   val ast = parseAll(nestedBoolean, stringInput).get
   assertEquals(expectedBooleanLogic, ast)
   assertEquals(expectedBoolean, evaluate(ast, State(0,0,0)))
 } 
 
 def testBooleanLogic() {
	 executeBooleanLogicTest("((1 + 3) = (0 - -4))", Equal(Plus(Constant(1), Constant(3)), Minus(Constant(0), Constant(-4))), true)
	 executeBooleanLogicTest("((1 + 3) <> (0 - -4))", NotEqual(Plus(Constant(1), Constant(3)), Minus(Constant(0), Constant(-4))), false)
 } 
 
 def testSimple() {
    val state = State(2, 2, 0)
    evaluate(parse("FORWARD").get, state)
    assertEquals(State(2, 1, 0), state)
    evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 1), state)
    evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 2), state)
    evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 3), state)
    evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 0), state)
    evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 3), state)
    evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 2), state)
    evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 1), state)
    evaluate(parse("LEFT").get, state)
    assertEquals(State(2, 1, 0), state)
    evaluate(parse("RIGHT").get, state)
    assertEquals(State(2, 1, 1), state)
    evaluate(parse("FORWARD 2").get, state)
    assertEquals(State(4, 1, 1), state)
    evaluate(parse("FORWARD (1+1)").get, state)
    assertEquals(State(6, 1, 1), state)
  }
 
  def testIfThenElse() {
    val state = State(2, 2, 0)
    evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE { RIGHT }").get, state)
    assertEquals(State(2, 1, 0), state)
    evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE { RIGHT }").get, state)
    assertEquals(State(2, 1, 1), state)
    evaluate(parse("IF (GRIDX < 3) { FORWARD } ELSE { LEFT }").get, state)
    assertEquals(State(3, 1, 1), state)
    evaluate(parse("IF (GRIDX < 3) { FORWARD } ELSE { LEFT }").get, state)
    assertEquals(State(3, 1, 0), state)
  }
  
  def testIfElseIf() {
    val state = State(2,2,0)
    evaluate(parse("IF (GRIDY <> 2) { FORWARD } ELSE IF (GRIDY = 2) { RIGHT }").get, state)
    assertEquals(State(2, 2, 1), state)
    evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE IF (GRIDY <> 2) { RIGHT }").get, state)
    assertEquals(State(3, 2, 1), state)
    evaluate(parse("IF (GRIDY <> 2) { FORWARD } ELSE IF (GRIDY = 2) { RIGHT }").get, state)
    assertEquals(State(3, 2, 2), state)
    evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE IF (GRIDY <> 2) { RIGHT }").get, state)
    assertEquals(State(3, 3, 2), state)
    evaluate(parse("IF (GRIDY = 2) { FORWARD } ELSE IF (GRIDX = 2) { RIGHT }").get, state)
    assertEquals(State(3, 3, 2), state)
  }
 
  def testStack() {
    val state = State(2,2,0)
    evaluate(parse("PUSH (1+2)").get, state)
    assertEquals(3, state.top)
    evaluate(parse("PUSH (POP + 1)").get, state)
    assertEquals(4, state.top)
  }
  
  def testWhileStack() {
    val state = State(2,2,0)
    evaluate(parse("""|PUSH 10
    				  |WHILE (TOP > 3) {
    			      |  PUSH(POP - 1)
                      |}""".stripMargin).get, state)
    assertEquals(3, state.top)
  }
  
  def testGridXY() {
    val state = State(2,3,0)
    evaluate(parse("""|PUSH GRIDX
    				  |PUSH GRIDY""".stripMargin).get, state)
    assertEquals(3, state.pop)
    assertEquals(2, state.pop)
  }
  
  def testWhileGrid() {
	  val state = State(2,2,1)
	  evaluate(parse("""|WHILE (GRIDX < 10) {
		  			    | FORWARD 
			  			|}""".stripMargin).get, state)
	  assertEquals(State(10, 2, 1), state)
	  evaluate(parse("""|RIGHT
			  		    |WHILE (GRIDY < 10) {
		  			    | FORWARD 
			  			|}""".stripMargin).get, state)
	  assertEquals(State(10, 10, 2), state)
  }
  
  def testDeltaXY() {
    val state = State(2,3,0)
    evaluate(parse("PUSH DELTAX").get, state)
    assertEquals(0, state.top)
    evaluate(parse("PUSH DELTAY").get, state)
    assertEquals(-1, state.top)
    evaluate(parse("""|
    			      |RIGHT
    			      |PUSH DELTAX""".stripMargin).get, state)
    assertEquals(1, state.top)
    evaluate(parse("PUSH DELTAY").get, state)
    assertEquals(0, state.top)
    
  }
  
  private def executeAbsTest(stringInput:String, expectedAst:Abs, expectedIntResult:Int) {
	 val ast = parseAll(expression, stringInput).get
	 assertEquals(expectedAst, ast)
	 assertEquals(expectedIntResult, evaluate(ast, State(0,0,0)))
  }
  
  def testAbs() {
    executeAbsTest("ABS(-1)", Abs(Constant(-1)), 1) 
    executeAbsTest("ABS(1)", Abs(Constant(1)), 1) 
    executeAbsTest("ABS(3-5)", Abs(Minus(Constant(3), Constant(5))), 2) 
  }
  
}
