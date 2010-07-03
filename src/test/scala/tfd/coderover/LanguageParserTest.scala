package tfd.coderover

import junit.framework.Assert._
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matcher

import junit.framework.TestCase

object HamcrestAdapter {
  import org.hamcrest.Matchers

  def is[T](value:T):Matcher[Any] = Matchers.is(value.asInstanceOf[Any])
}

import HamcrestAdapter._

class LanguageParserTest extends TestCase {
  private[this] val languageParser = new LanguageParser();  import languageParser._

  private def assertParsingWithParserProduces(parser:Parser[_])(expectedAst:Expression, code:String*) {
    code.foreach{ x =>
      val parseResult = parseAll(parser, x)
      if (!parseResult.successful) {
        fail("Parsing failed:\n" + parseResult.asInstanceOf[NoSuccess].msg)
      }
      assertThat(parseResult.get, is(expectedAst))
    }
  }

  private def assertProgramParsingProduces(expectedAst:List[Any], code:String*) {
    code.foreach{ x =>
      val parseResult = parse(x)
      if (!parseResult.successful) {
        fail("Parsing failed:\n" + parseResult.asInstanceOf[NoSuccess].msg)
      }
      assertThat(parseResult.get, is(expectedAst))
    }
  }

  def testEmptyProgram() {
    assertProgramParsingProduces(List(), "", " ","\n", "\n \n")
  }

  def testIntExpression() {
    val assertParsingIntExpression = assertParsingWithParserProduces(intExpression) _
    assertParsingIntExpression(Constant(1), "1", "01", "(1)", "((1))", "(((01)))", "((0000001))")
    assertParsingIntExpression(Constant(-1), "-1", "-01", "(-1)", "((-01))", "((-00001))")
    assertParsingIntExpression(Negate(Add(List(Constant(1), Constant(2)))), "-(1+2)", "-(((1)+(2)))")
  }

  def testStringConstant() {
    assertParsingWithParserProduces(stringConstant)(StringConstant("foo"), "\"foo\"")
  }

  def testMathematical() {
    val assertParsingMathematical = assertParsingWithParserProduces(mathematical) _
    assertParsingMathematical(Add(List(Constant(1), Constant(-1))), "1 + -1")
    assertParsingMathematical(Subtract(List(Constant(1), Constant(-1))), "1 - -1")
    assertParsingMathematical(Subtract(List(Add(List(Constant(-1), Constant(2))), Constant(3))), "(-1 + 2) - 3")
    assertParsingMathematical(Multiply(List(Constant(4), Constant(5))), "4 * 5")
    assertParsingMathematical(Divide(List(Constant(10), Constant(3))), "10 / 3")
    assertParsingMathematical(Modulus(List(Constant(10), Constant(3))), "10 % 3")
  }

  def testComparison() {
    val assertParsingComparison = assertParsingWithParserProduces(comparison) _
    assertParsingComparison(Equal(Constant(1), Constant(2)), "1 = 2")
    assertParsingComparison(LessThan(Constant(-1), Constant(1)), "-1 < 1")
    assertParsingComparison(GreaterThan(Constant(3), Constant(-2)), " 3  >  -2")
    assertParsingComparison(LessThanOrEqual(Constant(-1), Constant(1)), "-1 <= 1")
    assertParsingComparison(GreaterThanOrEqual(Constant(3), Constant(-2)), "3>=-2")
    assertParsingComparison(NotEqual(Constant(1), Constant(2)), "1<>2")
  }

  def testLogical() {
    val assertParsingLogical = assertParsingWithParserProduces(logical) _
    assertParsingLogical(And(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)))), "(2 = 2) AND (4 <> -3)")
    assertParsingLogical(Or(List(
      Equal(Constant(2), Constant(3)),
      NotEqual(Constant(4), Constant(-3)))), "(2 = 3) OR (4 <> -3)")
    assertParsingLogical(And(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)),
      LessThan(Constant(4), Constant(3)))), "(2 = 2) AND (4 <> -3) AND (4 < 3)")
    assertParsingLogical(Or(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)),
      LessThan(Constant(4), Constant(3)))), "(2 = 2) OR (4 <> -3) OR (4 < 3)")
  }

  def testForward() {
    assertProgramParsingProduces(List(Forward()), "FORWARD")
  }

  def testRightLeft() {
    assertProgramParsingProduces(List(TurnRight(), TurnLeft()), 
      """|RIGHT
    	   |LEFT""".stripMargin)
  }

  def testSimpleIfComparisons() {
    assertProgramParsingProduces(List(If(Equal(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 = 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(LessThan(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)), 
      """|
         |IF (1 < 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(GreaterThan(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 > 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(GreaterThanOrEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 >= 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(LessThanOrEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 <= 2) {                                                                                                               
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin,
    // unnecessary params
      """|
         |IF ((1 <> 2)) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
  }

  def testIfElse() {
    assertProgramParsingProduces(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), List(TurnRight()))), 
      """|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         |} ELSE {
         | RIGHT
         |}""".stripMargin)
  }

  def testIfElseIf() {
    assertProgramParsingProduces(List(
      If(NotEqual(Constant(1), Constant(2)),
        List(TurnLeft(), Forward()),
        List(If(Equal(Constant(2), Constant(3)), List(TurnRight()), Nil))
        )),
        """|
           |IF (1 <> 2) {
           | LEFT
           | FORWARD
           |} ELSE IF (2 = 3) {
           | RIGHT
           |}""".stripMargin)
  }

  def testLogicalComparisonAndMathematical() {
    assertProgramParsingProduces(List(
      If(
        And(List(
          GreaterThan(Constant(1), Constant(-1)),
          NotEqual(Constant(-3), Add(List(Constant(2), Constant(-2)))))),
        List(TurnLeft(), Forward(), TurnRight()), Nil)),
      """|
         |IF ((1 > -1) AND (-3 <> (2 + -2))) {
         | LEFT
         | FORWARD
         | RIGHT
         | }""".stripMargin)
  }

  def testWhile() {
    assertProgramParsingProduces(List(While(Equal(Constant(1), Constant(2)), List())),
      """|
         |WHILE (1 = 2) {
         |}""".stripMargin,
      """|
         |WHILE ((1 = 2)) {
         |}""".stripMargin)
    assertProgramParsingProduces(List(While(Not(Equal(Constant(1), Constant(2))), List())), 
      """|
         |WHILE NOT(1 = 2) {
         |}""".stripMargin,
      """|
         |WHILE (NOT(1 = 2)) {
         |}""".stripMargin, 
      """|
         |WHILE NOT(1 = 2) {
         |}""".stripMargin)
  }

  def testPush() {
    assertProgramParsingProduces(List(Push(Constant(1))), "PUSH 1")
    assertProgramParsingProduces(List(Push(Add(List(Constant(1), Constant(2))))), "PUSH (1+2)")
    assertProgramParsingProduces(List(Push(Subtract(List(Constant(8), GridX())))), "PUSH (8 - X)")
    assertProgramParsingProduces(List(Push(Negate(Add(List(Constant(1), Constant(2)))))), "PUSH -(1+2)")
    assertProgramParsingProduces(List(Push(Abs(Constant(-1)))), "PUSH ABS(-1)")
  }

  def testPop() {
    assertProgramParsingProduces(List(Push(Constant(1)), Pop()), "PUSH 1 POP")
  }

  def testReplace() {
    assertProgramParsingProduces(List(Push(Constant(1)), Replace(Multiply(List(Top(), Constant(2))))),
      """|PUSH 1
    	   |REPLACE (TOP * 2)""".stripMargin)
  }

  def testTop() {
    assertProgramParsingProduces(List(If(LessThanOrEqual(Top(), Constant(-1)), List(), Nil)),
      """|IF (TOP <= -1) {
		  	 |}""".stripMargin)
  }

  def testDepth() {
    assertProgramParsingProduces(List(Push(Add(List(Depth(), Constant(1))))), "PUSH (DEPTH + 1)")
  }

  def testGridX() {
    assertProgramParsingProduces(List(While(LessThan(GridX(), Constant(5)), List(Forward()))),
      """|WHILE (X < 5) {
         | FORWARD
		     |}""".stripMargin)
  }

  def testGridY() {
    assertProgramParsingProduces(List(While(GreaterThanOrEqual(GridY(), Constant(1)), List(Forward()))),
      """|WHILE (Y >= 1) {
         | FORWARD
		     |}""".stripMargin)
  }

  def testDeltaX() {
    assertProgramParsingProduces(List(While(Equal(DeltaX(), Constant(1)), List(TurnRight()))),
      """|WHILE (DX = 1) {
         | RIGHT
		  	 |}""".stripMargin)
  }

  def testDeltaY() {
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE (DY = 0) {
         | LEFT
		  	 |}""".stripMargin)
  }

  def testAbs() {
    assertProgramParsingProduces(List(Push(Abs(Constant(-1)))), "PUSH ABS(-1)")
    assertProgramParsingProduces(List(Push(Negate(Abs(Constant(-1))))), "PUSH -ABS(-1)")
    assertProgramParsingProduces(List(Push(Abs(Negate(Add(List(Constant(1), Constant(2), Constant(3))))))), "PUSH ABS(-(1 + 2 + 3))")
  }

  def testMax() {
    assertProgramParsingProduces(List(Push(Max(Constant(-1), Constant(2)))), "PUSH MAX(-1, 2)")
  }

  def testMin() {
    assertProgramParsingProduces(List(Push(Min(Constant(-1), Constant(2)))), "PUSH MIN(-1, 2)")
  }

  def testPaint() {
    assertProgramParsingProduces(List(Paint()), "PAINT")
  }

  def testPainted() {
    assertProgramParsingProduces(List((While(Painted(Constant(1), Constant(2)), List(Forward())))),
      "WHILE (PAINTED(1,2)) { FORWARD }")
    assertProgramParsingProduces(List((While(Painted(Constant(1), Constant(2)), List(Forward())))),
      "WHILE PAINTED(1,2) { FORWARD }")
    assertProgramParsingProduces(List((While(Painted(Constant(1), Constant(2)), List(Forward())))),
      "WHILE ((PAINTED(1,2))) { FORWARD }")

  }

  def testNot() {
    assertProgramParsingProduces(List((While(Not(Painted(Constant(1), Constant(2))), List(Forward())))),
      "WHILE (NOT(PAINTED(1,2))) { FORWARD }")
  }

  def testAdjacent() {
    assertProgramParsingProduces(List(If(Adjacent("MINE"), List(TurnRight()), Nil)), "IF (ADJACENT(MINE)) { RIGHT }")
  }

  def testDistances() {
    assertProgramParsingProduces(List(Push(DistanceX("FLAG"))), "PUSH DISTANCEX(FLAG)")
    assertProgramParsingProduces(List(Push(DistanceY("ROCK"))), "PUSH DISTANCEY(ROCK)")
    assertProgramParsingProduces(List(Push(Min(DistanceX("FLAG"), DistanceY("FLAG")))), "PUSH MIN(DISTANCEX(FLAG), DISTANCEY(FLAG))")
  }

  def testProc() {
    assertProgramParsingProduces(List(Proc("FUNC", List(TurnLeft(), TurnLeft()))), "PROC FUNC { LEFT LEFT }")
    assertProgramParsingProduces(List(Proc("FUNC_underscore", List(TurnLeft(), TurnLeft()))), "PROC FUNC_underscore { LEFT LEFT }")
  }

  def testCall() {
    assertProgramParsingProduces(List(InvokeProc("PRC", Nil)), "PRC", "PRC()")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42)))), "PRC(42)")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Add(List(Top(), Constant(28)))))), "PRC(42,TOP + 28)")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Add(List(Top(), Constant(28)))))), "PRC(42,(TOP + 28))")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Negate(Add(List(Top(), Constant(28))))))), "PRC(42,-(TOP + 28))")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Add(List(Negate(Top()), Constant(28)))))), "PRC(42,-TOP + 28)")
  }

  def testPrint() {
    assertProgramParsingProduces(List(Print(List(StringConstant("X = "), GridX()))), """PRINT "X = " + X""")
  }

  def testStore() {
    assertProgramParsingProduces(List(Store(Constant(1), GridX())), "STORE (1, X)", "STORE((1),(X))")
  }

  def testMem() {
    assertProgramParsingProduces(List(Push(Mem(Constant(1)))), "PUSH(MEM(1))", "PUSH((MEM((1))))")
  }

  def testComments() {
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE (DY = 0) { // comment
         | LEFT
         |}//comment""".stripMargin)
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE /* multi-line
         | comment */ (DY = 0) {
         | LEFT  /* Comment */
		     |}""".stripMargin)
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE (DY = 0) { // Comment
         | /* Comment */ LEFT
		     |}""".stripMargin)
  }

  def testObstructed() {
    assertProgramParsingProduces(List(If(Obstructed(Add(List(GridX(), DeltaX())), Add(List(GridY(), DeltaY()))), List(TurnLeft()), Nil)), "IF (OBSTRUCTED(X+DX,Y+DY)) { LEFT }")
  }

  def testNegativeExpression() {
    assertProgramParsingProduces(List(Print(List(StringConstant("-DX = "), Negate(DeltaX())))), """PRINT "-DX = " + -DX""")
  }

  def testEvalParameters() {
    assertProgramParsingProduces(List(Push(EvalParam(1))), "PUSH $1")
  }

  def testFunc() {
    assertProgramParsingProduces(List(Func("TEST", Add(List(DeltaX(), GridX())))), "FUNC TEST ( DX + X )")
  }

  def testPred() {
    assertProgramParsingProduces(List(Pred("DY_EQUALS", Equal(DeltaY(), EvalParam(1)))), "PRED DY_EQUALS ( DY = $1 )")
  }

  def testInvokeFunc() {
    assertProgramParsingProduces(List(Push(InvokeFunc("TEST", List(Constant(42))))), "PUSH TEST(42)")
  }

  def testInvokePred() {
    assertProgramParsingProduces(List(If(InvokePred("DY_EQUALS", List(Constant(2))), Nil, Nil)), "IF DY_EQUALS(2) { }")
  }

  def testTernary() {
    assertProgramParsingProduces(List(Push(Ternary(Equal(DeltaY(), Constant(0)), DeltaX(), Constant(-1)))), "PUSH ((DY = 0) ? DX : -1)")
  }

  def testRepeat() {
    assertProgramParsingProduces(List(Repeat(Constant(5), List(Forward(), TurnRight()))), "REPEAT 5 { FORWARD RIGHT }")
  }
}
