package tfd.coderover

import junit.framework._
import org.junit.Assert._

class LanguageParserTest extends TestCase {
  private[this] val languageParser = new LanguageParser()
  import languageParser._

  def testEmptyProgram() {
    assertEquals(List(), parse("").get)
    assertEquals(List(), parse(" ").get)
    assertEquals(List(), parse("\n").get)
  }

  def testIntExpression() {
    assertEquals(Constant(1), parseAll(intExpression, "1").get)
    assertEquals(Constant(-1), parseAll(intExpression, "-1").get)
    assertEquals(Negate(Add(List(Constant(1), Constant(2)))), parseAll(intExpression, "-(1+2)").get)
  }

  def testStringConstant() {
    assertEquals(StringConstant("foo"), parseAll(stringConstant, "\"foo\"").get)
  }

  def testMathematical() {
    assertEquals(Add(List(Constant(1), Constant(-1))), parseAll(mathematical, "1 + -1").get)
    assertEquals(Subtract(List(Constant(1), Constant(-1))), parseAll(mathematical, "1 - -1").get)
    assertEquals(Subtract(List(Add(List(Constant(-1), Constant(2))), Constant(3))), parseAll(mathematical, "(-1 + 2) - 3").get)
    assertEquals(Multiply(List(Constant(4), Constant(5))), parseAll(mathematical, "4 * 5").get)
    assertEquals(Divide(List(Constant(10), Constant(3))), parseAll(mathematical, "10 / 3").get)
    assertEquals(Modulus(List(Constant(10), Constant(3))), parseAll(mathematical, "10 % 3").get)
  }

  def testComparison() {
    assertEquals(Equal(Constant(1), Constant(2)), parseAll(comparison, "1 = 2").get)
    assertEquals(LessThan(Constant(-1), Constant(1)), parseAll(comparison, "-1 < 1").get)
    assertEquals(GreaterThan(Constant(3), Constant(-2)), parseAll(comparison, " 3  >  -2").get)
    assertEquals(LessThanOrEqual(Constant(-1), Constant(1)), parseAll(comparison, "-1 <= 1").get)
    assertEquals(GreaterThanOrEqual(Constant(3), Constant(-2)), parseAll(comparison, "3>=-2").get)
    assertEquals(NotEqual(Constant(1), Constant(2)), parseAll(comparison, "1<>2").get)
  }

  def testLogical() {
    assertEquals(And(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)))), parseAll(logical, "(2 = 2) AND (4 <> -3)").get)
    assertEquals(Or(List(
      Equal(Constant(2), Constant(3)),
      NotEqual(Constant(4), Constant(-3)))), parseAll(logical, "(2 = 3) OR (4 <> -3)").get)
    assertEquals(And(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)),
      LessThan(Constant(4), Constant(3)))), parseAll(logical, "(2 = 2) AND (4 <> -3) AND (4 < 3)").get)
    assertEquals(Or(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)),
      LessThan(Constant(4), Constant(3)))), parseAll(logical, "(2 = 2) OR (4 <> -3) OR (4 < 3)").get)
  }

  def testSingleForward() {
    assertEquals(List(Forward(Constant(1))), parse("FORWARD").get)
    assertEquals(List(Forward(Constant(2))), parse("FORWARD 2").get)
  }

  def testRightLeft() {
    assertEquals(List(TurnRight(), TurnLeft()), parse("""|RIGHT
    										             |LEFT""".stripMargin).get)
  }

  def testSimpleIfComparisons() {
    assertEquals(List(If(Equal(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF (1 = 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
    assertEquals(List(If(LessThan(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF (1 < 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
    assertEquals(List(If(GreaterThan(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF (1 > 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
    assertEquals(List(If(GreaterThanOrEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF (1 >= 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
    assertEquals(List(If(LessThanOrEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF (1 <= 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
    assertEquals(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
    // unnecessary params
    assertEquals(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), Nil)), parse(
      """|
         |IF ((1 <> 2)) {
         | LEFT
         | FORWARD
         | }""".stripMargin).get)
  }

  def testIfElse() {
    assertEquals(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward(Constant(1))), List(TurnRight()))), parse(
      """|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         |} ELSE {
         | RIGHT
         |}""".stripMargin).get)
  }

  def testIfElseIf() {
    assertEquals(List(
      If(NotEqual(Constant(1), Constant(2)),
        List(TurnLeft(), Forward(Constant(1))),
        List(If(Equal(Constant(2), Constant(3)), List(TurnRight()), Nil))
        )
      ), parse(
      """|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         |} ELSE IF (2 = 3 ) {
         | RIGHT
         |}""".stripMargin).get)
  }

  def testLogicalComparisonAndMathematical() {
    assertEquals(List(
      If(
        And(List(
          GreaterThan(Constant(1), Constant(-1)),
          NotEqual(Constant(-3), Add(List(Constant(2), Constant(-2)))))),
        List(TurnLeft(), Forward(Add(List(Constant(1), Constant(2)))), TurnRight()), Nil)), parse(
      """|
         |IF ((1 > -1) AND (-3 <> (2 + -2))) {
         | LEFT
         | FORWARD (1 + 2)
         | RIGHT
         | }""".stripMargin).get)
  }

  def testWhile() {
    assertEquals(List(While(Equal(Constant(1), Constant(2)), List())), parse(
      """|
         |WHILE (1 = 2) {
         |}""".stripMargin).get)
    assertEquals(List(While(Equal(Constant(1), Constant(2)), List())), parse(
      """|
         |WHILE ((1 = 2)) {
         |}""".stripMargin).get)
  }

  def testPush() {
    assertEquals(List(Push(Constant(1))), parse("""PUSH 1""").get)
    assertEquals(List(Push(Add(List(Constant(1), Constant(2))))), parse("""PUSH (1+2)""").get)
    assertEquals(List(Push(Subtract(List(Constant(8), GridX())))), parse("""PUSH (8 - X)""").get)
    assertEquals(List(Push(Negate(Add(List(Constant(1), Constant(2)))))), parse("""PUSH -(1+2)""").get)
  }

  def testPop() {
    assertEquals(List(Push(Constant(1)), Pop()), parse("""PUSH 1 POP""".stripMargin).get)
  }

  def testReplace() {
    assertEquals(List(Push(Constant(1)), Replace(Multiply(List(Top(), Constant(2))))),
      parse("""|PUSH 1
    			     |REPLACE (TOP * 2)""".stripMargin).get)
  }

  def testTop() {
    assertEquals(List(If(LessThanOrEqual(Top(), Constant(-1)), List(), Nil)),
      parse("""|IF (TOP <= -1) {
		  			   |}""".stripMargin).get)
  }

  def testDepth() {
    assertEquals(List(Push(Add(List(Depth(), Constant(1))))), parse("""PUSH (DEPTH + 1)""").get)
  }

  def testGridX() {
    assertEquals(List(While(LessThan(GridX(), Constant(5)), List(Forward(Constant(1))))),
      parse("""|WHILE (X < 5) {
            	 | FORWARD 1
		  			   |}""".stripMargin).get)
  }

  def testGridY() {
    assertEquals(List(While(GreaterThanOrEqual(GridY(), Constant(1)), List(Forward(Constant(1))))),
      parse("""|WHILE (Y >= 1) {
               | FORWARD 1
		  			   |}""".stripMargin).get)
  }

  def testDeltaX() {
    assertEquals(List(While(Equal(DeltaX(), Constant(1)), List(TurnRight()))),
      parse("""|WHILE (DX = 1) {
               | RIGHT
		  			   |}""".stripMargin).get)
  }

  def testDeltaY() {
    assertEquals(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      parse("""|WHILE (DY = 0) {
               | LEFT
		  			   |}""".stripMargin).get)
  }

  def testAbs() {
    assertEquals(List(Push(Abs(Constant(-1)))), parse("PUSH ABS(-1)").get)
    assertEquals(List(Push(Negate(Abs(Constant(-1))))), parse("PUSH -ABS(-1)").get)
    assertEquals(List(Push(Abs(Negate(Add(List(Constant(1), Constant(2), Constant(3))))))), parse("PUSH ABS(-(1 + 2 + 3))").get)
  }

  def testMax() {
    assertEquals(List(Push(Max(Constant(-1), Constant(2)))), parse("PUSH MAX(-1, 2)").get)
  }

  def testMin() {
    assertEquals(List(Push(Min(Constant(-1), Constant(2)))), parse("PUSH MIN(-1, 2)").get)
  }

  def testPaint() {
    assertEquals(List(Paint()), parse("PAINT").get)
  }

  def testPainted() {
    assertEquals(List((While(Painted(Constant(1), Constant(2)), List(Forward(Constant(1)))))),
      parse("""WHILE (PAINTED(1,2)) { FORWARD }""").get)
    assertEquals(List((While(Painted(Constant(1), Constant(2)), List(Forward(Constant(1)))))),
      parse("""WHILE PAINTED(1,2) { FORWARD }""").get)
    assertEquals(List((While(Painted(Constant(1), Constant(2)), List(Forward(Constant(1)))))),
      parse("""WHILE ((PAINTED(1,2))) { FORWARD }""").get) 

  }

  def testNot() {
    assertEquals(List((While(Not(Painted(Constant(1), Constant(2))), List(Forward(Constant(1)))))),
      parse("""WHILE (NOT(PAINTED(1,2))) { FORWARD }""").get)
  }

  def testAdjacent() {
    assertEquals(List(If(Adjacent("MINE"), List(TurnRight()), Nil)), parse("IF (ADJACENT(MINE)) { RIGHT }").get)
  }

  def testDistances() {
    assertEquals(List(Push(DistanceX("FLAG"))), parse("PUSH DISTANCEX(FLAG)").get)
    assertEquals(List(Push(DistanceY("ROCK"))), parse("PUSH DISTANCEY(ROCK)").get)
    assertEquals(List(Push(Min(DistanceX("FLAG"), DistanceY("FLAG")))), parse("PUSH MIN(DISTANCEX(FLAG), DISTANCEY(FLAG))").get)
  }

  def testDef() {
    assertEquals(List(Def("FUNC", List(TurnLeft(), TurnLeft()))), parse("DEF FUNC { LEFT LEFT }").get)
  }

  def testCall() {
    assertEquals(List(Call("FUNC")), parse("CALL FUNC").get)
  }

  def testPrint() {
    assertEquals(List(Print(List(StringConstant("X = "), GridX()))), parse("""PRINT "X = " + X""").get)
  }

  def testStore() {
    assertEquals(List(Store(Constant(1), GridX())), parse("""STORE (1, X)""").get)
  }

  def testMem() {
    assertEquals(List(Push(Mem(Constant(1)))), parse("""PUSH MEM(1)""").get)
  }

  def testComments() {
    assertEquals(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      parse("""|WHILE (DY = 0) { // comment
               | LEFT
               |}//comment""".stripMargin).get)
    assertEquals(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      parse("""|WHILE /* /* comment */ (DY = 0) {
               | LEFT  /* Comment */
		  			   |}""".stripMargin).get)
    assertEquals(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      parse("""|WHILE (DY = 0) { // Comment
               | /* Comment */ LEFT
		  			   |}""".stripMargin).get)
  }
}
