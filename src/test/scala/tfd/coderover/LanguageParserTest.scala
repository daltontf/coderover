package tfd.coderover

import junit.framework._
import org.junit.Assert._

class LanguageParserTest extends TestCase {
  private[this] val languageParser = new LanguageParser();  import languageParser._

  private def assertParsingWithParserProduces(parser:Parser[_])(expectedAst:Expression, code:String*) {
    code.foreach{ x => assertEquals(expectedAst, parseAll(parser, x).get) }
  }

  private def assertProgramParsingProduces(expectedAst:List[Expression], code:String*) {
    code.foreach{ x => assertEquals(expectedAst, parse(x).get) }
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
    List[(String, List[Instruction])](
      "CALL FUNC" -> List(Call("FUNC", Nil)),
      "CALL FUNC()" -> List(Call("FUNC", Nil)),
      "CALL FUNC(42)" -> List(Call("FUNC", List(Constant(42))))
    ).map {
       case (code:String, ast:List[Instruction]) =>
      assertEquals(ast, parse(code).get)                          
    }
    assertEquals(List(Call("FUNC", List(Constant(42), Add(List(Top(), Constant(28)))))), parse("CALL FUNC(42,TOP + 28)").get)
    assertEquals(List(Call("FUNC", List(Constant(42), Add(List(Top(), Constant(28)))))), parse("CALL FUNC(42,(TOP + 28))").get)
    assertEquals(List(Call("FUNC", List(Constant(42), Negate(Add(List(Top(), Constant(28))))))), parse("CALL FUNC(42,-(TOP + 28))").get)
    assertEquals(List(Call("FUNC", List(Constant(42), Add(List(Negate(Top()), Constant(28)))))), parse("CALL FUNC(42,-TOP + 28)").get)
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
      parse("""|WHILE /* multi-line
               | comment */ (DY = 0) {
               | LEFT  /* Comment */
		  			   |}""".stripMargin).get)
    assertEquals(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      parse("""|WHILE (DY = 0) { // Comment
               | /* Comment */ LEFT
		  			   |}""".stripMargin).get)
  }

  def testObstructed() {
    assertEquals(List(If(Obstructed(Add(List(GridX(), DeltaX())), Add(List(GridY(), DeltaY()))), List(TurnLeft()), Nil)), parse("IF (OBSTRUCTED(X+DX,Y+DY)) { LEFT }").get)
  }

  def testNegativeExpression() {
    assertEquals(List(Print(List(StringConstant("-DX = "), Negate(DeltaX())))), parse("""PRINT "-DX = " + -DX""").get)
  }

  def testParameters() {
    assertEquals(List(Forward(Param(1))), parse("""FORWARD :1""").get)
  }
}
