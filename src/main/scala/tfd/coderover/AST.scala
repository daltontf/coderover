package tfd.coderover

sealed abstract class Instruction 
case class Forward(expression:Expression) extends Instruction
case class TurnRight() extends Instruction
case class TurnLeft() extends Instruction
case class If(booleanExpression:BooleanExpression, thenStatements:List[Instruction], elseStatements:List[Instruction]) extends Instruction
case class While(booleanExpression:BooleanExpression, blockStatements:List[Instruction]) extends Instruction
case class Push(expression:Expression) extends Instruction
case class Paint(expression:Expression) extends Instruction

sealed abstract class Expression() 
 case class Constant(value:Int) extends Expression
 case class Pop() extends Expression
 case class Top() extends Expression
 case class GridX() extends Expression
 case class GridY() extends Expression
 case class DeltaX() extends Expression
 case class DeltaY() extends Expression
 case class Abs(expression:Expression) extends Expression
 case class Max(expression1:Expression, expression2:Expression) extends Expression
 case class Min(expression1:Expression, expression2:Expression) extends Expression
 
 sealed abstract case class Mathematical(left:Expression, right:Expression) extends Expression
  case class Add(override val left:Expression, override val right:Expression) extends Mathematical(left, right)
  case class Subtract(override val left:Expression, override val right:Expression) extends Mathematical(left, right)
  case class Multiply(override val left:Expression, override val right:Expression) extends Mathematical(left, right)
  case class Divide(override val left:Expression, override val right:Expression) extends Mathematical(left, right)
  case class Modulus(override val left:Expression, override val right:Expression) extends Mathematical(left, right)

sealed abstract class BooleanExpression()
 case class IsPainted(x:Expression, y:Expression) extends BooleanExpression
 case class Not(booleanExpression:BooleanExpression) extends BooleanExpression
 sealed abstract case class Logical() extends BooleanExpression
  case class Or(val expressions:List[BooleanExpression])extends Logical
  case class And(val expressions:List[BooleanExpression]) extends Logical
 sealed abstract case class Comparison(left:Expression, right:Expression) extends BooleanExpression
  case class LessThan(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class GreaterThan(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class Equal(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class GreaterThanOrEqual(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class LessThanOrEqual(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class NotEqual(override val left:Expression, override val right:Expression) extends Comparison(left, right)
