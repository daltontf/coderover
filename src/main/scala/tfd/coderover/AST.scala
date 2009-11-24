package tfd.coderover

sealed abstract class Instruction 
case class Forward(expression:IntExpression) extends Instruction
case class TurnRight() extends Instruction
case class TurnLeft() extends Instruction
case class If(booleanExpression:BooleanExpression, thenStatements:List[Instruction], elseStatements:List[Instruction]) extends Instruction
case class While(booleanExpression:BooleanExpression, blockStatements:List[Instruction]) extends Instruction
case class Push(expression:IntExpression) extends Instruction
case class Paint(expression:IntExpression) extends Instruction
case class Pop() extends Instruction
case class Replace(expression:IntExpression) extends Instruction
case class Def(name:String, statements:List[Instruction]) extends Instruction
case class Call(name:String) extends Instruction
case class Print(expression:List[Expression]) extends Instruction

sealed abstract class Expression()
  case class StringConstant(value:String) extends Expression

  sealed abstract class IntExpression() extends Expression
    case class Constant(value:Int) extends IntExpression
    case class Top() extends IntExpression
    case class GridX() extends IntExpression
    case class GridY() extends IntExpression
    case class DeltaX() extends IntExpression
    case class DeltaY() extends IntExpression
    case class Depth() extends IntExpression
    case class DistanceX(entity:String) extends IntExpression
    case class DistanceY(entity:String) extends IntExpression
    case class Abs(expression:IntExpression) extends IntExpression
    case class Max(expression1:IntExpression, expression2:IntExpression) extends IntExpression
    case class Min(expression1:IntExpression, expression2:IntExpression) extends IntExpression
    case class Negate(expression:IntExpression) extends IntExpression
    case class PaintColor(x:IntExpression, y:IntExpression) extends IntExpression
 
 sealed abstract case class Mathematical(expressions:List[IntExpression]) extends IntExpression
  case class Add(override val expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Subtract(override val expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Multiply(override val expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Divide(override val expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Modulus(override val expressions:List[IntExpression]) extends Mathematical(expressions)

sealed abstract class BooleanExpression() extends Expression
 case class Painted(x:IntExpression, y:IntExpression) extends BooleanExpression
 case class Adjacent(entity:String) extends BooleanExpression
 case class Not(booleanExpression:BooleanExpression) extends BooleanExpression
 sealed abstract case class Logical() extends BooleanExpression
  case class Or(val expressions:List[BooleanExpression])extends Logical
  case class And(val expressions:List[BooleanExpression]) extends Logical
 sealed abstract case class Comparison(left:IntExpression, right:IntExpression) extends BooleanExpression
  case class LessThan(override val left:IntExpression, override val right:IntExpression) extends Comparison(left, right)
  case class GreaterThan(override val left:IntExpression, override val right:IntExpression) extends Comparison(left, right)
  case class Equal(override val left:IntExpression, override val right:IntExpression) extends Comparison(left, right)
  case class GreaterThanOrEqual(override val left:IntExpression, override val right:IntExpression) extends Comparison(left, right)
  case class LessThanOrEqual(override val left:IntExpression, override val right:IntExpression) extends Comparison(left, right)
  case class NotEqual(override val left:IntExpression, override val right:IntExpression) extends Comparison(left, right)
