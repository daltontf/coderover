package tfd.coderover

sealed abstract class Instruction 
case class Forward(expression:Expression) extends Instruction
case class TurnRight() extends Instruction
case class TurnLeft() extends Instruction
case class If(expression:BooleanLogic, thenStatements:List[Instruction], elseStatements:List[Instruction]) extends Instruction
case class While(expression:BooleanLogic, blockStatements:List[Instruction]) extends Instruction
case class Push(expression:Expression) extends Instruction

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

sealed abstract class BooleanLogic() 
 sealed abstract case class Logical(left:BooleanLogic, right:BooleanLogic) extends BooleanLogic
  case class Or(override val left:BooleanLogic, override val right:BooleanLogic) extends Logical(left, right)
  case class And(override val left:BooleanLogic, override val right:BooleanLogic) extends Logical(left, right)
 sealed abstract case class Comparison(left:Expression, right:Expression) extends BooleanLogic
  case class LessThan(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class GreaterThan(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class Equal(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class GreaterThanOrEqual(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class LessThanOrEqual(override val left:Expression, override val right:Expression) extends Comparison(left, right)
  case class NotEqual(override val left:Expression, override val right:Expression) extends Comparison(left, right)
