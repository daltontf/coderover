package tfd.coderover

object Evaluator {
  
  
  final def evaluate(instructions:List[Instruction], state:State) {
	  instructions.foreach(evaluate(_, state))
  }
  
  final def evaluate(expression:Expression, state:State):Int = {
    expression match {
      case Constant(x) => x
      case Add(left, right) => evaluate(left, state) + evaluate(right, state)
      case Subtract(left, right) => evaluate(left, state) - evaluate(right, state)
      case Multiply(left, right) => evaluate(left, state) * evaluate(right, state)
      case Divide(left, right) => evaluate(left, state) / evaluate(right, state)
      case Modulus(left, right) => evaluate(left, state) % evaluate(right, state)
      case Pop() => state.pop()
      case Top() => state.top
      case GridX() => state.droidX
      case GridY() => state.droidY
      case DeltaX() => state.deltaX
      case DeltaY() => state.deltaY
      case Abs(expr) => Math.abs(evaluate(expr, state))
    }
  }
  
  final def evaluate(booleanExpression:BooleanLogic, state:State):Boolean = {
	  booleanExpression match {
	    case And(left, right) => (evaluate(left, state) && evaluate(right, state))
        case Or(left, right) => (evaluate(left, state) || evaluate(right, state)) 
        case Equal(left, right) =>  (evaluate(left, state) == evaluate(right, state))
        case LessThan(left, right) =>  (evaluate(left, state) < evaluate(right, state))
        case GreaterThan(left, right) =>  (evaluate(left, state) > evaluate(right, state))
        case LessThanOrEqual(left, right) =>  (evaluate(left, state) <= evaluate(right, state))
        case GreaterThanOrEqual(left, right) =>  (evaluate(left, state) >= evaluate(right, state))
        case NotEqual(left, right) =>  (evaluate(left, state) != evaluate(right, state))
	  }
  }
  
  final def evaluate(instruction:Instruction, state:State) {
      instruction match {
        case Forward(expression) => state.droidMoveForward(evaluate(expression, state))
        case TurnRight() => state.droidTurnRight()
        case TurnLeft() => state.droidTurnLeft()
        case Push(expression) => state.push(evaluate(expression, state))
        case If(booleanExpression, thenStatements, elseStatements) => if (evaluate(booleanExpression, state)) {
        	evaluate(thenStatements, state)
        } else if (!elseStatements.isEmpty) {
        	evaluate(elseStatements, state)            
        }
        case While(booleanExpression, blockStatements) => while (evaluate(booleanExpression, state)) {
            evaluate(blockStatements, state)
        }
      } 
  }
}
