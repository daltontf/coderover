package tfd.coderover

class Evaluator(environment:Environment) {
  
  final def evaluate(instructions:List[Instruction], state:State) {
	  if (!state.stopped) {
		  instructions.foreach(evaluate(_, state))
      }
  }
  
  final def evaluate(expression:Expression, state:State):Int = {
    expression match {
      case Constant(x)           => x
      case Add(left, right) 	 => evaluate(left, state) + evaluate(right, state)
      case Subtract(left, right) => evaluate(left, state) - evaluate(right, state)
      case Multiply(left, right) => evaluate(left, state) * evaluate(right, state)
      case Divide(left, right)   => evaluate(left, state) / evaluate(right, state)
      case Modulus(left, right)  => evaluate(left, state) % evaluate(right, state)
      case Pop() 				 => state.pop()
      case Top() 				 => state.top
      case GridX() 				 => state.gridX
      case GridY() 				 => state.gridY
      case DeltaX() 			 => state.deltaX
      case DeltaY() 			 => state.deltaY
      case Abs(expr) 			 => Math.abs(evaluate(expr,  state))
      case Max(expr1, expr2) 	 => Math.max(evaluate(expr1, state), evaluate(expr2, state))
      case Min(expr1, expr2) 	 => Math.min(evaluate(expr1, state), evaluate(expr2, state))
    }
  }
  
  final def evaluate(booleanExpression:BooleanLogic, state:State):Boolean = {
	  booleanExpression match {
	    case And(left, right) 				 => (evaluate(left, state) && evaluate(right, state))
        case Or(left, right) 				 => (evaluate(left, state) || evaluate(right, state)) 
        case Equal(left, right) 			 =>  (evaluate(left, state) == evaluate(right, state))
        case LessThan(left, right) 			 =>  (evaluate(left, state) < evaluate(right, state))
        case GreaterThan(left, right) 	     =>  (evaluate(left, state) > evaluate(right, state))
        case LessThanOrEqual(left, right) 	 =>  (evaluate(left, state) <= evaluate(right, state))
        case GreaterThanOrEqual(left, right) =>  (evaluate(left, state) >= evaluate(right, state))
        case NotEqual(left, right) 			 =>  (evaluate(left, state) != evaluate(right, state))
	  }
  }
  
  final def evaluate(instruction:Instruction, state:State) {
      if (!state.stopped) {
        instruction match {
        	case Forward(expression) => 
        	    var distance = Math.abs(evaluate(expression, state))
        		while (!state.stopped && distance > 0 && environment.canMoveForward(state)) {
        				moveForward(state)
        				environment.postMoveForward(state)
        				distance = distance - 1
        		}
        	case TurnRight() 	  => turnRight(state)
        	case TurnLeft() 	  => turnLeft(state)
        	case Push(expression) => state.push(evaluate(expression, state))
        	case If(booleanExpression, thenStatements, elseStatements) => 
        		if (evaluate(booleanExpression, state)) {
        			evaluate(thenStatements, state)
        		} else if (!elseStatements.isEmpty) {
        			evaluate(elseStatements, state)            
        		}
        	case While(booleanExpression, blockStatements) => 
        		while (!state.stopped && evaluate(booleanExpression, state)) {
        			evaluate(blockStatements, state)
        		}
            case Paint(expression) => paint(evaluate(expression, state), state)
        }
     }
  }
  
  def moveForward(state:State) = state.moveForward()
  
  def turnRight(state:State) = state.turnRight()
  
  def turnLeft(state:State) = state.turnLeft()
  
  def paint(color:Int, state:State) = environment.paint(color, state)
  

}
