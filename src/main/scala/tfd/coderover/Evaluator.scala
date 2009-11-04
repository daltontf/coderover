package tfd.coderover

class Evaluator(environment:Environment) {
  
  final def evaluate(instructions:List[Instruction], state:State) {
	  if (!state.stopped) {
		  instructions.foreach(evaluate(_, state))
      }
  }
  
  private[coderover] final def evaluate(expression:Expression, state:State):Int = {
    expression match {
      case Constant(x)           => x
      case Add(expressionList) 	 	 => expressionList.tail.foldLeft(evaluate(expressionList.head, state)){ _ + evaluate(_,state) }
      case Subtract(expressionList)  => expressionList.tail.foldLeft(evaluate(expressionList.head, state)){ _ - evaluate(_,state) }
      case Multiply(expressionList)  => expressionList.tail.foldLeft(evaluate(expressionList.head, state)){ _ * evaluate(_,state) }
      case Divide(expressionList)    => expressionList.tail.foldLeft(evaluate(expressionList.head, state)){ _ / evaluate(_,state) }
      case Modulus(expressionList)	 => expressionList.tail.foldLeft(evaluate(expressionList.head, state)){ _ % evaluate(_,state) }
      case Top() 				 => state.top
      case GridX() 				 => state.gridX
      case GridY() 				 => state.gridY
      case DeltaX() 			 => state.deltaX
      case DeltaY() 			 => state.deltaY
      case Depth()				 => state.depth
      case Abs(expr) 			 => Math.abs(evaluate(expr,  state))
      case Max(expr1, expr2) 	 => Math.max(evaluate(expr1, state), evaluate(expr2, state))
      case Min(expr1, expr2) 	 => Math.min(evaluate(expr1, state), evaluate(expr2, state))
      case Negate(expr)			 => -evaluate(expr, state)
    }
  }
  
   private[coderover] final def evaluate(booleanExpression:BooleanExpression, state:State):Boolean = {
	  booleanExpression match {
        case Painted(x, y)                   =>  environment.isPainted(evaluate(x, state), evaluate(y, state))
        case Not(booleanExpression)		     =>  !(evaluate(booleanExpression, state))
	    case And(booleanExpressionList)		 =>  booleanExpressionList.forall{ evaluate(_, state) }
        case Or(booleanExpressionList) 		 =>  booleanExpressionList.exists{ evaluate(_, state) }
        case Equal(left, right) 			 =>  (evaluate(left, state) == evaluate(right, state))
        case LessThan(left, right) 			 =>  (evaluate(left, state) < evaluate(right, state))
        case GreaterThan(left, right) 	     =>  (evaluate(left, state) > evaluate(right, state))
        case LessThanOrEqual(left, right) 	 =>  (evaluate(left, state) <= evaluate(right, state))
        case GreaterThanOrEqual(left, right) =>  (evaluate(left, state) >= evaluate(right, state))
        case NotEqual(left, right) 			 =>  (evaluate(left, state) != evaluate(right, state))
	  }
  }
  
  private[coderover] final def evaluate(instruction:Instruction, state:State) {
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
            case Pop() 			  => state.pop()
            case Replace(expression) => {
            	val evaluated = evaluate(expression, state)
            	state.pop()
            	state.push(evaluated)
            }
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
