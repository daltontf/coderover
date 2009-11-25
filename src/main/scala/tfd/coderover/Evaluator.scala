package tfd.coderover

class Evaluator(environment:Environment, controller:Controller) {
  def this(environment:Environment) = this(environment, new Controller(environment))
  
  private val blockMap = new scala.collection.mutable.HashMap[String, List[Instruction]]()
  
  final def evaluate(instructions:List[Instruction], state:State) {
	  if (!state.stopped) {
		  instructions.foreach(evaluate(_, state))
      }
  }
  
  private[this] final def processDistance(distance:Option[Int], entity:String, state:State) = 
    if (distance == None) { 
    	state.fail(new UnknownEntity(entity))
        0
    } else {
        distance.get
    }    
  
  private[coderover] final def evaluate(expression:IntExpression, state:State):Int = {
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
      case DistanceX(entity)  	 => processDistance(environment.distanceX(entity, state), entity, state)
      case DistanceY(entity)  	 => processDistance(environment.distanceY(entity, state), entity, state)
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
        case Adjacent(entity)				 =>  environment.adjacent(entity, state)
	  }
  }
   
  private[coderover] final def evaluateString(expression:Expression, state:State):String = {
	   expression match {
        case StringConstant(value)					=> value
        case x:IntExpression						=> evaluate(x, state).toString
        case x:BooleanExpression					=> evaluate(x, state).toString
	   }  
  }
  
  private[coderover] final def evaluate(instruction:Instruction, state:State) {
      if (!state.stopped) {
        instruction match {
            case Def(name, instructions) => blockMap += name -> instructions
            case Call(name) => if (blockMap.contains(name)) {
            						evaluate(blockMap(name),state)
            				   } else {
            					   	state.fail(new UndefinedBlock(name))
            				   }
        	case Forward(expression) => 
        	    var distance = Math.abs(evaluate(expression, state))
        		while (!state.stopped && distance > 0 && environment.canMoveForward(state)) {
        				controller.moveForward(state)
        				environment.postMoveForward(state)
        				distance = distance - 1
        		}
        	case TurnRight() 	  => controller.turnRight(state)
        	case TurnLeft() 	  => controller.turnLeft(state)
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
            case Paint() => controller.paint(state)
            case Print(expressionList) => controller.print(expressionList.tail.foldLeft(evaluateString(expressionList.head, state)){ _ + evaluateString(_,state) })
        }
     }
  }
}
