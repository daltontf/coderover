package tfd.coderover

class Evaluator(environment:Environment, controller:Controller) {
  def this(environment:Environment) = this(environment, new Controller(environment, DefaultConstraints))
  
  private val blockMap = new scala.collection.mutable.HashMap[String, List[Instruction]]()
  
  final def evaluate(instructions:List[Instruction], state:State):Unit = evaluate(instructions, Array.empty[Int], state)

  final def evaluate(instructions:List[Instruction], args:Array[Int], state:State) {
	  if (!state.stopped) {
		  instructions.foreach(evaluate(_, args, state))
      }
  }
  
  private[this] final def processDistance(distance:Option[Int], entity:String, state:State) = 
    if (distance == None) { 
    	state.fail(new UnknownEntity(entity))
        0
    } else {
        distance.get
    }

  private[this] def evaluateDivideByZero(expression:IntExpression, args:Array[Int] ,state:State) = {
    val value = evaluate(expression, args, state)
    if (state.abend == None && value == 0) {
      state.fail(DivideByZero)
      1
    } else {
      value
    }
  }
  
  private[coderover] final def evaluate(expression:IntExpression, args:Array[Int], state:State):Int = {
    expression match {
      case Constant(x)           => x
      case Add(expressionList) 	 	 => expressionList.tail.foldLeft(evaluate(expressionList.head, args, state)){ _ + evaluate(_, args, state) }
      case Subtract(expressionList)  => expressionList.tail.foldLeft(evaluate(expressionList.head, args, state)){ _ - evaluate(_, args, state) }
      case Multiply(expressionList)  => expressionList.tail.foldLeft(evaluate(expressionList.head, args, state)){ _ * evaluate(_, args, state) }
      case Divide(expressionList)    => expressionList.tail.foldLeft(evaluate(expressionList.head, args, state)){ _ / evaluateDivideByZero(_, args, state) }
      case Modulus(expressionList)	 => expressionList.tail.foldLeft(evaluate(expressionList.head, args, state)){ _ % evaluateDivideByZero(_, args, state) }
      case Top() 				 => state.top
      case GridX() 				 => state.gridX
      case GridY() 				 => state.gridY
      case DeltaX() 			 => state.deltaX
      case DeltaY() 			 => state.deltaY
      case Depth()				 => state.depth
      case Abs(expr) 			 => Math.abs(evaluate(expr, args, state))
      case Max(expr1, expr2) 	 => Math.max(evaluate(expr1, args, state), evaluate(expr2, args, state))
      case Min(expr1, expr2) 	 => Math.min(evaluate(expr1, args, state), evaluate(expr2, args, state))
      case Negate(expr)			 => -evaluate(expr, args, state)
      case DistanceX(entity)  	 => processDistance(environment.distanceX(entity, state), entity, state)
      case DistanceY(entity)  	 => processDistance(environment.distanceY(entity, state), entity, state)
      case Mem(address)     => controller.mem(evaluate(address, args, state), state)
      case Param(position) => if (position > 0 && position <= args.length) {
                                args(position-1)
                              } else {
                                state.fail(UnboundParameter(position))
                                0
                              }
    }
  }
  
   private[coderover] final def evaluate(booleanExpression:BooleanExpression, args:Array[Int], state:State):Boolean = {
	  booleanExpression match {
        case Obstructed(xExpression, yExpression) => {
                                                    val x = evaluate(xExpression, args, state)
                                                    val y = evaluate(yExpression, args, state)
                                                    (x < 0) ||
                                                    (y < 0) ||
                                                    (x >= environment.sizeX) ||
                                                    (y >= environment.sizeY) ||
                                                    environment.obstructed.contains((x,y))}
        case Painted(xExpression, yExpression) =>  environment.isPainted(evaluate(xExpression, args, state), evaluate(yExpression, args, state), state)
        case Not(booleanExpression)		     =>  !(evaluate(booleanExpression, args, state))
	      case And(booleanExpressionList)		 =>  booleanExpressionList.forall{ evaluate(_, args, state) }
        case Or(booleanExpressionList) 		 =>  booleanExpressionList.exists{ evaluate(_, args, state) }
        case Equal(left, right) 			 =>  (evaluate(left, args, state) == evaluate(right, args, state))
        case LessThan(left, right) 			 =>  (evaluate(left, args, state) < evaluate(right, args, state))
        case GreaterThan(left, right) 	     =>  (evaluate(left, args, state) > evaluate(right, args, state))
        case LessThanOrEqual(left, right) 	 =>  (evaluate(left, args, state) <= evaluate(right, args, state))
        case GreaterThanOrEqual(left, right) =>  (evaluate(left, args, state) >= evaluate(right, args, state))
        case NotEqual(left, right) 			 =>  (evaluate(left, args, state) != evaluate(right, args, state))
        case Adjacent(entity)				 =>  environment.adjacent(entity, state)
	  }
  }
   
  private[coderover] final def evaluateString(expression:Expression, args:Array[Int], state:State):String = {
	   expression match {
        case StringConstant(value)					=> value
        case x:IntExpression						=> evaluate(x, args, state).toString
        case x:BooleanExpression					=> evaluate(x, args, state).toString
	   }  
  }
  
  private[coderover] final def evaluate(instruction:Instruction, args:Array[Int], state:State) {
      if (!state.stopped) {
        instruction match {
            case Def(name, instructions) => blockMap += name -> instructions
            case Call(name, callArgs) => if (blockMap.contains(name)) {
                          controller.incrementCallStack(state)
                          if (!state.stopped) {
                            val evalArgs = callArgs.map { expr:IntExpression => evaluate(expr, args, state) }
            						    evaluate(
                              blockMap(name),
                              evalArgs.toArray,
                              state)
                            controller.decrementCallStack()
                          }
            				   } else {
            					   	state.fail(new UndefinedBlock(name))
            				   }
        	case Forward(expression) => 
        	    var distance = Math.abs(evaluate(expression, args, state))
        		while (!state.stopped && distance > 0 && environment.canMoveForward(state)) {
        				controller.moveForward(state)
        				environment.postMoveForward(state)
        				distance = distance - 1
        		}
        	case TurnRight() 	  => controller.turnRight(state)
        	case TurnLeft() 	  => controller.turnLeft(state)
        	case Push(expression) => controller.push(state, evaluate(expression, args, state))
            case Pop() 			  => state.pop()
            case Replace(expression) => {
            	val evaluated = evaluate(expression, args, state)
            	state.pop()
            	state.push(evaluated)
            }
        	case If(booleanExpression, thenStatements, elseStatements) => 
        		if (evaluate(booleanExpression, args, state)) {
        			evaluate(thenStatements, args, state)
        		} else if (!elseStatements.isEmpty) {
        			evaluate(elseStatements, args, state)            
        		}
        	case While(booleanExpression, blockStatements) => 
        		while (!state.stopped && evaluate(booleanExpression, args, state)) {
        			evaluate(blockStatements, args, state)
        		}
          case Paint() => controller.paint(state)
          case Print(expressionList) => controller.print(expressionList.tail.foldLeft(evaluateString(expressionList.head, args, state)){ _ + evaluateString(_, args, state) })
          case Store(address, value) => controller.store(evaluate(address, args, state), evaluate(value, args, state), state)
        }
     }
  }
}
