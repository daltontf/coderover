package tfd.coderover

sealed abstract class EvaluationResult[A](val success:Boolean) {
  def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B]

  def map[B](f: A => B): EvaluationResult[B]
}

case class EvaluationSuccess[A](val result:A) extends EvaluationResult[A](true) {
   override def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B] = f(result)

   override def map[B](f: A => B): EvaluationResult[B] = EvaluationSuccess(f(result))
}

abstract class EvaluationFailure[A]() extends EvaluationResult[A](false) {
   override def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B] = this.asInstanceOf[EvaluationResult[B]]

   override def map[B](f: A => B): EvaluationResult[B] = this.asInstanceOf[EvaluationResult[B]]
}

case class EvaluationAbend[A](val abend:Abend) extends EvaluationFailure[A]()

case class EvaluationStop[A]() extends EvaluationFailure[A]()

object EvaluationSuccessUnit extends EvaluationSuccess[Unit](())

class Evaluator(environment:Environment, controller:Controller) {
  def this(environment:Environment) = this(environment, new Controller(environment, DefaultConstraints))
  
  private val blockMap = new scala.collection.mutable.HashMap[String, List[Instruction]]()
  
  final def evaluate(instructions:List[Instruction], state:State):EvaluationResult[Unit] = evaluate(instructions, Array.empty[Int], state)

  final def evaluate(instructions:List[Instruction], args:Array[Int], state:State):EvaluationResult[Unit] =
    if (instructions != Nil) {
      instructions.tail.foldLeft(evaluateInstruction(instructions.head, args, state)) {
         (previousResult, instruction) =>
            for (xp <- previousResult; yp <- evaluateInstruction(instruction, args, state))
              yield (yp)
      }
    } else {
      EvaluationSuccessUnit
    }

  
  private[this] final def processDistance(distance:Option[Int], entity:String, state:State):EvaluationResult[Int] =
    if (distance == None) { 
    	EvaluationAbend(UnknownEntity(entity))
    } else {
      EvaluationSuccess(distance.get)
    }

  private[this] def evaluateDivideByZero(expression:IntExpression, args:Array[Int] ,state:State):EvaluationResult[Int] =
    evaluateInt(expression, args, state) match {
      case EvaluationSuccess(0) => EvaluationAbend(DivideByZero)
      case value:Any => value
    }

  private[coderover] val add = { (x:EvaluationResult[Int], y:EvaluationResult[Int]) =>
    for (xp <- x; yp <- x) yield (xp + yp)
  }

  private[coderover] final def evaluateInt(expression:IntExpression, args:Array[Int], state:State):EvaluationResult[Int] = {
    expression match {
      case Constant(x)               => EvaluationSuccess(x)
      case Add(expressionList) 	 	   => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, state)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, state))
                                              yield (xp + yp)
                                        }
      case Subtract(expressionList)  => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, state)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, state))
                                              yield (xp - yp) 
                                        }
      case Multiply(expressionList)  => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, state)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, state))
                                              yield (xp * yp)
                                        }
      case Divide(expressionList)    => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, state)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, state))
                                              yield (xp / yp)
                                        }
      case Modulus(expressionList)   => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, state)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, state))
                                              yield (xp % yp)
                                        }
      case Top() 				             => EvaluationSuccess(state.top)
      case GridX() 				           => EvaluationSuccess(state.gridX)
      case GridY() 				           => EvaluationSuccess(state.gridY)
      case DeltaX() 			           => EvaluationSuccess(state.deltaX)
      case DeltaY() 			           => EvaluationSuccess(state.deltaY)
      case Depth()				           => EvaluationSuccess(state.depth)
      case Abs(expr) 			           => for (x <- evaluateInt(expr, args, state)) yield (Math.abs(x))
      case Max(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, state);
                                             y <- evaluateInt(expr2, args, state)) yield (Math.max(x,y))
      case Min(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, state);
                                             y <- evaluateInt(expr2, args, state)) yield (Math.min(x,y))
      case Negate(expr)			         => for (x <- evaluateInt(expr, args, state)) yield (-x)
      case DistanceX(entity)  	     => processDistance(environment.distanceX(entity, state), entity, state)
      case DistanceY(entity)  	     => processDistance(environment.distanceY(entity, state), entity, state)
      case Mem(address)              => for (x <- evaluateInt(address, args, state)) yield (controller.mem(x, state))
      case Param(position)           => if (position > 0 && position <= args.length) {
                                          EvaluationSuccess(args(position-1))
                                        } else {
                                          EvaluationAbend(UnboundParameter(position))
                                        }
    }
  }
  
   private[coderover] final def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], state:State):EvaluationResult[Boolean] = {
	  booleanExpression match {
        case Obstructed(xExpression, yExpression) => for (x <- evaluateInt(xExpression, args, state);
                                                          y <- evaluateInt(yExpression, args, state))
                                                          yield (
                                                              (x < 0) ||
                                                              (y < 0) ||
                                                              (x >= environment.sizeX) ||
                                                              (y >= environment.sizeY) ||
                                                              environment.obstructed.contains((x,y)) )
        case Painted(xExpression, yExpression)    => for (x <- evaluateInt(xExpression, args, state);
                                                          y <- evaluateInt(yExpression, args, state))
                                                          yield (environment.isPainted(x, y, state))
        case Not(booleanExpression)		            => for (x <- evaluateBoolean(booleanExpression, args, state))
                                                          yield ( !x )
	      case And(booleanExpressionList)		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, state)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateBoolean(booleanExpression, args, state))
                                                            yield (xp && yp)
                                                     }
        case Or(booleanExpressionList) 		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, state)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateBoolean(booleanExpression, args, state))
                                                            yield (xp || yp)
                                                     }
        case Equal(left, right) 			            => for (x <- evaluateInt(left, args, state);
                                                           y <- evaluateInt(right, args, state))
                                                             yield (x == y)
        case LessThan(left, right) 			          => for (x <- evaluateInt(left, args, state);
                                                           y <- evaluateInt(right, args, state))
                                                             yield (x < y)
        case GreaterThan(left, right) 	          => for (x <- evaluateInt(left, args, state);
                                                           y <- evaluateInt(right, args, state))
                                                             yield (x > y)
        case LessThanOrEqual(left, right) 	      => for (x <- evaluateInt(left, args, state);
                                                           y <- evaluateInt(right, args, state))
                                                             yield (x <= y)
        case GreaterThanOrEqual(left, right)      =>  for (x <- evaluateInt(left, args, state);
                                                           y <- evaluateInt(right, args, state))
                                                             yield (x >= y)
        case NotEqual(left, right) 			          =>  for (x <- evaluateInt(left, args, state);
                                                           y <- evaluateInt(right, args, state))
                                                             yield (x != y)
        case Adjacent(entity)				              =>  EvaluationSuccess(environment.adjacent(entity, state))
	  }
  }
   
  private[coderover] final def evaluateString(expression:Expression, args:Array[Int], state:State):EvaluationResult[String] = {
	   expression match {
        case StringConstant(value)					      => EvaluationSuccess(value)
        case intExpression:IntExpression          => for (x <- evaluateInt(intExpression, args, state))
                                                        yield(x.toString)
        case booleanExpression:BooleanExpression  => for (x <- evaluateBoolean(booleanExpression, args, state))
                                                        yield(x.toString)
	   }  
  }

  private[coderover] final def isSuccessAndTrue(booleanExpression:BooleanExpression, args:Array[Int], state:State) =
    evaluateBoolean(booleanExpression, args, state) match {
      case EvaluationSuccess(true) => true
      case _:Any => false
    }

  private[coderover] final def evaluateInstruction(instruction:Instruction, args:Array[Int], state:State):EvaluationResult[Unit] = {
        instruction match {
            case Def(name, instructions) => {
                                                blockMap += name -> instructions
                                                EvaluationSuccessUnit
                                            }
            case Call(name, callArgs)    => if (blockMap.contains(name)) {
                                                controller.incrementCallStack(state)
                                                val evalArgs = callArgs.map{ evaluateInt(_, args, state) }
                                                val failedArgEval = evalArgs.find{ !_.success }
                                                val result:EvaluationResult[Unit] = if (failedArgEval.isEmpty) {
                                                  evaluate(
                                                      blockMap(name),
                                                      evalArgs.map {_.asInstanceOf[EvaluationSuccess[Int]].result }.toArray,
                                                      state)
                                                } else {
                                                  EvaluationAbend(failedArgEval.get.asInstanceOf[EvaluationAbend[Int]].abend)
                                                }
                                                controller.decrementCallStack()
                                                result
            				                        } else {
            					   	                    EvaluationAbend(new UndefinedBlock(name))
            				                        }
        	  case Forward(expression)     => for (distance <- evaluateInt(expression, args, state))
                                              yield {
        	                                      var absDistance = Math.abs(distance)
        		                                    while (!state.stopped && absDistance > 0 && environment.canMoveForward(state)) {
        				                                  controller.moveForward(state)
        				                                  environment.postMoveForward(state)
        				                                  absDistance = absDistance - 1
        		                                    }
                                            }
        	  case TurnRight()             => {
                                              controller.turnRight(state)
                                              EvaluationSuccessUnit
                                            }
        	  case TurnLeft()	             => {
                                              controller.turnLeft(state)
                                              EvaluationSuccessUnit
                                            }
        	  case Push(expression)        => for (x <- evaluateInt(expression, args, state))
                                              yield ( controller.push(state, x))
            case Pop() 			             => {
                                              state.pop()
                                              EvaluationSuccessUnit
                                            }
            case Replace(expression)     => for (x <- evaluateInt(expression, args, state))
            	                                yield {
                                                state.pop()
            	                                  state.push(x)
                                              }
        	case If(booleanExpression, thenStatements, elseStatements) => 
        		for (x <- evaluateBoolean(booleanExpression, args, state))
              yield {
                if (x) {
                  evaluate(thenStatements, args, state)
        		    } else if (!elseStatements.isEmpty) {
        			    evaluate(elseStatements, args, state)
        		    }
              }
        	case While(booleanExpression, blockStatements) => {
            var result:EvaluationResult[Unit] = EvaluationSuccessUnit
        		while (isSuccessAndTrue(booleanExpression, args, state) && (result.success)) {
            		result = evaluate(blockStatements, args, state)
        		}
            result
          }
          case Paint() => {
              controller.paint(state)
              EvaluationSuccessUnit
          }
          case Print(expressionList) => for (evaluationResult <- expressionList.tail.foldLeft(evaluateString(expressionList.head, args, state)) {
                                                        (previousResult, stringExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateString(stringExpression, args, state))
                                                            yield (xp + yp)
                                           }) yield { controller.print(evaluationResult) }
          case Store(address, value) => for (x <- evaluateInt(address, args, state);
                                             y <- evaluateInt(value, args, state))
                                                yield {  controller.store(x, y, state)  }
        }
  }
}
