package tfd.coderover

import collection.mutable.Stack

class Evaluator() {
  
  final def evaluate(instructions:List[Instruction], controller:Controller):ResultOrAbend[Unit] = {
    controller.stopped = false
    evaluate(instructions, Array.empty[Int], controller)
  }  

  private[this] final def evaluate(instructions:List[Instruction], args:Array[Int], controller:Controller):ResultOrAbend[Unit] =
    if (instructions != Nil) {
      instructions.tail.foldLeft(evaluateInstruction(instructions.head, args, controller)) {
         (previousResult, instruction) =>
            for (xp <- previousResult; yp <- evaluateInstruction(instruction, args, controller))
              yield (yp)
      }
    } else {
      SuccessResultUnit
    }

  
  private[this] final def processDistance(distance:Option[Int], entity:String):ResultOrAbend[Int] =
    if (distance == None) { 
    	new ResultOrAbend(UnknownEntity(entity))
    } else {
      new ResultOrAbend(distance.get)
    }

  private[this] def evaluateDivideByZero(expression:IntExpression, args:Array[Int], controller:Controller):ResultOrAbend[Int] = {
    val result = evaluateInt(expression, args, controller)
    if (!result.value.isEmpty && result.value.get == 0) {
      new ResultOrAbend(DivideByZero)
    } else {
      result
    }
  }

  private[coderover] val add = { (x:ResultOrAbend[Int], y:ResultOrAbend[Int]) =>
    for (xp <- x; yp <- x) yield (xp + yp)
  }

  private[coderover] final def evaluateInt(expression:IntExpression, args:Array[Int], controller:Controller):ResultOrAbend[Int] = {
    expression match {
      case Constant(x)               => new ResultOrAbend(x)
      case Add(expressionList) 	 	   => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
                                              yield (xp + yp)
                                        }
      case Subtract(expressionList)  => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
                                              yield (xp - yp) 
                                        }
      case Multiply(expressionList)  => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
                                              yield (xp * yp)
                                        }
      case Divide(expressionList)    => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, controller))
                                              yield (xp / yp)
                                        }
      case Modulus(expressionList)   => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, controller))
                                              yield (xp % yp)
                                        }
      case Top() 				             => controller.top
      case GridX() 				           => new ResultOrAbend(controller.gridX)
      case GridY() 				           => new ResultOrAbend(controller.gridY)
      case DeltaX() 			           => new ResultOrAbend(controller.deltaX)
      case DeltaY() 			           => new ResultOrAbend(controller.deltaY)
      case Depth()				           => new ResultOrAbend(controller.depth)
      case Abs(expr) 			           => for (x <- evaluateInt(expr, args, controller)) yield (Math.abs(x))
      case Max(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, controller);
                                             y <- evaluateInt(expr2, args, controller)) yield (Math.max(x,y))
      case Min(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, controller);
                                             y <- evaluateInt(expr2, args, controller)) yield (Math.min(x,y))
      case Negate(expr)			         => for (x <- evaluateInt(expr, args, controller)) yield (-x)
      case DistanceX(entity)  	     => processDistance(controller.distanceX(entity), entity)
      case DistanceY(entity)  	     => processDistance(controller.distanceY(entity), entity)
      case Mem(address)              => for (x <- evaluateInt(address, args, controller);
                                             y <- controller.mem(x)) yield (y)
      case EvalParam(position)           => if (position > 0 && position <= args.length) {
                                          new ResultOrAbend(args(position-1))
                                        } else {
                                          new ResultOrAbend(UnboundParameter(position))
                                        }
      case InvokeFunc(name, invArgs)    => if (controller.funcMap.contains(name)) {
                                        controller.incrementCallStack()
                                        val evalArgs = invArgs.map{ evaluateInt(_, args, controller) }
                                        val failedArgEval = evalArgs.find{ !_.success }
                                        val result:ResultOrAbend[Int] = if (failedArgEval.isEmpty) {
                                          evaluateInt(
                                            controller.funcMap(name),
                                            evalArgs.map {_.value.get }.toArray,
                                            controller)
                                          } else {
                                            new ResultOrAbend(failedArgEval.get.abend.get)
                                          }
                                        controller.decrementCallStack()
                                        result
                                        } else {
                                          new ResultOrAbend(UndefinedFunction(name))
                                        }
      case Ternary(booleanExpression, thenExpression, elseExpression) =>
            for (x <- evaluateBoolean(booleanExpression, args, controller);
                 res <-evaluateInt(if (x) thenExpression else elseExpression, args, controller))
              yield { res }
    }
  }
  
   private[coderover] final def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], controller:Controller):ResultOrAbend[Boolean] = {
	  booleanExpression match {
        case Obstructed(xExpression, yExpression) => for (x <- evaluateInt(xExpression, args, controller);
                                                          y <- evaluateInt(yExpression, args, controller))
                                                          yield (
                                                              (x < 0) ||
                                                              (y < 0) ||
                                                              (x >= controller.sizeX) ||
                                                              (y >= controller.sizeY) ||
                                                              controller.isObstructed(x,y) )
        case Painted(xExpression, yExpression)    => for (x <- evaluateInt(xExpression, args, controller);
                                                          y <- evaluateInt(yExpression, args, controller))
                                                          yield (controller.isPainted(x, y))
        case Not(booleanExpression)		            => for (x <- evaluateBoolean(booleanExpression, args, controller))
                                                          yield ( !x )
	      case And(booleanExpressionList)		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, controller)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateBoolean(booleanExpression, args, controller))
                                                            yield (xp && yp)
                                                     }
        case Or(booleanExpressionList) 		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, controller)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateBoolean(booleanExpression, args, controller))
                                                            yield (xp || yp)
                                                     }
        case Equal(left, right) 			            => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x == y)
        case LessThan(left, right) 			          => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x < y)
        case GreaterThan(left, right) 	          => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x > y)
        case LessThanOrEqual(left, right) 	      => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x <= y)
        case GreaterThanOrEqual(left, right)      =>  for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x >= y)
        case NotEqual(left, right) 			          =>  for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x != y)
        case Adjacent(entity)				              =>  new ResultOrAbend(controller.isAdjacent(entity))

        case InvokePred(name, invArgs)    => if (controller.predMap.contains(name)) {
                                        controller.incrementCallStack()
                                        val evalArgs = invArgs.map{ evaluateInt(_, args, controller) }
                                        val failedArgEval = evalArgs.find{ !_.success }
                                        val result:ResultOrAbend[Boolean] = if (failedArgEval.isEmpty) {
                                          evaluateBoolean(
                                            controller.predMap(name),
                                            evalArgs.map {_.value.get }.toArray,
                                            controller)
                                          } else {
                                            new ResultOrAbend(failedArgEval.get.abend.get)
                                          }
                                        controller.decrementCallStack()
                                        result
                                        } else {
                                          new ResultOrAbend(UndefinedPredicate(name))
                                        }
	  }
  }
   
  private[coderover] final def evaluateString(expression:Expression, args:Array[Int], controller:Controller):ResultOrAbend[String] = {
	   expression match {
        case StringConstant(value)					      => new ResultOrAbend(value)
        case intExpression:IntExpression          => for (x <- evaluateInt(intExpression, args, controller))
                                                        yield(x.toString)
        case booleanExpression:BooleanExpression  => for (x <- evaluateBoolean(booleanExpression, args, controller))
                                                        yield(x.toString)
	   }  
  }

  private[coderover] final def isSuccessAndTrue(booleanExpression:BooleanExpression, args:Array[Int], controller:Controller)=
    evaluateBoolean(booleanExpression, args, controller).value == Some(true)

  private[coderover] final def evaluateInstruction(instruction:Instruction, args:Array[Int], controller:Controller):ResultOrAbend[Unit] = {
      if (!controller.stopped) {
        instruction match {
            case Proc(name, instructions) => {
                                                controller.procMap += name -> instructions
                                                SuccessResultUnit
                                            }
            case InvokeProc(name, callArgs)    => if (controller.procMap.contains(name)) {
                                                controller.incrementCallStack()
                                                val evalArgs = callArgs.map{ evaluateInt(_, args, controller) }
                                                val failedArgEval = evalArgs.find{ !_.success }
                                                val result:ResultOrAbend[Unit] = if (failedArgEval.isEmpty) {
                                                  evaluate(
                                                      controller.procMap(name),
                                                      evalArgs.map {_.value.get }.toArray,
                                                      controller)
                                                } else {
                                                  new ResultOrAbend(failedArgEval.get.abend.get)
                                                }
                                                controller.decrementCallStack()
                                                result
            				                        } else {
            					   	                    new ResultOrAbend(UndefinedProcedure(name))
            				                        }
        	  case Forward()               => controller.moveForward()
        	  case TurnRight()             => {
                                              controller.turnRight()
                                              SuccessResultUnit
                                            }
        	  case TurnLeft()	             => {
                                              controller.turnLeft()
                                              SuccessResultUnit
                                            }
        	  case Push(expression)        => for (x <- evaluateInt(expression, args, controller);
                                                 result <- controller.push(x))
                                              yield (result)
            case Pop() 			             => controller.pop
            case Replace(expression)     => for (x <- evaluateInt(expression, args, controller);
            	                                   _ <- controller.pop();
                                                 result <- controller.push(x))
                                                       yield (result)
        	case If(booleanExpression, thenstatements, elsestatements) => 
        		for (x <- evaluateBoolean(booleanExpression, args, controller))
              yield {
                if (x) {
                  evaluate(thenstatements, args, controller)
        		    } else if (!elsestatements.isEmpty) {
        			    evaluate(elsestatements, args, controller)
        		    }
              }
        	case While(booleanExpression, blockstatements) => {
            var result:ResultOrAbend[Unit] = SuccessResultUnit
        		while (isSuccessAndTrue(booleanExpression, args, controller) && (result.success)) {
            		result = evaluate(blockstatements, args, controller)
        		}
            result
          }
          case Paint() => {
              controller.paint()
              SuccessResultUnit
          }
          case Print(expressionList) => for (evaluationResult <- expressionList.tail.foldLeft(evaluateString(expressionList.head, args, controller)) {
                                                        (previousResult, stringExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateString(stringExpression, args, controller))
                                                            yield (xp + yp)
                                           }) yield { controller.print(evaluationResult) }
          case Store(address, value) => for (x <- evaluateInt(address, args, controller);
                                             y <- evaluateInt(value, args, controller);
                                             result <- controller.store(x, y))
                                        yield (result)
          case Func(name, expression) => {
                                                controller.funcMap += name -> expression
                                                SuccessResultUnit
                                         }
          case Pred(name, expression) => {
                                                controller.predMap += name -> expression
                                                SuccessResultUnit
                                         }
          case Repeat(timesExpression, instructions) => {
                for (times <- evaluateInt(timesExpression, args, controller) )
                     yield (for (x <- 1 to Math.abs(times)) yield (evaluate(instructions, args, controller)))
            } 
        }
      } else {
          new ResultOrAbend(None, None)
      }
  }
}
